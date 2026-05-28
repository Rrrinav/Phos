#pragma once

#include "core/value/ffi_support.hpp"
#include "frontend/environment/type_environment.hpp"

#include <filesystem>
#include <fstream>
#include <iostream>
#include <memory>
#include <span>
#include <sstream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace phos::vm::modules {

using Ctx = vm::Vm_context;

inline types::Model_type io_error_sig{};
inline types::Model_type io_in_stream_sig{};

struct Io_stream_state
{
    std::unique_ptr<std::ifstream> owned_file;
    std::istream *input = nullptr;
    bool closed = false;
    bool is_stdin = false;
};

inline std::unordered_map<int64_t, Io_stream_state> io_streams{};
inline int64_t next_io_stream_id = 1;

namespace detail {

inline types::Type_id register_native_union(
    Type_environment &env, const std::string &name, const std::vector<std::pair<std::string, types::Type_id>> &variants)
{
    env.register_union(name);
    types::Type_id union_type = env.tt.union_(name, variants);
    env.global_types[name] = union_type;
    return union_type;
}

inline String_data *intern_runtime_name(Ctx &ctx, std::string_view text)
{
    return Value::make_string(ctx.scratch_arena(), text).as_string_data();
}

inline Value make_error(Ctx &ctx, std::string_view kind, std::string_view message, std::string_view path = "")
{
    std::vector<Value> fields = {
        Value::make_string(ctx.scratch_arena(), kind),
        Value::make_string(ctx.scratch_arena(), message),
        Value::make_string(ctx.scratch_arena(), path),
    };
    return ffi::make_model(ctx, io_error_sig, fields);
}

inline Value make_stream(Ctx &ctx, int64_t handle_id, std::string_view source, bool is_stdin)
{
    std::vector<Value> fields = {
        Value(handle_id),
        Value::make_string(ctx.scratch_arena(), source),
        Value(is_stdin),
        Value(false),
    };
    return ffi::make_model(ctx, io_in_stream_sig, fields);
}

inline Value make_union_value(Ctx &ctx, std::string_view union_name, std::string_view variant_name, Value payload = Value())
{
    auto payload_guard = ctx.protect(&payload);
    return Value::make_union(ctx, intern_runtime_name(ctx, union_name), intern_runtime_name(ctx, variant_name), payload);
}

inline Value make_status_ok(Ctx &ctx)
{
    return make_union_value(ctx, "io::Status", "Ok");
}

inline Value make_status_error(Ctx &ctx, Value error)
{
    return make_union_value(ctx, "io::Status", "Error", error);
}

inline Value make_open_ok(Ctx &ctx, Value stream)
{
    return make_union_value(ctx, "io::Open_result", "Ok", stream);
}

inline Value make_open_error(Ctx &ctx, Value error)
{
    return make_union_value(ctx, "io::Open_result", "Error", error);
}

inline Value make_read_line_ok(Ctx &ctx, Value line)
{
    return make_union_value(ctx, "io::Read_line_result", "Ok", line);
}

inline Value make_read_line_eof(Ctx &ctx)
{
    return make_union_value(ctx, "io::Read_line_result", "Eof");
}

inline Value make_read_line_error(Ctx &ctx, Value error)
{
    return make_union_value(ctx, "io::Read_line_result", "Error", error);
}

inline Value make_read_text_ok(Ctx &ctx, Value text)
{
    return make_union_value(ctx, "io::Read_text_result", "Ok", text);
}

inline Value make_read_text_error(Ctx &ctx, Value error)
{
    return make_union_value(ctx, "io::Read_text_result", "Error", error);
}

inline int64_t stream_handle(const Value &stream)
{
    return ffi::require_model_field(stream, io_in_stream_sig, "handle_id").as_int();
}

inline Value *stream_closed_field(Value &stream)
{
    return ffi::find_model_field_mut(stream, io_in_stream_sig, "closed");
}

inline void mark_closed(Value &stream)
{
    if (Value *closed = stream_closed_field(stream)) {
        *closed = Value(true);
    }
}

inline bool is_closed_model(Value &stream)
{
    if (const Value *closed = ffi::find_model_field(stream, io_in_stream_sig, "closed")) {
        return closed->as_bool();
    }
    return true;
}

inline std::string stream_source(Value &stream)
{
    if (const Value *source = ffi::find_model_field(stream, io_in_stream_sig, "source")) {
        return std::string(source->as_string());
    }
    return "";
}

inline Io_stream_state *lookup_stream(Value &stream)
{
    auto it = io_streams.find(stream_handle(stream));
    if (it == io_streams.end()) {
        return nullptr;
    }
    return &it->second;
}

inline Value invalid_state_error(Ctx &ctx, Value &stream, std::string_view message)
{
    return make_error(ctx, "invalid_state", message, stream_source(stream));
}

} // namespace detail

struct io_native
{
    static Value open(Ctx &ctx, std::span<Value> args)
    {
        std::string path(args[0].as_string());

        auto file = std::make_unique<std::ifstream>(path);
        if (!file->is_open()) {
            std::string_view kind = std::filesystem::exists(path) ? "open_failed" : "not_found";
            Value error = detail::make_error(ctx, kind, "Could not open input stream.", path);
            return detail::make_open_error(ctx, error);
        }

        const int64_t handle_id = next_io_stream_id++;

        Io_stream_state state{};
        state.owned_file = std::move(file);
        state.input = state.owned_file.get();
        state.closed = false;
        state.is_stdin = false;
        io_streams.emplace(handle_id, std::move(state));

        Value stream = detail::make_stream(ctx, handle_id, path, false);
        return detail::make_open_ok(ctx, stream);
    }

    static Value stdin_stream(Ctx &ctx, std::span<Value>)
    {
        const int64_t handle_id = next_io_stream_id++;

        Io_stream_state state{};
        state.input = &std::cin;
        state.closed = false;
        state.is_stdin = true;
        io_streams.emplace(handle_id, std::move(state));

        return detail::make_stream(ctx, handle_id, "<stdin>", true);
    }

    static Value read_line(Ctx &ctx, std::span<Value> args)
    {
        Value &stream = args[0];
        if (detail::is_closed_model(stream)) {
            Value error = detail::invalid_state_error(ctx, stream, "Cannot read from a closed stream.");
            return detail::make_read_line_error(ctx, error);
        }

        Io_stream_state *state = detail::lookup_stream(stream);
        if (state == nullptr || state->closed || state->input == nullptr) {
            detail::mark_closed(stream);
            Value error = detail::invalid_state_error(ctx, stream, "Stream handle is no longer available.");
            return detail::make_read_line_error(ctx, error);
        }

        std::string line;
        if (std::getline(*state->input, line)) {
            Value text = Value::make_string(ctx, line);
            return detail::make_read_line_ok(ctx, text);
        }

        if (state->input->eof()) {
            return detail::make_read_line_eof(ctx);
        }

        Value error = detail::make_error(ctx, "read_failed", "Failed to read a line from the stream.", detail::stream_source(stream));
        return detail::make_read_line_error(ctx, error);
    }

    static Value read_all(Ctx &ctx, std::span<Value> args)
    {
        Value &stream = args[0];
        if (detail::is_closed_model(stream)) {
            Value error = detail::invalid_state_error(ctx, stream, "Cannot read from a closed stream.");
            return detail::make_read_text_error(ctx, error);
        }

        Io_stream_state *state = detail::lookup_stream(stream);
        if (state == nullptr || state->closed || state->input == nullptr) {
            detail::mark_closed(stream);
            Value error = detail::invalid_state_error(ctx, stream, "Stream handle is no longer available.");
            return detail::make_read_text_error(ctx, error);
        }

        std::ostringstream buffer;
        buffer << state->input->rdbuf();

        if (state->input->bad()) {
            Value error = detail::make_error(ctx, "read_failed", "Failed to read the remaining stream contents.", detail::stream_source(stream));
            return detail::make_read_text_error(ctx, error);
        }

        Value text = Value::make_string(ctx, buffer.str());
        return detail::make_read_text_ok(ctx, text);
    }

    static Value close(Ctx &ctx, std::span<Value> args)
    {
        Value &stream = args[0];
        if (detail::is_closed_model(stream)) {
            return detail::make_status_ok(ctx);
        }

        Io_stream_state *state = detail::lookup_stream(stream);
        if (state == nullptr) {
            detail::mark_closed(stream);
            return detail::make_status_ok(ctx);
        }

        if (state->owned_file && state->owned_file->is_open()) {
            // Reaching EOF sets fail bits on the stream; clear them before closing so
            // we only treat an actually open file that refuses to close as an error.
            state->owned_file->clear();
            state->owned_file->close();
            if (state->owned_file->is_open()) {
                Value error = detail::make_error(ctx, "close_failed", "Failed to close the stream cleanly.", detail::stream_source(stream));
                return detail::make_status_error(ctx, error);
            }
        }

        state->closed = true;
        state->input = nullptr;
        detail::mark_closed(stream);
        io_streams.erase(detail::stream_handle(stream));
        return detail::make_status_ok(ctx);
    }

    static Value is_open(Ctx &, std::span<Value> args)
    {
        Value &stream = args[0];
        if (detail::is_closed_model(stream)) {
            return Value(false);
        }

        Io_stream_state *state = detail::lookup_stream(stream);
        return Value(state != nullptr && !state->closed && state->input != nullptr);
    }
};

inline void register_io(Type_environment &env)
{
    auto error_type = env.define_native_model(
        "io::Error",
        {
            {"kind", env.tt.get_string()},
            {"message", env.tt.get_string()},
            {"path", env.tt.get_string()},
        });
    io_error_sig = env.tt.get(error_type).as<types::Model_type>();

    auto in_stream_type = env.define_native_model(
        "io::In_stream",
        {
            {"handle_id", env.tt.get_i64()},
            {"source", env.tt.get_string()},
            {"is_stdin", env.tt.get_bool()},
            {"closed", env.tt.get_bool()},
        });
    io_in_stream_sig = env.tt.get(in_stream_type).as<types::Model_type>();

    detail::register_native_union(
        env,
        "io::Status",
        {
            {"Ok", env.tt.get_void()},
            {"Error", error_type},
        });

    detail::register_native_union(
        env,
        "io::Open_result",
        {
            {"Ok", in_stream_type},
            {"Error", error_type},
        });

    detail::register_native_union(
        env,
        "io::Read_line_result",
        {
            {"Ok", env.tt.get_string()},
            {"Eof", env.tt.get_void()},
            {"Error", error_type},
        });

    detail::register_native_union(
        env,
        "io::Read_text_result",
        {
            {"Ok", env.tt.get_string()},
            {"Error", error_type},
        });

    env.define_native(
        "io::open",
        std::vector<Native_param>{Native_param{.name = "path", .type_str = "string", .default_value = std::nullopt}},
        "io::Open_result",
        io_native::open);

    env.define_native("io::stdin", std::vector<Native_param>{}, "io::In_stream", io_native::stdin_stream);

    env.define_native(
        "io::In_stream::read_line",
        std::vector<Native_param>{Native_param{.name = "this", .type_str = "io::In_stream", .default_value = std::nullopt}},
        "io::Read_line_result",
        io_native::read_line);

    env.define_native(
        "io::In_stream::read_all",
        std::vector<Native_param>{Native_param{.name = "this", .type_str = "io::In_stream", .default_value = std::nullopt}},
        "io::Read_text_result",
        io_native::read_all);

    env.define_native(
        "io::In_stream::close",
        std::vector<Native_param>{Native_param{.name = "this", .type_str = "io::In_stream", .default_value = std::nullopt}},
        "io::Status",
        io_native::close);

    env.define_native(
        "io::In_stream::is_open",
        std::vector<Native_param>{Native_param{.name = "this", .type_str = "io::In_stream", .default_value = std::nullopt}},
        "bool",
        io_native::is_open);
}

} // namespace phos::vm::modules
