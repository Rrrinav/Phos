#include "core/value/value.hpp"

#include "virtual_machine/vm_context.hpp"

#include <charconv>
#include <chrono>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <utility>

namespace phos::vm {

using Ctx = vm::Vm_context;

struct env_native
{
    static Value argv_impl(Ctx &ctx, std::span<Value>)
    {
        uint32_t count = static_cast<uint32_t>(ctx.cmd_args.size());
        Value arr_val = Value::make_array(ctx, count);
        auto *arr = arr_val.as_array();
        arr->count = count;

        for (uint32_t i = 0; i < count; ++i) {
            arr->elements[i] = Value::make_string(ctx, ctx.cmd_args[i]);
        }
        return arr_val;
    }

    static Value cwd(Ctx &ctx, std::span<Value>)
    {
        std::string path = std::filesystem::current_path().string();
        return Value::make_string(ctx, path);
    }
};

struct core
{
    static phos::Value native_clock(Ctx &, std::span<phos::Value>)
    {
        auto now = std::chrono::steady_clock::now().time_since_epoch();
        return phos::Value(std::chrono::duration<double>(now).count());
    }

    static phos::Value native_is_same(Ctx &, std::span<phos::Value> args)
    {
        if (args.size() < 2) {
            return phos::Value(false);
        }

        const phos::Value &a = args[0];
        const phos::Value &b = args[1];

        if (a.tag() != b.tag()) {
            return phos::Value(false);
        }

        switch (a.tag()) {
        case phos::Value_tag::String:
            return phos::Value(a.as_string().data() == b.as_string().data());

        case phos::Value_tag::Array:
            return phos::Value(a.as_array() == b.as_array());

        case phos::Value_tag::Model:
            return phos::Value(a.as_model() == b.as_model());

        case phos::Value_tag::Union:
            return phos::Value(a.as_union() == b.as_union());

        case phos::Value_tag::Closure:
            return phos::Value(a.as_closure() == b.as_closure());

        case phos::Value_tag::Iterator:
            return phos::Value(a.as_iterator() == b.as_iterator());

        case phos::Value_tag::Green_thread:
            return phos::Value(a.as_green_thread() == b.as_green_thread());

        case phos::Value_tag::Upvalue:
            return phos::Value(a.as_upvalue() == b.as_upvalue());

        default:
            return phos::Value(a == b);
        }
    }

    [[noreturn]] static phos::Value exit(Ctx &, std::span<phos::Value> args)
    {
        std::exit(args[0].as_int());
    }

    static phos::Value native_len(Ctx &, std::span<phos::Value> args)
    {
        const phos::Value &val = args[0];

        switch (val.tag()) {
        case phos::Value_tag::String:
            return phos::Value(static_cast<int64_t>(val.as_string().length()));
        case phos::Value_tag::Array:
            return phos::Value(static_cast<int64_t>(val.as_array()->count));
        default:
            std::unreachable();
        }
    }
};

struct numerical
{
    static Value to_string(Ctx &ctx, std::span<phos::Value> args)
    {
        std::string str_val = args[0].to_string();
        return phos::Value::make_string(ctx, str_val);
    }

    static phos::Value parse_i64(Ctx &, std::span<phos::Value> args)
    {
        std::string_view str = args[0].as_string();

        size_t start = str.find_first_not_of(" \t\r\n");
        if (start == std::string_view::npos) {
            return phos::Value();
        }
        str = str.substr(start);

        int64_t result = 0;
        auto [ptr, ec] = std::from_chars(str.data(), str.data() + str.size(), result);

        if (ec == std::errc()) {
            return phos::Value(result);
        }

        return phos::Value();
    }

    static phos::Value parse_f64(Ctx &, std::span<phos::Value> args)
    {
        std::string_view str = args[0].as_string();

        size_t start = str.find_first_not_of(" \t\r\n");
        if (start == std::string_view::npos) {
            return phos::Value();
        }
        str = str.substr(start);

        double result = 0.0;
        auto [ptr, ec] = std::from_chars(str.data(), str.data() + str.size(), result);

        if (ec == std::errc()) {
            return phos::Value(result);
        }

        return phos::Value();
    }
};

struct string_methods
{
    static phos::Value substr(Ctx &ctx, std::span<phos::Value> args)
    {
        std::string_view str = args[0].as_string();

        size_t start = 0;
        if (auto opt_u = args[1].try_as_u64(); opt_u) {
            start = static_cast<size_t>(*opt_u);
        } else if (auto opt_i = args[1].try_as_i64(); opt_i) {
            start = *opt_i < 0 ? 0 : static_cast<size_t>(*opt_i);
        }

        size_t length = 0;
        if (auto opt_u = args[2].try_as_u64(); opt_u) {
            length = static_cast<size_t>(*opt_u);
        } else if (auto opt_i = args[2].try_as_i64(); opt_i) {
            length = *opt_i < 0 ? 0 : static_cast<size_t>(*opt_i);
        }

        if (start >= str.length() || length == 0) {
            return phos::Value::make_string(ctx, "");
        }

        if (start + length > str.length()) {
            length = str.length() - start;
        }

        return phos::Value::make_string(ctx, str.substr(start, length));
    }

    static phos::Value starts_with(Ctx &, std::span<phos::Value> args)
    {
        return phos::Value(args[0].as_string().starts_with(args[1].as_string()));
    }

    static phos::Value ends_with(Ctx &, std::span<phos::Value> args)
    {
        return phos::Value(args[0].as_string().ends_with(args[1].as_string()));
    }

    static phos::Value trim(Ctx &ctx, std::span<phos::Value> args)
    {
        std::string_view str = args[0].as_string();
        size_t first = str.find_first_not_of(" \t\r\n");

        if (first == std::string_view::npos) {
            return phos::Value::make_string(ctx, "");
        }

        size_t last = str.find_last_not_of(" \t\r\n");
        std::string_view trimmed = str.substr(first, (last - first + 1));

        return phos::Value::make_string(ctx, trimmed);
    }

    static phos::Value to_upper(Ctx &ctx, std::span<phos::Value> args)
    {
        std::string_view str = args[0].as_string();
        std::string upper_str(str);

        for (char &c : upper_str) {
            c = static_cast<char>(std::toupper(static_cast<unsigned char>(c)));
        }

        return phos::Value::make_string(ctx, upper_str);
    }

    static phos::Value to_lower(Ctx &ctx, std::span<phos::Value> args)
    {
        std::string_view str = args[0].as_string();
        std::string lower_str(str);

        for (char &c : lower_str) {
            c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
        }

        return phos::Value::make_string(ctx, lower_str);
    }

    static phos::Value split(Ctx &ctx, std::span<phos::Value> args)
    {
        std::string_view str = args[0].as_string();
        std::string_view delim = args[1].as_string();

        std::vector<std::string_view> temp_parts;

        if (delim.empty()) {
            for (size_t i = 0; i < str.length(); ++i) {
                temp_parts.push_back(str.substr(i, 1));
            }
        } else {
            size_t start = 0;
            size_t end;
            while ((end = str.find(delim, start)) != std::string_view::npos) {
                temp_parts.push_back(str.substr(start, end - start));
                start = end + delim.length();
            }
            temp_parts.push_back(str.substr(start));
        }

        uint32_t capacity = static_cast<uint32_t>(temp_parts.size());
        phos::Value arr_val = phos::Value::make_array(ctx, capacity);
        auto *arr = arr_val.as_array();

        auto guard = ctx.protect(&arr_val);

        for (uint32_t i = 0; i < capacity; ++i) {
            arr->elements[i] = phos::Value::make_string(ctx, temp_parts[i]);
            arr->count = i + 1;
        }

        return arr_val;
    }

    static phos::Value repeat(Ctx &ctx, std::span<phos::Value> args)
    {
        if (args.size() < 2 || !args[0].is_string() || !args[1].is_integer()) {
            return phos::Value();
        }

        std::string_view str = args[0].as_string();
        int64_t count = args[1].as_int();

        if (count <= 0 || str.empty()) {
            return phos::Value::make_string(ctx, "");
        }

        std::string result;
        result.reserve(str.length() * static_cast<size_t>(count));

        for (int64_t i = 0; i < count; ++i) {
            result.append(str);
        }

        return phos::Value::make_string(ctx, result);
    }

    static phos::Value index_of(Ctx &, std::span<phos::Value> args)
    {
        if (args.size() < 2 || !args[0].is_string() || !args[1].is_string()) {
            return phos::Value();
        }

        std::string_view haystack = args[0].as_string();
        std::string_view needle = args[1].as_string();

        size_t pos = haystack.find(needle);

        if (pos == std::string_view::npos) {
            return phos::Value(); 
        }

        return phos::Value(static_cast<int64_t>(pos));
    }
};

struct array_methods
{
    static phos::Value len(Ctx &, std::span<phos::Value> args)
    {
        return phos::Value(static_cast<int64_t>(args[0].as_array()->count));
    }

    static phos::Value is_empty(Ctx &, std::span<phos::Value> args)
    {
        return phos::Value(args[0].as_array()->count == 0);
    }

    static phos::Value clear(Ctx &, std::span<phos::Value> args)
    {
        args[0].as_array()->count = 0;
        return phos::Value();
    }

    static phos::Value push(Ctx &ctx, std::span<phos::Value> args)
    {
        auto *arr = args[0].as_array();

        if (arr->count >= arr->capacity) {
            uint32_t new_cap = arr->capacity == 0 ? 8 : arr->capacity * 2;
            phos::Value *new_elems = nullptr;

            if (arr->elements_on_heap) {
                new_elems = static_cast<phos::Value *>(std::realloc(arr->elements, sizeof(phos::Value) * new_cap));
                if (!new_elems) throw std::bad_alloc{};
                ctx.add_external_bytes((new_cap - arr->capacity) * sizeof(phos::Value));
            } else {
                new_elems = static_cast<phos::Value *>(std::malloc(sizeof(phos::Value) * new_cap));
                if (!new_elems) throw std::bad_alloc{};

                if (arr->elements && arr->count > 0) {
                    std::memcpy(new_elems, arr->elements, arr->count * sizeof(phos::Value));
                }

                ctx.add_external_bytes(new_cap * sizeof(phos::Value));
                arr->elements_on_heap = true;
            }

            std::uninitialized_fill_n(new_elems + arr->capacity, new_cap - arr->capacity, phos::Value());
            arr->elements = new_elems;
            arr->capacity = new_cap;
        }

        arr->elements[arr->count++] = args[1];
        return phos::Value();
    }

    static phos::Value pop(Ctx &, std::span<phos::Value> args)
    {
        auto *arr = args[0].as_array();
        if (arr->count == 0) {
            return phos::Value();
        }

        return arr->elements[--arr->count];
    }

    static phos::Value insert(Ctx &ctx, std::span<phos::Value> args)
    {
        auto *arr = args[0].as_array();
        int64_t idx = args[1].as_int();

        if (idx < 0 || idx > static_cast<int64_t>(arr->count)) {
            return phos::Value(); 
        }

        if (arr->count >= arr->capacity) {
            uint32_t new_cap = arr->capacity == 0 ? 8 : arr->capacity * 2;
            phos::Value *new_elems = nullptr;

            if (arr->elements_on_heap) {
                new_elems = static_cast<phos::Value *>(std::realloc(arr->elements, sizeof(phos::Value) * new_cap));
                if (!new_elems) throw std::bad_alloc{};
                ctx.add_external_bytes((new_cap - arr->capacity) * sizeof(phos::Value));
            } else {
                new_elems = static_cast<phos::Value *>(std::malloc(sizeof(phos::Value) * new_cap));
                if (!new_elems) throw std::bad_alloc{};

                if (arr->elements && arr->count > 0) {
                    std::memcpy(new_elems, arr->elements, arr->count * sizeof(phos::Value));
                }

                ctx.add_external_bytes(new_cap * sizeof(phos::Value));
                arr->elements_on_heap = true;
            }

            std::uninitialized_fill_n(new_elems + arr->capacity, new_cap - arr->capacity, phos::Value());
            arr->elements = new_elems;
            arr->capacity = new_cap;
        }

        for (uint32_t i = arr->count; i > static_cast<uint32_t>(idx); --i) {
            arr->elements[i] = arr->elements[i - 1];
        }

        arr->elements[idx] = args[2];
        arr->count++;

        return phos::Value();
    }

    static phos::Value remove_at(Ctx &, std::span<phos::Value> args)
    {
        auto *arr = args[0].as_array();
        int64_t idx = args[1].as_int();

        if (idx < 0 || idx >= static_cast<int64_t>(arr->count)) {
            return phos::Value(); 
        }

        phos::Value removed = arr->elements[idx];

        for (uint32_t i = static_cast<uint32_t>(idx); i < arr->count - 1; ++i) {
            arr->elements[i] = arr->elements[i + 1];
        }

        arr->count--;
        return removed;
    }

    static phos::Value u_remove_at(Ctx &, std::span<phos::Value> args)
    {
        auto *arr = args[0].as_array();
        int64_t idx = args[1].as_int();

        if (idx < 0 || idx >= static_cast<int64_t>(arr->count)) {
            return phos::Value(); 
        }
        phos::Value removed = arr->elements[idx];
        arr->elements[idx] = arr->elements[arr->count - 1];
        arr->count--;
        return removed;
    }

    static phos::Value reverse(Ctx &, std::span<phos::Value> args)
    {
        auto *arr = args[0].as_array();

        if (arr->count > 1) {
            for (uint32_t i = 0; i < arr->count / 2; ++i) {
                std::swap(arr->elements[i], arr->elements[arr->count - 1 - i]);
            }
        }

        return phos::Value();
    }
};

} // namespace phos::vm


namespace phos {
} // namespace phos
