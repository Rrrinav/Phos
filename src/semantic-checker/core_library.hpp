#pragma once

#include <chrono>
#include "../value/value.hpp"

namespace phos::vm {

struct core
{
    static phos::Value native_clock(mem::Arena&, std::span<phos::Value> args)
    {
        auto now = std::chrono::steady_clock::now().time_since_epoch();
        return phos::Value(std::chrono::duration<double>(now).count());
    }

    static phos::Value native_is_same(mem::Arena&, std::span<phos::Value> args)
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
            // primitives (numbers, bool, nil)
            return phos::Value(a == b);
        }
    }

    [[noreturn]] static phos::Value exit(mem::Arena&, std::span<phos::Value> args) {
        std::exit(args[0].as_int());
    }
};

struct numerical
{
    static Value to_string(mem::Arena& arena, std::span<phos::Value> args)
    {
        std::string str_val = args[0].to_string();
        return phos::Value::make_string(arena, str_val);
    }

    static phos::Value parse_i64(mem::Arena &arena, std::span<phos::Value> args)
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

    static phos::Value parse_f64(mem::Arena &arena, std::span<phos::Value> args)
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

}
