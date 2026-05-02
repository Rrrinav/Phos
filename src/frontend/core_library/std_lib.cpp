#include "core/value/value.hpp"

#include <chrono>
#include <iostream>
#include <utility>
#include <print>

namespace phos::vm {

struct core
{
    static phos::Value native_clock(mem::Arena &, std::span<phos::Value> args)
    {
        auto now = std::chrono::steady_clock::now().time_since_epoch();
        return phos::Value(std::chrono::duration<double>(now).count());
    }

    static phos::Value native_is_same(mem::Arena &, std::span<phos::Value> args)
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

    [[noreturn]] static phos::Value exit(mem::Arena &, std::span<phos::Value> args)
    {
        std::exit(args[0].as_int());
    }

    static phos::Value native_len(mem::Arena &arena, std::span<phos::Value> args)
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
    static Value to_string(mem::Arena &arena, std::span<phos::Value> args)
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

struct string_methods
{
    static phos::Value substr(mem::Arena &arena, std::span<phos::Value> args)
    {
        std::string_view str = args[0].as_string();

        // 1. Safely extract start index
        size_t start = 0;
        if (auto opt_u = args[1].try_as_u64(); opt_u) {
            start = static_cast<size_t>(*opt_u);
        } else if (auto opt_i = args[1].try_as_i64(); opt_i) {
            start = *opt_i < 0 ? 0 : static_cast<size_t>(*opt_i);
        }

        // 2. Safely extract length
        size_t length = 0;
        if (auto opt_u = args[2].try_as_u64(); opt_u) {
            length = static_cast<size_t>(*opt_u);
        } else if (auto opt_i = args[2].try_as_i64(); opt_i) {
            length = *opt_i < 0 ? 0 : static_cast<size_t>(*opt_i);
        }

        // 3. Strict bounds checking to prevent host VM crashes
        if (start >= str.length() || length == 0) {
            return phos::Value::make_string(arena, "");
        }

        if (start + length > str.length()) {
            length = str.length() - start;
        }

        return phos::Value::make_string(arena, str.substr(start, length));
    }

    static phos::Value starts_with(mem::Arena &arena, std::span<phos::Value> args)
    {
        return phos::Value(args[0].as_string().starts_with(args[1].as_string()));
    }

    static phos::Value ends_with(mem::Arena &arena, std::span<phos::Value> args)
    {
        return phos::Value(args[0].as_string().ends_with(args[1].as_string()));
    }

    static phos::Value trim(mem::Arena &arena, std::span<phos::Value> args)
    {
        std::string_view str = args[0].as_string();
        size_t first = str.find_first_not_of(" \t\r\n");

        if (first == std::string_view::npos) {
            return phos::Value::make_string(arena, ""); // Entirely whitespace
        }

        size_t last = str.find_last_not_of(" \t\r\n");
        std::string_view trimmed = str.substr(first, (last - first + 1));

        // Allocate the trimmed view into the Arena
        return phos::Value::make_string(arena, trimmed);
    }

    static phos::Value to_upper(mem::Arena &arena, std::span<phos::Value> args)
    {
        std::string_view str = args[0].as_string();
        std::string upper_str(str);

        for (char &c : upper_str) {
            c = static_cast<char>(std::toupper(static_cast<unsigned char>(c)));
        }

        return phos::Value::make_string(arena, upper_str);
    }

    static phos::Value to_lower(mem::Arena &arena, std::span<phos::Value> args)
    {
        std::string_view str = args[0].as_string();
        std::string lower_str(str);

        for (char &c : lower_str) {
            c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
        }

        return phos::Value::make_string(arena, lower_str);
    }

    static phos::Value split(mem::Arena &arena, std::span<phos::Value> args)
    {
        std::string_view str = args[0].as_string();
        std::string_view delim = args[1].as_string();

        // 1. Collect the split views into a temporary vector so we know the exact count
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
            // Push the final chunk
            temp_parts.push_back(str.substr(start));
        }

        // 2. Allocate the exact capacity needed in the Arena using your make_array
        uint32_t capacity = static_cast<uint32_t>(temp_parts.size());
        phos::Value arr_val = phos::Value::make_array(arena, capacity);
        auto *arr = arr_val.as_array();

        // 3. Populate the raw pointer array and set the final count
        for (uint32_t i = 0; i < capacity; ++i) {
            arr->elements[i] = phos::Value::make_string(arena, temp_parts[i]);
        }
        arr->count = capacity;

        return arr_val;
    }
    static phos::Value repeat(mem::Arena &arena, std::span<phos::Value> args)
    {
        if (args.size() < 2 || !args[0].is_string() || !args[1].is_integer()) {
            return phos::Value();
        }

        std::string_view str = args[0].as_string();
        int64_t count = args[1].as_int();

        if (count <= 0 || str.empty()) {
            return phos::Value::make_string(arena, "");
        }

        // Pre-allocate the exact memory needed to avoid reallocations
        std::string result;
        result.reserve(str.length() * static_cast<size_t>(count));

        for (int64_t i = 0; i < count; ++i) {
            result.append(str);
        }

        return phos::Value::make_string(arena, result);
    }

    static phos::Value index_of(mem::Arena &arena, std::span<phos::Value> args)
    {
        if (args.size() < 2 || !args[0].is_string() || !args[1].is_string()) {
            return phos::Value();
        }

        std::string_view haystack = args[0].as_string();
        std::string_view needle = args[1].as_string();

        size_t pos = haystack.find(needle);

        if (pos == std::string_view::npos) {
            return phos::Value(); // Returns monadic nil (Depth 0)
        }

        return phos::Value(static_cast<int64_t>(pos));
    }
};

struct array_methods
{
    static phos::Value len(mem::Arena &arena, std::span<phos::Value> args)
    {
        return phos::Value(static_cast<int64_t>(args[0].as_array()->count));
    }

    static phos::Value is_empty(mem::Arena &arena, std::span<phos::Value> args)
    {
        return phos::Value(args[0].as_array()->count == 0);
    }

    static phos::Value clear(mem::Arena &arena, std::span<phos::Value> args)
    {
        args[0].as_array()->count = 0;
        return phos::Value();
    }

    static phos::Value push(mem::Arena &arena, std::span<phos::Value> args)
    {
        auto *arr = args[0].as_array();

        // 1. Check if we need to resize
        if (arr->count >= arr->capacity) {
            uint32_t new_cap = arr->capacity == 0 ? 8 : arr->capacity * 2;
            phos::Value *new_elems = arena.allocate<phos::Value>(new_cap);

            // 2. Copy old elements to the new arena block
            if (arr->elements) {
                for (uint32_t i = 0; i < arr->count; ++i) {
                    new_elems[i] = arr->elements[i];
                }
            }

            arr->elements = new_elems;
            arr->capacity = new_cap;
        }

        // 3. Append the new value
        arr->elements[arr->count++] = args[1];
        return phos::Value();
    }

    static phos::Value pop(mem::Arena &arena, std::span<phos::Value> args)
    {
        auto *arr = args[0].as_array();
        if (arr->count == 0) {
            return phos::Value(); // Return monadic nil
        }

        // Just decrement count and return the value that is now "out of bounds"
        return arr->elements[--arr->count];
    }

    static phos::Value insert(mem::Arena &arena, std::span<phos::Value> args)
    {
        auto *arr = args[0].as_array();
        int64_t idx = args[1].as_int();

        if (idx < 0 || idx > static_cast<int64_t>(arr->count)) {
            return phos::Value(); // Out of bounds
        }

        // Resize if at capacity
        if (arr->count >= arr->capacity) {
            uint32_t new_cap = arr->capacity == 0 ? 8 : arr->capacity * 2;
            phos::Value *new_elems = arena.allocate<phos::Value>(new_cap);

            if (arr->elements) {
                for (uint32_t i = 0; i < arr->count; ++i) {
                    new_elems[i] = arr->elements[i];
                }
            }
            arr->elements = new_elems;
            arr->capacity = new_cap;
        }

        // Shift elements right to make room
        for (uint32_t i = arr->count; i > static_cast<uint32_t>(idx); --i) {
            arr->elements[i] = arr->elements[i - 1];
        }

        arr->elements[idx] = args[2];
        arr->count++;

        return phos::Value();
    }

    static phos::Value remove_at(mem::Arena &arena, std::span<phos::Value> args)
    {
        auto *arr = args[0].as_array();
        int64_t idx = args[1].as_int();

        if (idx < 0 || idx >= static_cast<int64_t>(arr->count)) {
            return phos::Value(); // Out of bounds
        }

        phos::Value removed = arr->elements[idx];

        // Shift elements left to fill the gap
        for (uint32_t i = static_cast<uint32_t>(idx); i < arr->count - 1; ++i) {
            arr->elements[i] = arr->elements[i + 1];
        }

        arr->count--;
        return removed;
    }

    static phos::Value u_remove_at(mem::Arena &arena, std::span<phos::Value> args)
    {
        auto *arr = args[0].as_array();
        int64_t idx = args[1].as_int();

        if (idx < 0 || idx >= static_cast<int64_t>(arr->count)) {
            return phos::Value(); // Out of bounds
        }
        phos::Value removed = arr->elements[idx];
        arr->elements[idx] = arr->elements[arr->count - 1];
        arr->count--;
        return removed;
    }

    static phos::Value reverse(mem::Arena &arena, std::span<phos::Value> args)
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
