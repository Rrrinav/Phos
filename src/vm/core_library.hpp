#pragma once

#include <chrono>
#include <cmath>

#include "virtual_machine.hpp"
#include "../type-checker/type-checker.hpp"

namespace phos::vm::core
{

inline double clock_now()
{
    using namespace std::chrono;
    return duration_cast<duration<double>>(system_clock::now().time_since_epoch()).count();
}

// --- Queries (Functions) ---
inline int64_t core_len(Value val)
{
    if (is_array(val))
        return static_cast<int64_t>(get_array(val)->elements.size());
    if (is_string(val))
        return static_cast<int64_t>(get_string(val).length());
    return 0;  // Fallback for unsupported types
}

// --- Mutations (Methods) ---
inline Value array_push(mem::rc_ptr<Array_value> arr, Value val)
{
    arr->elements.push_back(val);
    return val;
}

inline Value array_pop(mem::rc_ptr<Array_value> arr)
{
    if (arr->elements.empty())
        return nullptr;
    Value last = std::move(arr->elements.back());
    arr->elements.pop_back();
    return last;
}

inline double math_sqrt(double x) { return std::sqrt(x); }
inline double math_pow(double base, double exp) { return std::pow(base, exp); }
inline double math_abs(double x) { return std::abs(x); }

inline void register_core_library(Virtual_machine &vm, Type_checker &tc)
{
    // Clock
    vm.bind_native<clock_now>("clock", {}, "f64", tc);

    // len() strictly accepts either ONE string OR ONE array
    // (We use two bind_native calls to register two valid signatures for the same function!)
    vm.bind_native<core_len>("len", {"string"}, "i64", tc);
    vm.bind_native<core_len>("len", {"any[]"}, "i64", tc);

    // Array.push() takes an array of T, and a value of T. Returns T.
    vm.bind_native<array_push>("Array::push", {"T[]", "T"}, "T", tc);

    // Array.pop() takes an array of T, and returns an optional T?
    vm.bind_native<array_pop>("Array::pop", {"T[]"}, "T?", tc);

    // Math
    vm.bind_native<math_sqrt>("sqrt", {"f64"}, "f64", tc);
    vm.bind_native<math_pow>("pow", {"f64", "f64"}, "f64", tc);
    vm.bind_native<math_abs>("abs", {"f64"}, "f64", tc);
}

}  // namespace phos::vm::core
