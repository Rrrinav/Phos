#pragma once

#include <string>
#include <unordered_map>
#include <vector>
#include "../value/type.hpp"

namespace phos::native
{

// Defines a native function's signature, including overloads for each parameter.
struct Native_function_signature
{
    // Each inner vector represents one parameter's allowed types.
    std::vector<std::vector<types::Type>> allowed_params;
    types::Type return_type;
};

// Returns a map of all native function signatures for the type checker.
inline std::unordered_map<std::string, Native_function_signature> get_native_signatures()
{
    std::unordered_map<std::string, Native_function_signature> signatures;

    // --- Global Function Signatures ---

    // Defines len() with two overloads for its first parameter:
    // 1. len(string) -> i64
    // 2. len(any[]) -> i64
    signatures["len"] = {.allowed_params =
        {// The first (and only) parameter can be a string OR any array.
            {
                types::Type(types::Primitive_kind::String),
                types::Type(std::make_shared<types::Array_type>(types::Primitive_kind::Void))  // void[] = any array
            }},
        .return_type = types::Primitive_kind::Int};

    // clock() -> f64
    signatures["clock"] = {.allowed_params = {},  // No parameters
                           .return_type = types::Primitive_kind::Float};

    return signatures;
}

}  // namespace phos::native
