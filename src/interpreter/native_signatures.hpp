#pragma once

#include <string>
#include <unordered_map>
#include <vector>
#include "../value/type.hpp"

namespace phos::native
{

// Signature for a global native function
struct Native_function_signature
{
    std::vector<std::vector<types::Type>> allowed_params;
    types::Type return_type;
};

// Signature for a native method
struct Native_method_signature
{
    std::vector<types::Type> valid_this_types;
    std::vector<std::vector<types::Type>> allowed_params;
    types::Type return_type;
};

// Returns a map of all native GLOBAL FUNCTION signatures
inline std::unordered_map<std::string, Native_function_signature> get_native_fn_signatures()
{
    std::unordered_map<std::string, Native_function_signature> signatures;

    // --- Helper Types for Readability ---
    auto any_array_type = types::Type(mem::make_rc<types::Array_type>(types::Primitive_kind::Any));
    auto string_type = types::Type(types::Primitive_kind::String);
    auto i64_type = types::Type(types::Primitive_kind::Int);

    // len(string | array) -> i64
    signatures["len"] = {.allowed_params = {{string_type, any_array_type}}, .return_type = i64_type};

    // clock() -> f64
    signatures["clock"] = {.allowed_params = {}, .return_type = types::Primitive_kind::Float};

    return signatures;
}

// Returns a map of all native METHOD signatures
inline std::unordered_map<std::string, Native_method_signature> get_native_method_signatures()
{
    std::unordered_map<std::string, Native_method_signature> signatures;

    // --- Helper Types for Readability ---
    auto any_array_type    = types::Type(mem::make_rc<types::Array_type>(types::Primitive_kind::Any));
    auto any_optional_type = types::Type(mem::make_rc<types::Optional_type>(types::Primitive_kind::Any));
    auto string_type       = types::Type(types::Primitive_kind::String);
    auto i64_type          = types::Type(types::Primitive_kind::Int);
    auto bool_type         = types::Type(types::Primitive_kind::Bool);

    // A generic placeholder for an array's element type.
    auto generic_element_type = types::Type(types::Primitive_kind::Any);

    // ===================================
    // ## Array Methods
    // ===================================
    signatures["push"] = {
        .valid_this_types = {any_array_type},
        .allowed_params   = {{generic_element_type}},
        .return_type      = types::Primitive_kind::Void
    };

    signatures["pop"] = {
        .valid_this_types = {any_array_type},
        .allowed_params   = {},
        .return_type      = generic_element_type  // Returns the element type
    };
    signatures["insert"] = {
        .valid_this_types = {any_array_type},
        .allowed_params   = {{i64_type}, {generic_element_type}},
        .return_type      = types::Primitive_kind::Void
    };

    signatures["remove"]  = {
        .valid_this_types = {any_array_type},
        .allowed_params   = {{i64_type}},
        .return_type      = generic_element_type  // Returns the removed element
    };

    signatures["clear"]    = {.valid_this_types = {any_array_type}, .allowed_params = {}, .return_type = types::Primitive_kind::Void};
    signatures["contains"] = {.valid_this_types = {any_array_type}, .allowed_params = {{generic_element_type}}, .return_type = bool_type};

    // ===================================
    // ## String & Array Methods
    // ===================================
    signatures["is_empty"] = {.valid_this_types = {string_type, any_array_type}, .allowed_params = {}, .return_type = bool_type};

    // ===================================
    // ## String Methods
    // ===================================
    signatures["to_upper"] = {.valid_this_types = {string_type}, .allowed_params = {}, .return_type = string_type};
    signatures["to_lower"] = {.valid_this_types = {string_type}, .allowed_params = {}, .return_type = string_type};
    signatures["trim"]     = {.valid_this_types = {string_type}, .allowed_params = {}, .return_type = string_type};
    signatures["split"]    = {
        .valid_this_types = {string_type},
        .allowed_params   = {{string_type}},
        .return_type      = types::Type(mem::make_rc<types::Array_type>(string_type))
    };
    signatures["replace"] = { .valid_this_types = {string_type}, .allowed_params = {{string_type}, {string_type}}, .return_type = string_type};

    // ===================================
    // ## Optional Methods
    // ===================================
    signatures["exists"] = {{any_optional_type}, {}, types::Primitive_kind::Bool};
    signatures["value"] = {{any_optional_type}, {}, types::Primitive_kind::Any}; // Returns generic T
    signatures["value_or"] = {{any_optional_type}, {{types::Primitive_kind::Any}}, types::Primitive_kind::Any}; // Param and return are generic T

    auto map_closure_type = types::Type(mem::make_rc<types::Closure_type>());
    signatures["map"] = {{any_array_type, any_optional_type}, {{map_closure_type}}, types::Primitive_kind::Any};

    return signatures;
}

}  // namespace phos::native
