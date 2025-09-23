#pragma once

#include <cstdint>
#include <string>
#include <vector>
#include <unordered_map>
#include <variant>
#include <compare>  // Required for the spaceship operator

#include "../memory/ref_counted.hpp"

namespace phos::types
{
enum class Primitive_kind : uint8_t { Int, Float, Bool, String, Void, Any, Nil };

// Modern type representation using variant
using Type = std::variant<
    Primitive_kind,
    phos::mem::rc_ptr<struct Function_type>,
    phos::mem::rc_ptr<struct Closure_type>,
    phos::mem::rc_ptr<struct Model_type>,
    phos::mem::rc_ptr<struct Array_type>,
    phos::mem::rc_ptr<struct Native_function_type>,
    phos::mem::rc_ptr<struct Optional_type>
>;

bool operator==(const Type& lhs, const Type& rhs);

struct Function_type
{
    std::vector<Type> parameter_types;
    Type return_type;

    // The compiler will generate ==, !=, <, >, <=, >= automatically.
    auto operator<=>(const Function_type &) const = default;
};

struct Closure_type
{
    Function_type function_type;
    std::unordered_map<std::string, Type> captured_variables;

    // NOTE: Cannot use <=> because std::unordered_map is not ordered.
    // We must define == manually.
    bool operator==(const Closure_type &other) const
    {
        return function_type == other.function_type && captured_variables == other.captured_variables;
    }
};

struct Model_type
{
    std::string name;
    std::unordered_map<std::string, Type> fields;
    std::unordered_map<std::string, Function_type> methods;

    // NOTE: Cannot use <=> because std::unordered_map is not ordered.
    bool operator==(const Model_type &other) const { return name == other.name && fields == other.fields && methods == other.methods; }
};

struct Native_function_type
{
    std::vector<std::vector<Type>> allowed_types;
    Type return_type;

    auto operator<=>(const Native_function_type &) const = default;
};

struct Array_type
{
    types::Type element_type;

    auto operator<=>(const Array_type &) const = default;
};

struct Optional_type
{
    Type base_type;

    auto operator<=>(const Optional_type &) const = default;
};

inline Primitive_kind get_primitive_kind(const Type &type) { return std::get<Primitive_kind>(type); }

// --- Helper Functions (Declarations) ---
// These small, inline functions can stay in the header for performance.
inline bool is_primitive(const Type &type) { return std::holds_alternative<Primitive_kind>(type); }
inline bool is_function(const Type &type)  { return std::holds_alternative<phos::mem::rc_ptr<Function_type>>(type); }
inline bool is_closure(const Type &type)   { return std::holds_alternative<phos::mem::rc_ptr<Closure_type>>(type); }
inline bool is_model(const Type &type)     { return std::holds_alternative<phos::mem::rc_ptr<Model_type>>(type); }
inline bool is_array(const Type &type)     { return std::holds_alternative<phos::mem::rc_ptr<Array_type>>(type); }

inline phos::mem::rc_ptr<Function_type> get_function_type(const Type &type) { return std::get<phos::mem::rc_ptr<Function_type>>(type); }
inline phos::mem::rc_ptr<Closure_type> get_closure_type(const Type &type)   { return std::get<phos::mem::rc_ptr<Closure_type>>(type); }
inline phos::mem::rc_ptr<Model_type> get_model_type(const Type &type)       { return std::get<phos::mem::rc_ptr<Model_type>>(type); }
inline phos::mem::rc_ptr<Array_type> get_array_type(const Type &type)       { return std::get<phos::mem::rc_ptr<Array_type>>(type); }

inline bool is_any(const Type &type)       { if (is_primitive(type)) return get_primitive_kind(type) == types::Primitive_kind::Any; else return false; }
inline bool is_nil(const Type &type) { if (is_primitive(type)) return get_primitive_kind(type) == types::Primitive_kind::Nil; else return false; }
inline bool is_optional(const Type &type) { return std::holds_alternative<phos::mem::rc_ptr<Optional_type>>(type); }

inline phos::mem::rc_ptr<Optional_type> get_optional_type(const Type &type) { return std::get<phos::mem::rc_ptr<Optional_type>>(type); }

std::string type_to_string(const Type &type);

}  // namespace phos::types
