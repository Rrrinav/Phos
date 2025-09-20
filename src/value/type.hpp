#pragma once

#include <cstdint>
#include <string>
#include <vector>
#include <unordered_map>
#include <memory>
#include <variant>
#include <compare>  // Required for the spaceship operator

namespace phos::types
{
enum class Primitive_kind : uint8_t { Int, Float, Bool, String, Void, Any, Nil };

// Forward declarations
struct Function_type;
struct Closure_type;
struct Model_type;
struct Array_type;
struct Native_function_type;
struct Optional_type;

// Modern type representation using variant
using Type = std::variant<
    Primitive_kind,
    std::shared_ptr<Function_type>,
    std::shared_ptr<Closure_type>,
    std::shared_ptr<Model_type>,
    std::shared_ptr<Array_type>,
    std::shared_ptr<Native_function_type>,
    std::shared_ptr<Optional_type>
>;

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

// --- Helper Functions (Declarations) ---
// These small, inline functions can stay in the header for performance.
inline bool is_primitive(const Type &type) { return std::holds_alternative<Primitive_kind>(type); }
inline Primitive_kind get_primitive_kind(const Type &type) { return std::get<Primitive_kind>(type); }
inline bool is_function(const Type &type) { return std::holds_alternative<std::shared_ptr<Function_type>>(type); }
inline std::shared_ptr<Function_type> get_function_type(const Type &type) { return std::get<std::shared_ptr<Function_type>>(type); }
inline bool is_closure(const Type &type) { return std::holds_alternative<std::shared_ptr<Closure_type>>(type); }
inline std::shared_ptr<Closure_type> get_closure_type(const Type &type) { return std::get<std::shared_ptr<Closure_type>>(type); }
inline bool is_model(const Type &type) { return std::holds_alternative<std::shared_ptr<Model_type>>(type); }
inline std::shared_ptr<Model_type> get_model_type(const Type &type) { return std::get<std::shared_ptr<Model_type>>(type); }
inline bool is_array(const Type &type) { return std::holds_alternative<std::shared_ptr<Array_type>>(type); }
inline std::shared_ptr<Array_type> get_array_type(const Type &type) { return std::get<std::shared_ptr<Array_type>>(type); }
inline bool is_any(const Type &type)
{
    if (is_primitive(type))
        return get_primitive_kind(type) == types::Primitive_kind::Any;
    else
        return false;
}
inline bool is_nil(const Type &type)
{
    if (is_primitive(type))
        return get_primitive_kind(type) == types::Primitive_kind::Nil;
    else
        return false;
}
inline bool is_optional(const Type &type) { return std::holds_alternative<std::shared_ptr<Optional_type>>(type); }
inline std::shared_ptr<Optional_type> get_optional_type(const Type &type) { return std::get<std::shared_ptr<Optional_type>>(type); }

// The declaration for our main conversion function. Its body will be in the .cpp file.
std::string type_to_string(const Type &type);

}  // namespace phos::types
