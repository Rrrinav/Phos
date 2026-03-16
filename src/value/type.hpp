#pragma once

#include <cstdint>
#include <string>
#include <vector>
#include <unordered_map>
#include <variant>

#include "../memory/ref_counted.hpp"

namespace phos::types
{

enum class Primitive_kind : uint8_t { Int, Float, Bool, String, Void, Any, Nil };

struct Function_type;
struct Model_type;
struct Array_type;
struct Optional_type;
struct Union_type;

// Dramatically simplified: A single Function_type covers all callables.
using Type = std::variant<
    Primitive_kind,
    phos::mem::rc_ptr<Function_type>,
    phos::mem::rc_ptr<Model_type>,
    phos::mem::rc_ptr<Array_type>,
    phos::mem::rc_ptr<Optional_type>,
    phos::mem::rc_ptr<Union_type>
>;

bool operator==(const Type& lhs, const Type& rhs);
std::string type_to_string(const Type &type);

struct Function_type
{
    std::vector<Type> parameter_types;
    Type return_type;
    auto operator<=>(const Function_type &) const = default;
};

struct Model_type
{
    std::string name;
    std::vector<std::pair<std::string, Type>> fields;
    std::unordered_map<std::string, size_t> field_indices;

    std::vector<std::pair<std::string, Function_type>> methods;
    std::unordered_map<std::string, size_t> method_indices;

    std::vector<std::pair<std::string, Function_type>> static_methods;
    std::unordered_map<std::string, size_t> static_method_indices;

    bool operator==(const Model_type &other) const
    {
        return name == other.name && fields == other.fields;
    }
};

struct Array_type
{
    Type element_type;
    auto operator<=>(const Array_type &) const = default;
};

struct Optional_type
{
    Type base_type;
    auto operator<=>(const Optional_type &) const = default;
};

struct Union_type
{
    std::string name;
    std::vector<std::pair<std::string, Type>> variants;
    bool operator==(const Union_type &other) const { return name == other.name && variants == other.variants; }
};

// --- Fast Inline Helpers ---
inline Primitive_kind get_primitive_kind(const Type &type) { return std::get<Primitive_kind>(type); }

inline bool is_primitive(const Type &type) { return std::holds_alternative<Primitive_kind>(type); }
inline bool is_function(const Type &type)  { return std::holds_alternative<phos::mem::rc_ptr<Function_type>>(type); }
inline bool is_model(const Type &type)     { return std::holds_alternative<phos::mem::rc_ptr<Model_type>>(type); }
inline bool is_array(const Type &type)     { return std::holds_alternative<phos::mem::rc_ptr<Array_type>>(type); }
inline bool is_optional(const Type &type)  { return std::holds_alternative<phos::mem::rc_ptr<Optional_type>>(type); }
inline bool is_union(const Type &type)     { return std::holds_alternative<phos::mem::rc_ptr<Union_type>>(type); }

inline phos::mem::rc_ptr<Function_type> get_function_type(const Type &type) { return std::get<phos::mem::rc_ptr<Function_type>>(type); }
inline phos::mem::rc_ptr<Model_type> get_model_type(const Type &type)       { return std::get<phos::mem::rc_ptr<Model_type>>(type); }
inline phos::mem::rc_ptr<Array_type> get_array_type(const Type &type)       { return std::get<phos::mem::rc_ptr<Array_type>>(type); }
inline phos::mem::rc_ptr<Optional_type> get_optional_type(const Type &type) { return std::get<phos::mem::rc_ptr<Optional_type>>(type); }
inline phos::mem::rc_ptr<Union_type> get_union_type(const Type &type)       { return std::get<phos::mem::rc_ptr<Union_type>>(type); }

inline bool is_any(const Type &type) { return is_primitive(type) && get_primitive_kind(type) == Primitive_kind::Any; }
inline bool is_nil(const Type &type) { return is_primitive(type) && get_primitive_kind(type) == Primitive_kind::Nil; }

}  // namespace phos::types
