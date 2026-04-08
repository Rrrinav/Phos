#pragma once

#include "../memory/ref_counted.hpp"

#include <cstdint>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

namespace phos {
struct Enum_variants;
}

namespace phos::types {

enum class Primitive_kind : uint8_t { I8, I16, I32, I64, U8, U16, U32, U64, F16, F32, F64, Bool, String, Void, Any, Nil };

struct Function_type;
struct Model_type;
struct Array_type;
struct Optional_type;
struct Union_type;
struct Iterator_type;
struct Enum_type;

namespace consts {
inline constexpr int primitve_i = 0;
inline constexpr int function_i = 1;
inline constexpr int model_i = 2;
inline constexpr int array_i = 3;
inline constexpr int optional_i = 4;
inline constexpr int union_i = 5;
inline constexpr int iterator_i = 6;
inline constexpr int enum_i = 7;
} // namespace consts

using Type = std::variant<
    Primitive_kind,
    phos::mem::rc_ptr<Function_type>,
    phos::mem::rc_ptr<Model_type>,
    phos::mem::rc_ptr<Array_type>,
    phos::mem::rc_ptr<Optional_type>,
    phos::mem::rc_ptr<Union_type>,
    phos::mem::rc_ptr<Iterator_type>,
    phos::mem::rc_ptr<Enum_type>>;

bool operator==(const Type &lhs, const Type &rhs);
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
    bool operator==(const Union_type &other) const
    {
        return name == other.name && variants == other.variants;
    }
};

struct Iterator_type
{
    Type element_type;
    auto operator<=>(const Iterator_type &) const = default;
};

struct Enum_type
{
    std::string name;
    Type base_type;

    // 2. Opaque pointer safely holds the map without needing Value's definition here.
    phos::mem::rc_ptr<phos::Enum_variants> variants;

    bool operator==(const Enum_type &other) const
    {
        return name == other.name;
    }
};

inline Primitive_kind get_primitive_kind(const Type &type)
{
    return std::get<Primitive_kind>(type);
}

inline bool is_primitive(const Type &type)
{
    return std::holds_alternative<Primitive_kind>(type);
}
inline bool is_function(const Type &type)
{
    return std::holds_alternative<phos::mem::rc_ptr<Function_type>>(type);
}
inline bool is_model(const Type &type)
{
    return std::holds_alternative<phos::mem::rc_ptr<Model_type>>(type);
}
inline bool is_array(const Type &type)
{
    return std::holds_alternative<phos::mem::rc_ptr<Array_type>>(type);
}
inline bool is_optional(const Type &type)
{
    return std::holds_alternative<phos::mem::rc_ptr<Optional_type>>(type);
}
inline bool is_union(const Type &type)
{
    return std::holds_alternative<phos::mem::rc_ptr<Union_type>>(type);
}
inline bool is_iterator(const Type &type)
{
    return std::holds_alternative<phos::mem::rc_ptr<Iterator_type>>(type);
}
inline bool is_enum(const Type &type)
{
    return std::holds_alternative<phos::mem::rc_ptr<Enum_type>>(type);
}

inline phos::mem::rc_ptr<Function_type> get_function_type(const Type &type)
{
    return std::get<phos::mem::rc_ptr<Function_type>>(type);
}
inline phos::mem::rc_ptr<Model_type> get_model_type(const Type &type)
{
    return std::get<phos::mem::rc_ptr<Model_type>>(type);
}
inline phos::mem::rc_ptr<Array_type> get_array_type(const Type &type)
{
    return std::get<phos::mem::rc_ptr<Array_type>>(type);
}
inline phos::mem::rc_ptr<Optional_type> get_optional_type(const Type &type)
{
    return std::get<phos::mem::rc_ptr<Optional_type>>(type);
}
inline phos::mem::rc_ptr<Union_type> get_union_type(const Type &type)
{
    return std::get<phos::mem::rc_ptr<Union_type>>(type);
}
inline phos::mem::rc_ptr<Iterator_type> get_iterator_type(const Type &type)
{
    return std::get<phos::mem::rc_ptr<Iterator_type>>(type);
}
inline phos::mem::rc_ptr<Enum_type> get_enum_type(const Type &type)
{
    return std::get<phos::mem::rc_ptr<Enum_type>>(type);
}

inline bool is_any(const Type &type)
{
    return is_primitive(type) && get_primitive_kind(type) == Primitive_kind::Any;
}
inline bool is_nil(const Type &type)
{
    return is_primitive(type) && get_primitive_kind(type) == Primitive_kind::Nil;
}

inline bool is_signed_integer_primitive(Primitive_kind kind)
{
    switch (kind) {
    case Primitive_kind::I8:
    case Primitive_kind::I16:
    case Primitive_kind::I32:
    case Primitive_kind::I64:
        return true;
    default:
        return false;
    }
}

inline bool is_unsigned_integer_primitive(Primitive_kind kind)
{
    switch (kind) {
    case Primitive_kind::U8:
    case Primitive_kind::U16:
    case Primitive_kind::U32:
    case Primitive_kind::U64:
        return true;
    default:
        return false;
    }
}

inline bool is_integer_primitive(Primitive_kind kind)
{
    return is_signed_integer_primitive(kind) || is_unsigned_integer_primitive(kind);
}

inline bool is_float_primitive(Primitive_kind kind)
{
    switch (kind) {
    case Primitive_kind::F16:
    case Primitive_kind::F32:
    case Primitive_kind::F64:
        return true;
    default:
        return false;
    }
}

inline bool is_numeric_primitive(Primitive_kind kind)
{
    return is_integer_primitive(kind) || is_float_primitive(kind);
}

inline int primitive_bit_width(Primitive_kind kind)
{
    switch (kind) {
    case Primitive_kind::I8:
    case Primitive_kind::U8:
        return 8;
    case Primitive_kind::I16:
    case Primitive_kind::U16:
    case Primitive_kind::F16:
        return 16;
    case Primitive_kind::I32:
    case Primitive_kind::U32:
    case Primitive_kind::F32:
        return 32;
    case Primitive_kind::I64:
    case Primitive_kind::U64:
    case Primitive_kind::F64:
        return 64;
    default:
        return 0;
    }
}

static int get_optional_depth(types::Type t)
{
    int depth = 0;
    while (types::is_optional(t)) {
        depth++;
        t = types::get_optional_type(t)->base_type;
    }
    return depth;
}

static types::Type get_optional_base(types::Type t)
{
    while (types::is_optional(t)) {
        t = types::get_optional_type(t)->base_type;
    }
    return t;
}

} // namespace phos::types
