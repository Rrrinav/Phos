#pragma once

#include <cstdint>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>
#include <optional>

namespace phos::types {

struct Type_id
{
    uint32_t value{};

    constexpr bool operator==(const Type_id &o) const noexcept
    {
        return value == o.value;
    }
    constexpr bool operator!=(const Type_id &o) const noexcept
    {
        return value != o.value;
    }
    constexpr explicit operator uint32_t() const noexcept
    {
        return value;
    }
    constexpr explicit operator int() const noexcept
    {
        return value;
    }

    constexpr bool is_null() const noexcept
    {
        return value == 0xFFFFFFFF;
    }
    static constexpr Type_id null() noexcept
    {
        return {0xFFFFFFFF};
    }
};

struct Type_id_hash
{
    size_t operator()(Type_id t) const noexcept
    {
        return std::hash<uint32_t>{}(t.value);
    }
};

// Types 'any' and 'unknown' are strictly for internal usage.
// Using them inside code is not allowed.
enum class Primitive_kind : uint8_t { I8, I16, I32, I64, U8, U16, U32, U64, F16, F32, F64, Bool, String, Void, Any, Nil, Unknown };

struct Function_type
{
    std::vector<Type_id> params;
    Type_id ret;
};
struct Array_type
{
    Type_id elem;
};
struct Optional_type
{
    Type_id base;
};
struct Iterator_type
{
    Type_id elem;
};

struct Model_type
{
    std::string name;
    std::vector<std::pair<std::string, Type_id>> fields;
    std::unordered_map<std::string, uint32_t> field_indices;
    std::vector<std::pair<std::string, Function_type>> methods;
    std::unordered_map<std::string, uint32_t> method_indices;
    std::vector<std::pair<std::string, Function_type>> static_methods;
    std::unordered_map<std::string, uint32_t> static_method_indices;
};

struct Union_type
{
    std::string name;
    std::vector<std::pair<std::string, Type_id>> variants;
};

struct Enum_type
{
    std::string name;
    Type_id base;
    uint32_t variant_table_id = 0;
};

struct Unresolved_type
{
    std::string name;
};

struct Type
{
    using Variant = std::variant<
        Primitive_kind,
        Function_type,
        Model_type,
        Array_type,
        Optional_type,
        Union_type,
        Iterator_type,
        Enum_type,
        Unresolved_type>;
    Variant data;

    template <typename T>
    bool is() const
    {
        return std::holds_alternative<T>(data);
    }
    template <typename T>
    const T &as() const
    {
        return std::get<T>(data);
    }
};

class Type_table
{
public:
    Type_table();

    // Pre-interned Primitives
    Type_id t_i8, t_i16, t_i32, t_i64;
    Type_id t_u8, t_u16, t_u32, t_u64;
    Type_id t_f16, t_f32, t_f64;
    Type_id t_bool, t_string, t_void, t_any, t_nil, t_unknown;

    // intern
    Type_id primitive(Primitive_kind p);
    Type_id array(Type_id elem);
    Type_id optional(Type_id base);
    Type_id function(const std::vector<Type_id> &params, Type_id ret);
    Type_id model(const std::string &name);
    Type_id model(const std::string &name, const std::vector<std::pair<std::string, Type_id>> &fields);
    Type_id union_(const std::string &name, const std::vector<std::pair<std::string, Type_id>> &variants);
    Type_id enum_(const std::string &name, Type_id base);
    Type_id iterator(types::Type_id elem);
    Type_id unresolved(const std::string &name);

    // query
    const Type &get(Type_id id) const;

    std::optional<uint32_t> get_model_field_index(Type_id id, const std::string &field_name) const;

    bool is_primitive(Type_id id) const;
    bool is_array(Type_id id) const;
    bool is_function(Type_id id) const;
    bool is_model(Type_id id) const;
    bool is_union(Type_id id) const;
    bool is_enum(Type_id id) const;
    bool is_iterator(Type_id id) const;
    bool is_unresolved(Type_id id) const;

    Primitive_kind get_primitive(Type_id id) const;
    Type_id get_array_elem(Type_id id) const;
    Type_id get_iter_elem(Type_id id) const;
    Type_id get_optional_base(Type_id id) const;

    bool is_optional(Type_id id) const;
    Type_id optional_base(Type_id id) const;
    uint32_t optional_depth(Type_id id) const;

    inline Type_id get_i8() const
    {
        return t_i8;
    }
    inline Type_id get_i16() const
    {
        return t_i16;
    }
    inline Type_id get_i32() const
    {
        return t_i32;
    }
    inline Type_id get_i64() const
    {
        return t_i64;
    }
    inline Type_id get_u8() const
    {
        return t_u8;
    }
    inline Type_id get_u16() const
    {
        return t_u16;
    }
    inline Type_id get_u32() const
    {
        return t_u32;
    }
    inline Type_id get_u64() const
    {
        return t_u64;
    }
    inline Type_id get_f16() const
    {
        return t_f16;
    }
    inline Type_id get_f32() const
    {
        return t_f32;
    }
    inline Type_id get_f64() const
    {
        return t_f64;
    }
    inline Type_id get_bool() const
    {
        return t_bool;
    }
    inline Type_id get_string() const
    {
        return t_string;
    }
    inline Type_id get_void() const
    {
        return t_void;
    }
    inline Type_id get_any() const
    {
        return t_any;
    }
    inline Type_id get_nil() const
    {
        return t_nil;
    }

    inline Type_id get_unknown() const
    {
        return t_unknown;
    }

    inline bool is_i8(Type_id id) const
    {
        return id == t_i8;
    }
    inline bool is_i16(Type_id id) const
    {
        return id == t_i16;
    }
    inline bool is_i32(Type_id id) const
    {
        return id == t_i32;
    }
    inline bool is_i64(Type_id id) const
    {
        return id == t_i64;
    }
    inline bool is_u8(Type_id id) const
    {
        return id == t_u8;
    }
    inline bool is_u16(Type_id id) const
    {
        return id == t_u16;
    }
    inline bool is_u32(Type_id id) const
    {
        return id == t_u32;
    }
    inline bool is_u64(Type_id id) const
    {
        return id == t_u64;
    }
    inline bool is_f16(Type_id id) const
    {
        return id == t_f16;
    }
    inline bool is_f32(Type_id id) const
    {
        return id == t_f32;
    }
    inline bool is_f64(Type_id id) const
    {
        return id == t_f64;
    }
    inline bool is_bool(Type_id id) const
    {
        return id == t_bool;
    }
    inline bool is_string(Type_id id) const
    {
        return id == t_string;
    }
    inline bool is_void(Type_id id) const
    {
        return id == t_void;
    }
    inline bool is_nil(Type_id id) const
    {
        return id == t_nil;
    }
    inline bool is_any(Type_id id) const
    {
        return id == t_any;
    }

    inline bool is_unknown(Type_id id) const
    {
        return id == t_unknown;
    }

    inline bool is_integer_primitive(Type_id k) const
    {
        return k == t_i8 || k == t_i16 || k == t_i32 || k == t_i64 || k == t_u8 || k == t_u16 || k == t_u32 || k == t_u64;
    }
    inline bool is_unsigned_integer_primitive(Type_id k) const
    {
        return k == t_u8 || k == t_u16 || k == t_u32 || k == t_u64;
    }
    inline bool is_signed_integer_primitive(Type_id k) const
    {
        return k == t_i8 || k == t_i16 || k == t_i32 || k == t_i64;
    }
    inline bool is_float_primitive(Type_id k) const
    {
        return k == t_f16 || k == t_f32 || k == t_f64;
    }
    inline bool is_numeric_primitive(Type_id k) const
    {
        return is_integer_primitive(k) || is_float_primitive(k);
    }

    std::string to_string(Type_id id) const;

    std::vector<Type> types_;
    std::unordered_map<Primitive_kind, Type_id> prim_cache_;

    struct Arr_key
    {
        Type_id elem;
        bool operator==(const Arr_key &o) const
        {
            return elem == o.elem;
        }
    };
    struct Opt_key
    {
        Type_id base;
        bool operator==(const Opt_key &o) const
        {
            return base == o.base;
        }
    };
    struct Arr_hash
    {
        size_t operator()(const Arr_key &k) const
        {
            return std::hash<uint32_t>{}(k.elem.value);
        }
    };
    struct Opt_hash
    {
        size_t operator()(const Opt_key &k) const
        {
            return std::hash<uint32_t>{}(k.base.value);
        }
    };

    struct Fn_key
    {
        std::vector<Type_id> params;
        Type_id ret;
        bool operator==(const Fn_key &o) const
        {
            return params == o.params && ret == o.ret;
        }
    };
    // AI generated
    struct Fn_hash
    {
        size_t operator()(const Fn_key &k) const
        {
            size_t h = std::hash<uint32_t>{}(k.ret.value);
            for (auto p : k.params) {
                h ^= std::hash<uint32_t>{}(p.value) + 0x9e3779b9 + (h << 6) + (h >> 2);
            }
            return h;
        }
    };
    struct Iter_key
    {
        Type_id elem;
        bool operator==(const Iter_key &o) const
        {
            return elem == o.elem;
        }
    };
    struct Iter_hash
    {
        size_t operator()(const Iter_key &k) const
        {
            return std::hash<uint32_t>{}(k.elem.value);
        }
    };

    std::unordered_map<Fn_key, Type_id, Fn_hash> fn_cache_;
    std::unordered_map<Arr_key, Type_id, Arr_hash> arr_cache_;
    std::unordered_map<Opt_key, Type_id, Opt_hash> opt_cache_;
    std::unordered_map<Iter_key, Type_id, Iter_hash> iter_cache_;
    std::unordered_map<std::string, Type_id> model_cache_;
    std::unordered_map<std::string, Type_id> union_cache_;
    std::unordered_map<std::string, Type_id> enum_cache_;
};

bool is_integer_primitive(Primitive_kind k);
bool is_unsigned_integer_primitive(Primitive_kind k);
bool is_signed_integer_primitive(Primitive_kind k);
bool is_float_primitive(Primitive_kind k);
bool is_numeric_primitive(Primitive_kind k);
int primitive_bit_width(Primitive_kind k);
std::string prim_to_string(Primitive_kind p);

} // namespace phos::types
