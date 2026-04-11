#include "type.hpp"

namespace phos::types {

Type_table::Type_table()
{
    this->t_i8     = primitive(Primitive_kind::I8);
    this->t_i16    = primitive(Primitive_kind::I16);
    this->t_i32    = primitive(Primitive_kind::I32);
    this->t_i64    = primitive(Primitive_kind::I64);

    this->t_u8     = primitive(Primitive_kind::U8);
    this->t_u16    = primitive(Primitive_kind::U16);
    this->t_u32    = primitive(Primitive_kind::U32);
    this->t_u64    = primitive(Primitive_kind::U64);

    this->t_f16    = primitive(Primitive_kind::F16);
    this->t_f32    = primitive(Primitive_kind::F32);
    this->t_f64    = primitive(Primitive_kind::F64);

    this->t_bool   = primitive(Primitive_kind::Bool);
    this->t_string = primitive(Primitive_kind::String);
    this->t_void   = primitive(Primitive_kind::Void);
    this->t_any    = primitive(Primitive_kind::Any);
    this->t_nil    = primitive(Primitive_kind::Nil);
}

Type_id Type_table::primitive(Primitive_kind p)
{
    auto it = prim_cache_.find(p);
    if (it != prim_cache_.end())
        return it->second;
    Type_id id{(uint32_t)types_.size()};
    types_.push_back(Type{p});
    prim_cache_[p] = id;
    return id;
}

Type_id Type_table::array(Type_id elem)
{
    Arr_key k{elem};
    auto it = arr_cache_.find(k);
    if (it != arr_cache_.end())
        return it->second;
    Type_id id{(uint32_t)types_.size()};
    types_.push_back(Type{Array_type{elem}});
    arr_cache_[k] = id;
    return id;
}

Type_id Type_table::optional(Type_id base)
{
    if (is_nil(base))
        return base;
    Opt_key k{base};
    auto it = opt_cache_.find(k);
    if (it != opt_cache_.end())
        return it->second;
    Type_id id{(uint32_t)types_.size()};
    types_.push_back(Type{Optional_type{base}});
    opt_cache_[k] = id;
    return id;
}

Type_id Type_table::union_(const std::string &name, const std::vector<std::pair<std::string, Type_id>> &variants)
{
    auto it = union_cache_.find(name);
    if (it != union_cache_.end())
        return it->second;
    Union_type u;
    u.name = name;
    u.variants = variants;
    Type_id id{(uint32_t)types_.size()};
    types_.push_back(Type{u});
    union_cache_[name] = id;
    return id;
}

Type_id Type_table::function(const std::vector<Type_id> &params, Type_id ret)
{
    Fn_key k{params, ret};
    auto it = fn_cache_.find(k);
    if (it != fn_cache_.end())
        return it->second;
    Type_id id{(uint32_t)types_.size()};
    types_.push_back(Type{Function_type{params, ret}});
    fn_cache_[k] = id;
    return id;
}

Type_id Type_table::enum_(const std::string &name, Type_id base)
{
    auto it = enum_cache_.find(name);
    if (it != enum_cache_.end())
        return it->second;
    Enum_type e;
    e.name = name;
    e.base = base;
    Type_id id{(uint32_t)types_.size()};
    types_.push_back(Type{e});
    enum_cache_[name] = id;
    return id;
}

Type_id Type_table::model(const std::string &name, const std::vector<std::pair<std::string, Type_id>> &fields)
{
    if (!name.empty()) {
        auto it = model_cache_.find(name);
        if (it != model_cache_.end())
            return it->second;
    }
    Model_type m;
    m.name = name;
    m.fields = fields;
    Type_id id{(uint32_t)types_.size()};
    types_.push_back(Type{m});
    if (!name.empty())
        model_cache_[name] = id;
    return id;
}

Type_id Type_table::model(const std::string &name)
{
    return model(name, {});
}

Type_id Type_table::unresolved(const std::string &name)
{
    Type_id id{(uint32_t)types_.size()};
    types_.push_back(Type{Unresolved_type{name}});
    return id;
}

const Type &Type_table::get(Type_id id) const
{
    return types_[id.value];
}
bool Type_table::is_primitive(Type_id id) const
{
    return std::holds_alternative<Primitive_kind>(types_[id.value].data);
}
bool Type_table::is_array(Type_id id) const
{
    return std::holds_alternative<Array_type>(types_[id.value].data);
}
bool Type_table::is_function(Type_id id) const
{
    return std::holds_alternative<Function_type>(types_[id.value].data);
}
bool Type_table::is_model(Type_id id) const
{
    return std::holds_alternative<Model_type>(types_[id.value].data);
}
bool Type_table::is_union(Type_id id) const
{
    return std::holds_alternative<Union_type>(types_[id.value].data);
}
bool Type_table::is_iterator(Type_id id) const
{
    return std::holds_alternative<Iterator_type>(types_[id.value].data);
}
bool Type_table::is_unresolved(Type_id id) const
{
    return std::holds_alternative<Unresolved_type>(types_[id.value].data);
}

Primitive_kind Type_table::get_primitive(Type_id id) const
{
    return std::get<Primitive_kind>(types_[id.value].data);
}
Type_id Type_table::get_array_elem(Type_id id) const
{
    return std::get<Array_type>(types_[id.value].data).elem;
}
Type_id Type_table::get_optional_base(Type_id id) const
{
    return std::get<Optional_type>(types_[id.value].data).base;
}

bool Type_table::is_optional(Type_id id) const
{
    return std::holds_alternative<Optional_type>(types_[id.value].data);
}
Type_id Type_table::optional_base(Type_id id) const
{
    while (is_optional(id))
        id = get_optional_base(id);
    return id;
}
uint32_t Type_table::optional_depth(Type_id id) const
{
    uint32_t d = 0;
    while (is_optional(id)) {
        id = get_optional_base(id);
        ++d;
    }
    return d;
}

bool is_integer_primitive(Primitive_kind k)
{
    switch (k) {
    case Primitive_kind::I8:
    case Primitive_kind::I16:
    case Primitive_kind::I32:
    case Primitive_kind::I64:
    case Primitive_kind::U8:
    case Primitive_kind::U16:
    case Primitive_kind::U32:
    case Primitive_kind::U64:
        return true;
    default:
        return false;
    }
}
bool is_unsigned_integer_primitive(Primitive_kind k)
{
    switch (k) {
    case Primitive_kind::U8:
    case Primitive_kind::U16:
    case Primitive_kind::U32:
    case Primitive_kind::U64:
        return true;
    default:
        return false;
    }
}
bool is_signed_integer_primitive(Primitive_kind k)
{
    switch (k) {
    case Primitive_kind::I8:
    case Primitive_kind::I16:
    case Primitive_kind::I32:
    case Primitive_kind::I64:
        return true;
    default:
        return false;
    }
}
bool is_float_primitive(Primitive_kind k)
{
    switch (k) {
    case Primitive_kind::F16:
    case Primitive_kind::F32:
    case Primitive_kind::F64:
        return true;
    default:
        return false;
    }
}
bool is_numeric_primitive(Primitive_kind k)
{
    return is_integer_primitive(k) || is_float_primitive(k);
}

int primitive_bit_width(Primitive_kind k)
{
    switch (k) {
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

std::string Type_table::to_string(Type_id id) const
{
    const auto &t = types_[id.value];

    if (std::holds_alternative<Primitive_kind>(t.data))
        return prim_to_string(std::get<Primitive_kind>(t.data));

    if (std::holds_alternative<Array_type>(t.data))
        return to_string(get_array_elem(id)) + "[]";

    if (std::holds_alternative<Optional_type>(t.data))
        return to_string(get_optional_base(id)) + "?";

    if (std::holds_alternative<Unresolved_type>(t.data))
        return std::get<Unresolved_type>(t.data).name;

    if (std::holds_alternative<Function_type>(t.data)) {
        const auto &f = std::get<Function_type>(t.data);
        std::string s = "fn(";
        for (size_t i = 0; i < f.params.size(); ++i) {
            s += to_string(f.params[i]);
            if (i + 1 < f.params.size())
                s += ", ";
        }
        return s + ") -> " + to_string(f.ret);
    }

    if (std::holds_alternative<Model_type>(t.data)) {
        const auto &m = std::get<Model_type>(t.data);
        std::string s = "model " + m.name + " { ";
        for (size_t i = 0; i < m.fields.size(); ++i) {
            s += to_string(m.fields[i].second);
            if (i + 1 < m.fields.size())
                s += ", ";
        }
        return s + " }";
    }
    if (std::holds_alternative<Union_type>(t.data)) {
        const auto &u = std::get<Union_type>(t.data);
        std::string s = "union " + u.name + " { ";
        for (size_t i = 0; i < u.variants.size(); ++i) {
            s += u.variants[i].first + ": " + to_string(u.variants[i].second);
            if (i + 1 < u.variants.size())
                s += ", ";
        }
        return s + " }";
    }
    if (std::holds_alternative<Enum_type>(t.data))
        return std::get<Enum_type>(t.data).name;

    if (std::holds_alternative<Iterator_type>(t.data))
        return "iter<" + to_string(std::get<Iterator_type>(t.data).elem) + ">";

    return "<unknown>";
}

std::string prim_to_string(Primitive_kind p)
{
    switch (p) {
    case Primitive_kind::I8:
        return "i8";
    case Primitive_kind::I16:
        return "i16";
    case Primitive_kind::I32:
        return "i32";
    case Primitive_kind::I64:
        return "i64";
    case Primitive_kind::U8:
        return "u8";
    case Primitive_kind::U16:
        return "u16";
    case Primitive_kind::U32:
        return "u32";
    case Primitive_kind::U64:
        return "u64";
    case Primitive_kind::F16:
        return "f16";
    case Primitive_kind::F32:
        return "f32";
    case Primitive_kind::F64:
        return "f64";
    case Primitive_kind::Bool:
        return "bool";
    case Primitive_kind::String:
        return "string";
    case Primitive_kind::Void:
        return "void";
    case Primitive_kind::Any:
        return "any";
    case Primitive_kind::Nil:
        return "nil";
    }
    return "<prim>";
}

} // namespace phos::types
