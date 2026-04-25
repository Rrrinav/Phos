#include "type_environment.hpp"

#include "core_library.hpp"

namespace phos {

Type_environment::Type_environment(types::Type_table &tt) : tt(tt)
{}

void Type_environment::define_native(
    const std::string &name, const std::vector<std::string> &params, const std::string &ret, Native_fn func)
{
    std::vector<types::Native_param> native_params;
    native_params.reserve(params.size());

    for (const auto &param : params) {
        native_params.push_back(types::Native_param{
            .name = "",
            .type_str = param,
            .default_value = std::nullopt
        });
    }

    native_signatures[name].push_back(types::Native_sig{.params = std::move(native_params), .ret_type_str = ret, .func = func});
}

void Type_environment::define_native(
    const std::string &name, const std::vector<types::Native_param> &params, const std::string &ret, Native_fn func)
{
    native_signatures[name].push_back(types::Native_sig{
        .params = params,
        .ret_type_str = ret,
        .func = func
    });
}

// Query API: Global Types
bool Type_environment::is_type_defined(const std::string &name) const
{
    return global_types.find(name) != global_types.end();
}

std::optional<types::Type_id> Type_environment::get_type(const std::string &name) const
{
    auto it = global_types.find(name);
    if (it != global_types.end()) {
        return it->second;
    }
    return std::nullopt;
}

// Query API: Functions
bool Type_environment::is_function_defined(const std::string &name) const
{
    return functions.find(name) != functions.end();
}

const types::Function_type_data *Type_environment::get_function(const std::string &name) const
{
    auto it = functions.find(name);
    if (it != functions.end()) {
        return &it->second;
    }
    return nullptr;
}

// Query API: Models
bool Type_environment::is_model_defined(const std::string &name) const
{
    return model_data.find(name) != model_data.end();
}

const types::Model_type_data *Type_environment::get_model(const std::string &name) const
{
    auto it = model_data.find(name);
    if (it != model_data.end()) {
        return &it->second;
    }
    return nullptr;
}

const types::Function_type_data *Type_environment::get_model_method(const std::string &model_name, const std::string &method_name) const
{
    auto model_it = model_data.find(model_name);
    if (model_it == model_data.end()) {
        return nullptr;
    }

    auto method_it = model_it->second.methods.find(method_name);
    if (method_it != model_it->second.methods.end()) {
        return &method_it->second;
    }

    return nullptr;
}

const types::Function_type_data *
Type_environment::get_model_static_method(const std::string &model_name, const std::string &method_name) const
{
    auto model_it = model_data.find(model_name);
    if (model_it == model_data.end()) {
        return nullptr;
    }

    auto method_it = model_it->second.static_methods.find(method_name);
    if (method_it != model_it->second.static_methods.end()) {
        return &method_it->second;
    }

    return nullptr;
}

std::optional<ast::Expr_id> Type_environment::get_model_field_default(const std::string &model_name, const std::string &field_name) const
{
    auto model_it = model_data.find(model_name);
    if (model_it == model_data.end()) {
        return std::nullopt;
    }

    auto field_it = model_it->second.field_defaults.find(field_name);
    if (field_it != model_it->second.field_defaults.end()) {
        return field_it->second;
    }

    return std::nullopt;
}

// Query API: Unions
bool Type_environment::is_union_defined(const std::string &name) const
{
    return union_data.find(name) != union_data.end();
}

const types::Union_type_data *Type_environment::get_union(const std::string &name) const
{
    auto it = union_data.find(name);
    if (it != union_data.end()) {
        return &it->second;
    }
    return nullptr;
}

std::optional<ast::Expr_id>
Type_environment::get_union_variant_default(const std::string &union_name, const std::string &variant_name) const
{
    auto union_it = union_data.find(union_name);
    if (union_it == union_data.end()) {
        return std::nullopt;
    }

    auto variant_it = union_it->second.variant_defaults.find(variant_name);
    if (variant_it != union_it->second.variant_defaults.end()) {
        return variant_it->second;
    }

    return std::nullopt;
}

// Query API: Enums
bool Type_environment::is_enum_defined(const std::string &name) const
{
    return enum_data.find(name) != enum_data.end();
}

const types::Enum_type_data *Type_environment::get_enum(const std::string &name) const
{
    auto it = enum_data.find(name);
    if (it != enum_data.end()) {
        return &it->second;
    }
    return nullptr;
}

std::optional<Value> Type_environment::get_enum_variant_value(const std::string &enum_name, const std::string &variant_name) const
{
    auto enum_it = enum_data.find(enum_name);
    if (enum_it == enum_data.end()) {
        return std::nullopt;
    }

    auto variant_it = enum_it->second.variants.find(variant_name);
    if (variant_it != enum_it->second.variants.end()) {
        return variant_it->second;
    }

    return std::nullopt;
}

// Query API: Natives
bool Type_environment::is_native_defined(const std::string &name) const
{
    return native_signatures.find(name) != native_signatures.end();
}

const std::vector<types::Native_sig> *Type_environment::get_native_signatures(const std::string &name) const
{
    auto it = native_signatures.find(name);
    if (it != native_signatures.end()) {
        return &it->second;
    }
    return nullptr;
}

// Core Registration
void Type_environment::register_core_methods()
{
    static auto numericals = [] () { return std::string{"i8 | i16 | i32 | i64 | f16 | f32 | f64"}; };
    this->define_native("clock", std::vector<std::string>{}, "f64", vm::core::native_clock);
    this->define_native("is_same", std::vector<std::string>{"T", "T"}, "bool", vm::core::native_clock);
    this->define_native("exit", std::vector<std::string>{"T"}, "void", vm::core::exit);
    this->define_native("to_string", std::vector<std::string>{numericals()}, "string", vm::numerical::to_string);
    this->define_native("parse_i64", std::vector<std::string>{"string"}, "i64?", vm::numerical::parse_i64);
    this->define_native("parse_f64", std::vector<std::string>{"string"}, "f64?", vm::numerical::parse_f64);

    // String
    this->define_native("String::starts_with", std::vector<std::string>{"string", "string"}, "bool", vm::string_methods::starts_with);
    this->define_native("String::ends_with", std::vector<std::string>{"string", "string"}, "bool", vm::string_methods::ends_with);
    this->define_native("String::to_upper", std::vector<std::string>{"string"}, "string", vm::string_methods::to_upper);
    this->define_native("String::to_lower", std::vector<std::string>{"string"}, "string", vm::string_methods::to_lower);
    this->define_native("String::trim", std::vector<std::string>{"string"}, "string", vm::string_methods::trim);
    this->define_native("String::split", std::vector<std::string>{"string", "string"}, "string[]", vm::string_methods::split);
    this->define_native("String::substr", std::vector<std::string>{"string", numericals(), numericals()}, "string", vm::string_methods::substr);
    this->define_native("String::repeat", std::vector<std::string>{"string", "i64"}, "string", vm::string_methods::repeat);
    this->define_native("String::index_of", std::vector<std::string>{"string", "string"}, "i64?", vm::string_methods::index_of);

    // Array
    this->define_native("Array::is_empty", std::vector<std::string>{"T[]"}, "bool", vm::array_methods::is_empty);
    this->define_native("Array::clear", std::vector<std::string>{"T[]"}, "void", vm::array_methods::clear);
    this->define_native("Array::push", std::vector<std::string>{"T[]", "T"}, "void", vm::array_methods::push);
    this->define_native("Array::pop", std::vector<std::string>{"T[]"}, "T", vm::array_methods::pop);
    this->define_native("Array::insert", std::vector<std::string>{"T[]", "i32", "T"}, "void", vm::array_methods::insert);
    this->define_native("Array::remove_at", std::vector<std::string>{"T[]", "T"}, "", vm::array_methods::remove_at);
    this->define_native("Array::remove_at", std::vector<std::string>{"T[]", "T"}, "", vm::array_methods::remove_at);
    this->define_native("Array::uo_remove_at", std::vector<std::string>{"T[]", "T"}, "", vm::array_methods::u_remove_at);
    this->define_native("Array::reverse", std::vector<std::string>{"T[]"}, "void", vm::array_methods::reverse);
}

} // namespace phos
