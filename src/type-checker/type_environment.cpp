#include "type_environment.hpp"

namespace phos {

Type_environment::Type_environment(types::Type_table &tt) : tt(tt)
{
    register_core_methods();
}

void Type_environment::define_native(const std::string &name, const std::vector<std::string> &params, const std::string &ret)
{
    std::vector<Native_param> native_params;
    native_params.reserve(params.size());

    for (const auto &param : params) {
        native_params.push_back({.name = "", .type_str = param, .default_value = std::nullopt});
    }

    native_signatures[name].push_back({native_params, ret});
}

void Type_environment::define_native(const std::string &name, const std::vector<Native_param> &params, const std::string &ret)
{
    native_signatures[name].push_back({params, ret});
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

const Function_data *Type_environment::get_function(const std::string &name) const
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

const Model_data *Type_environment::get_model(const std::string &name) const
{
    auto it = model_data.find(name);
    if (it != model_data.end()) {
        return &it->second;
    }
    return nullptr;
}

const Function_data *Type_environment::get_model_method(const std::string &model_name, const std::string &method_name) const
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

const Function_data *Type_environment::get_model_static_method(const std::string &model_name, const std::string &method_name) const
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

const Union_data *Type_environment::get_union(const std::string &name) const
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

const Enum_data *Type_environment::get_enum(const std::string &name) const
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

const std::vector<Native_sig> *Type_environment::get_native_signatures(const std::string &name) const
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
    // Deliberately left empty to decouple native function definitions
    // and signatures until the Virtual Machine implementation.
}

} // namespace phos
