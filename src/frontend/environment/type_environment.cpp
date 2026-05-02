#include "type_environment.hpp"

namespace phos {

Type_environment::Type_environment(types::Type_table &tt) : tt(tt)
{}

// REGISTRATION API
bool Type_environment::register_global_type(const std::string &name, types::Type_id type)
{
    if (global_types.contains(name)) {
        return false;
    }
    global_types[name] = type;
    return true;
}

bool Type_environment::register_function(const std::string &name, ast::Stmt_id declaration)
{
    if (functions.contains(name)) {
        return false;
    }
    functions[name] = Function_type_data{.declaration = declaration};
    return true;
}

// --- Models ---
bool Type_environment::register_model(const std::string &name)
{
    if (model_data.contains(name)) {
        return false;
    }
    model_data[name] = Model_type_data{};
    return true;
}

bool Type_environment::add_model_field_default(const std::string &model_name, const std::string &field, ast::Expr_id expr)
{
    if (!model_data.contains(model_name)) {
        return false;
    }
    model_data[model_name].field_defaults[field] = expr;
    return true;
}

bool Type_environment::add_model_static_field(const std::string &model_name, const std::string &field, types::Type_id type, ast::Expr_id expr)
{
    if (!model_data.contains(model_name)) {
        return false;
    }
    model_data[model_name].static_field_types[field] = type;
    model_data[model_name].static_fields[field] = expr;
    return true;
}

bool Type_environment::add_model_method(const std::string &model_name, const std::string &method, ast::Stmt_id decl)
{
    if (!model_data.contains(model_name)) {
        return false;
    }
    if (model_data[model_name].methods.contains(method)) {
        return false;
    }
    model_data[model_name].methods[method] = Function_type_data{.declaration = decl};
    return true;
}

bool Type_environment::add_model_static_method(const std::string &model_name, const std::string &method, ast::Stmt_id decl)
{
    if (!model_data.contains(model_name)) {
        return false;
    }
    if (model_data[model_name].static_methods.contains(method)) {
        return false;
    }
    model_data[model_name].static_methods[method] = Function_type_data{.declaration = decl};
    return true;
}

// --- Unions ---
bool Type_environment::register_union(const std::string &name)
{
    if (union_data.contains(name)) {
        return false;
    }
    union_data[name] = Union_type_data{};
    return true;
}

bool Type_environment::add_union_variant_default(const std::string &union_name, const std::string &variant, ast::Expr_id expr)
{
    if (!union_data.contains(union_name)) {
        return false;
    }
    union_data[union_name].variant_defaults[variant] = expr;
    return true;
}

// --- Enums ---
bool Type_environment::register_enum(const std::string &name)
{
    if (enum_data.contains(name)) {
        return false;
    }
    enum_data[name] = Enum_type_data{};
    return true;
}

bool Type_environment::add_enum_variant(const std::string &enum_name, const std::string &variant, Value val)
{
    if (!enum_data.contains(enum_name)) {
        return false;
    }
    if (enum_data[enum_name].variants.contains(variant)) {
        return false;
    }
    enum_data[enum_name].variants[variant] = val;
    return true;
}

// ============================================================================
// QUERY API
// ============================================================================

bool Type_environment::is_type_defined(const std::string &name) const
{
    return global_types.contains(name);
}

std::optional<types::Type_id> Type_environment::get_type(const std::string &name) const
{
    if (auto it = global_types.find(name); it != global_types.end()) {
        return it->second;
    }
    return std::nullopt;
}

bool Type_environment::is_function_defined(const std::string &name) const
{
    return functions.contains(name);
}

const Function_type_data *Type_environment::get_function(const std::string &name) const
{
    if (auto it = functions.find(name); it != functions.end()) {
        return &it->second;
    }
    return nullptr;
}

bool Type_environment::is_model_defined(const std::string &name) const
{
    return model_data.contains(name);
}

const Model_type_data *Type_environment::get_model(const std::string &name) const
{
    if (auto it = model_data.find(name); it != model_data.end()) {
        return &it->second;
    }
    return nullptr;
}

const Function_type_data *Type_environment::get_model_method(const std::string &model_name, const std::string &method_name) const
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

const Function_type_data *Type_environment::get_model_static_method(const std::string &model_name, const std::string &method_name) const
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

std::optional<ast::Expr_id> Type_environment::get_model_static_field(const std::string &model_name, const std::string &field_name) const
{
    auto model_it = model_data.find(model_name);
    if (model_it == model_data.end()) {
        return std::nullopt;
    }
    auto field_it = model_it->second.static_fields.find(field_name);
    if (field_it != model_it->second.static_fields.end()) {
        return field_it->second;
    }
    return std::nullopt;
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

bool Type_environment::is_union_defined(const std::string &name) const
{
    return union_data.contains(name);
}

const Union_type_data *Type_environment::get_union(const std::string &name) const
{
    if (auto it = union_data.find(name); it != union_data.end()) {
        return &it->second;
    }
    return nullptr;
}

std::optional<ast::Expr_id> Type_environment::get_union_variant_default(const std::string &union_name, const std::string &variant_name) const
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

bool Type_environment::is_enum_defined(const std::string &name) const
{
    return enum_data.contains(name);
}

const Enum_type_data *Type_environment::get_enum(const std::string &name) const
{
    if (auto it = enum_data.find(name); it != enum_data.end()) {
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

// ============================================================================
// NATIVE FFI REGISTRATION
// ============================================================================

void Type_environment::define_native(const std::string &name, const std::vector<std::string> &params, const std::string &ret, Native_fn func)
{
    std::vector<Native_param> native_params;
    native_params.reserve(params.size());

    for (const auto &param : params) {
        native_params.push_back(Native_param{.name = "", .type_str = param, .default_value = std::nullopt});
    }

    native_signatures[name].push_back(Native_sig{.params = std::move(native_params), .ret_type_str = ret, .func = func});
}

void Type_environment::define_native(const std::string &name, const std::vector<Native_param> &params, const std::string &ret, Native_fn func)
{
    native_signatures[name].push_back(Native_sig{.params = params, .ret_type_str = ret, .func = func});
}

void Type_environment::define_native_const(const std::string &name, Value val)
{
    native_constants[name] = val;
}

std::optional<Value> Type_environment::get_native_const(const std::string &name) const
{
    if (auto it = native_constants.find(name); it != native_constants.end()) {
        return it->second;
    }
    return std::nullopt;
}

bool Type_environment::is_native_defined(const std::string &name) const
{
    return native_signatures.contains(name);
}

const std::vector<Native_sig> *Type_environment::get_native_signatures(const std::string &name) const
{
    if (auto it = native_signatures.find(name); it != native_signatures.end()) {
        return &it->second;
    }
    return nullptr;
}

void Type_environment::register_core_methods()
{
    // Implement your core standard library injection here later!
}

} // namespace phos
