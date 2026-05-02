#pragma once

#include "core/core_types.hpp"
#include "core/value/type.hpp"
#include "core/value/value.hpp"

#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

namespace phos {

struct Function_type_data
{
    ast::Stmt_id declaration = ast::Stmt_id::null();
};

struct Model_type_data
{
    std::unordered_map<std::string, ast::Expr_id> field_defaults;
    std::unordered_map<std::string, ast::Expr_id> static_fields;
    std::unordered_map<std::string, types::Type_id> static_field_types;
    std::unordered_map<std::string, Function_type_data> methods;
    std::unordered_map<std::string, Function_type_data> static_methods;
};

struct Union_type_data
{
    std::unordered_map<std::string, ast::Expr_id> variant_defaults;
};

struct Enum_type_data
{
    std::unordered_map<std::string, Value> variants;
};

// FFI / Native Method Registry (Signatures Only)
struct Native_param
{
    std::string name;
    std::string type_str;
    std::optional<Value> default_value;
};

struct Native_sig
{
    std::vector<Native_param> params;
    std::string ret_type_str;
    Native_fn func = nullptr;
};

class Type_environment
{
public:
    explicit Type_environment(types::Type_table &tt);

    types::Type_table &tt;

    // RESOLVED STRUCTURAL DATA
    std::unordered_map<std::string, types::Type_id> global_types;
    std::unordered_map<std::string, Function_type_data> functions;
    std::unordered_map<std::string, Model_type_data> model_data;
    std::unordered_map<std::string, Union_type_data> union_data;
    std::unordered_map<std::string, Enum_type_data> enum_data;

    // FFI / NATIVE BOUNDARIES
    std::unordered_map<std::string, std::vector<Native_sig>> native_signatures;
    std::unordered_map<std::string, Value> native_constants;

    // REGISTRATION API (Used by Binder to populate environment)
    // Returns 'false' if the item is already defined
    bool register_global_type(const std::string &name, types::Type_id type);
    bool register_function(const std::string &name, ast::Stmt_id declaration);

    // Models
    bool register_model(const std::string &name);
    bool add_model_field_default(const std::string &model_name, const std::string &field, ast::Expr_id expr);
    bool add_model_static_field(const std::string &model_name, const std::string &field, types::Type_id type, ast::Expr_id expr);
    bool add_model_method(const std::string &model_name, const std::string &method, ast::Stmt_id decl);
    bool add_model_static_method(const std::string &model_name, const std::string &method, ast::Stmt_id decl);

    // Unions & Enums
    bool register_union(const std::string &name);
    bool add_union_variant_default(const std::string &union_name, const std::string &variant, ast::Expr_id expr);

    bool register_enum(const std::string &name);
    bool add_enum_variant(const std::string &enum_name, const std::string &variant, Value val);

    // QUERY API (Used by Semantic Checker to validate types)
    bool is_type_defined(const std::string &name) const;
    std::optional<types::Type_id> get_type(const std::string &name) const;

    bool is_function_defined(const std::string &name) const;
    const Function_type_data *get_function(const std::string &name) const;

    bool is_model_defined(const std::string &name) const;
    const Model_type_data *get_model(const std::string &name) const;
    const Function_type_data *get_model_method(const std::string &model_name, const std::string &method_name) const;
    const Function_type_data *get_model_static_method(const std::string &model_name, const std::string &method_name) const;
    std::optional<ast::Expr_id> get_model_static_field(const std::string &model_name, const std::string &field_name) const;
    std::optional<ast::Expr_id> get_model_field_default(const std::string &model_name, const std::string &field_name) const;

    bool is_union_defined(const std::string &name) const;
    const Union_type_data *get_union(const std::string &name) const;
    std::optional<ast::Expr_id> get_union_variant_default(const std::string &union_name, const std::string &variant_name) const;

    bool is_enum_defined(const std::string &name) const;
    const Enum_type_data *get_enum(const std::string &name) const;
    std::optional<Value> get_enum_variant_value(const std::string &enum_name, const std::string &variant_name) const;

    // NATIVE FFI API
    void define_native(const std::string &name, const std::vector<std::string> &params, const std::string &ret, Native_fn func = nullptr);
    void define_native(const std::string &name, const std::vector<Native_param> &params, const std::string &ret, Native_fn func = nullptr);
    void define_native_const(const std::string &name, Value val);
    std::optional<Value> get_native_const(const std::string &name) const;
    bool is_native_defined(const std::string &name) const;
    const std::vector<Native_sig> *get_native_signatures(const std::string &name) const;

    void register_core_methods();
};

} // namespace phos
