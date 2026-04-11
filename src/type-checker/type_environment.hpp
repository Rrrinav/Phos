#pragma once

#include "../parser/ast.hpp"
#include "../value/type.hpp"
#include "../value/value.hpp"

#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

namespace phos {

struct Function_data
{
    ast::Stmt_id declaration = ast::Stmt_id::null();
};

struct Model_data
{
    std::unordered_map<std::string, ast::Expr_id> field_defaults;
    std::unordered_map<std::string, Function_data> methods;
    std::unordered_map<std::string, Function_data> static_methods;
};

struct Union_data
{
    std::unordered_map<std::string, ast::Expr_id> variant_defaults;
};

struct Enum_data
{
    std::unordered_map<std::string, Value> variants;
};

// FFI / Native Method Registry (Signatures Only)
struct Native_param
{
    std::string name;
    std::string type_str; // e.g. "i64" or generic "T"
    std::optional<Value> default_value;
};

struct Native_sig
{
    std::vector<Native_param> params;
    std::string ret_type_str;
};

// =============================================================================
// Global Type Environment
// =============================================================================

class Type_environment
{
public:
    explicit Type_environment(types::Type_table &tt);

    types::Type_table &tt;

    // Resolved AST Data
    std::unordered_map<std::string, types::Type_id> global_types;
    std::unordered_map<std::string, Function_data> functions;
    std::unordered_map<std::string, Model_data> model_data;
    std::unordered_map<std::string, Union_data> union_data;
    std::unordered_map<std::string, Enum_data> enum_data;

    // Native FFI Signatures
    std::unordered_map<std::string, std::vector<Native_sig>> native_signatures;

    // Registration API
    void define_native(const std::string &name, const std::vector<std::string> &params, const std::string &ret);
    void define_native(const std::string &name, const std::vector<Native_param> &params, const std::string &ret);

    // Query API: Global Types
    bool is_type_defined(const std::string &name) const;
    std::optional<types::Type_id> get_type(const std::string &name) const;

    // Query API: Functions
    bool is_function_defined(const std::string &name) const;
    const Function_data *get_function(const std::string &name) const;

    // Query API: Models
    bool is_model_defined(const std::string &name) const;
    const Model_data *get_model(const std::string &name) const;
    const Function_data *get_model_method(const std::string &model_name, const std::string &method_name) const;
    const Function_data *get_model_static_method(const std::string &model_name, const std::string &method_name) const;
    std::optional<ast::Expr_id> get_model_field_default(const std::string &model_name, const std::string &field_name) const;

    // Query API: Unions
    bool is_union_defined(const std::string &name) const;
    const Union_data *get_union(const std::string &name) const;
    std::optional<ast::Expr_id> get_union_variant_default(const std::string &union_name, const std::string &variant_name) const;

    // Query API: Enums
    bool is_enum_defined(const std::string &name) const;
    const Enum_data *get_enum(const std::string &name) const;
    std::optional<Value> get_enum_variant_value(const std::string &enum_name, const std::string &variant_name) const;

    // Query API: Natives
    bool is_native_defined(const std::string &name) const;
    const std::vector<Native_sig> *get_native_signatures(const std::string &name) const;

    void register_core_methods();
};

} // namespace phos
