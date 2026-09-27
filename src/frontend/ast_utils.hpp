#pragma once

// Shared AST/type utilities for the compiler passes.
//
// The resolver, semantic checker and compiler each carried their own copies
// of several of these helpers (node accessors, return-type normalization,
// type-string splitting). This module is the single home for them so the
// passes cannot drift apart.

#include "core/core_types.hpp"
#include "core/value/type.hpp"
#include "frontend/environment/compiler_context.hpp"
#include "frontend/parser/ast.hpp"

#include <algorithm>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace phos {

class Semantic_checker;

// --- Node accessors (used by every pass over the AST) ---
template <typename T>
T &get_node(ast::Ast_tree &tree, ast::Expr_id id)
{
    return std::get<T>(tree.get(id).node);
}

template <typename T>
T &get_stmt(ast::Ast_tree &tree, ast::Stmt_id id)
{
    return std::get<T>(tree.get(id).node);
}

template <typename Vec>
auto find_by_key(const Vec &vec, const std::string &key)
{
    return std::find_if(vec.begin(), vec.end(), [&](const auto &pair) { return pair.first == key; });
}

template <typename Vec>
bool contains_key(const Vec &vec, const std::string &key)
{
    return find_by_key(vec, key) != vec.end();
}

// --- Multi-return helpers ---
std::vector<types::Type_id> normalize_return_types(const std::vector<types::Type_id> &returns, const types::Type_table &tt);
std::vector<types::Type_id> declared_return_types(const std::vector<ast::Function_param> &returns, const types::Type_table &tt);
std::vector<std::pair<std::string, types::Type_id>> positional_multi_return_fields(const std::vector<types::Type_id> &returns);
types::Type_id effective_return_type(const std::vector<types::Type_id> &returns, types::Type_table &tt);
types::Type_id effective_return_type(const types::Function_type &fn, types::Type_table &tt);
types::Type_id effective_return_type(const std::vector<ast::Function_param> &returns, types::Type_table &tt);

// --- Type-string splitting (for parse_type_string and friends) ---
std::string trim_copy(std::string_view text);
std::vector<std::string> split_top_level(std::string_view text, char delimiter);
std::vector<std::string> split_top_level_fields(std::string_view text);

// --- Module/symbol helpers ---
std::optional<Module_id> imported_module_id(const Compiler_context &ctx, Module_id current_module_id, const std::string &alias);
types::Type_id resolve_symbol_type(Compiler_context &ctx, Symbol_id sym_id, Semantic_checker &checker);

} // namespace phos
