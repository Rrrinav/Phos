#include "frontend/ast_utils.hpp"

#include "frontend/semantic/semantic_checker.hpp"

#include <cctype>
#include <format>

namespace phos {

std::vector<types::Type_id> normalize_return_types(const std::vector<types::Type_id> &returns, const types::Type_table &tt)
{
    if (returns.size() == 1 && tt.is_void(returns.front())) {
        return {};
    }
    return returns;
}

std::vector<types::Type_id> declared_return_types(const std::vector<ast::Function_param> &returns, const types::Type_table &tt)
{
    std::vector<types::Type_id> out;
    out.reserve(returns.size());
    for (const auto &ret : returns) {
        out.push_back(ret.type);
    }
    return normalize_return_types(out, tt);
}

std::vector<std::pair<std::string, types::Type_id>> positional_multi_return_fields(const std::vector<types::Type_id> &returns)
{
    std::vector<std::pair<std::string, types::Type_id>> fields;
    fields.reserve(returns.size());
    for (size_t i = 0; i < returns.size(); ++i) {
        fields.push_back({std::format("_{}", i), returns[i]});
    }
    return fields;
}

types::Type_id effective_return_type(const std::vector<types::Type_id> &returns, types::Type_table &tt)
{
    auto normalized = normalize_return_types(returns, tt);
    if (normalized.empty()) {
        return tt.get_void();
    }
    if (normalized.size() == 1) {
        return normalized.front();
    }
    return tt.model("", positional_multi_return_fields(normalized));
}

types::Type_id effective_return_type(const types::Function_type &fn, types::Type_table &tt)
{
    return effective_return_type(fn.returns, tt);
}

types::Type_id effective_return_type(const std::vector<ast::Function_param> &returns, types::Type_table &tt)
{
    auto types = declared_return_types(returns, tt);
    if (types.empty()) {
        return tt.get_void();
    }
    if (types.size() == 1) {
        return types.front();
    }
    return tt.model("", positional_multi_return_fields(types));
}

std::string trim_copy(std::string_view text)
{
    size_t start = 0;
    while (start < text.size() && std::isspace(static_cast<unsigned char>(text[start]))) {
        ++start;
    }

    size_t end = text.size();
    while (end > start && std::isspace(static_cast<unsigned char>(text[end - 1]))) {
        --end;
    }

    return std::string(text.substr(start, end - start));
}

std::vector<std::string> split_top_level(std::string_view text, char delimiter)
{
    std::vector<std::string> parts;
    size_t start = 0;
    int paren_depth = 0;
    int brace_depth = 0;
    int angle_depth = 0;
    int bracket_depth = 0;

    for (size_t i = 0; i < text.size(); ++i) {
        switch (text[i]) {
        case '(':
            ++paren_depth;
            break;
        case ')':
            --paren_depth;
            break;
        case '{':
            ++brace_depth;
            break;
        case '}':
            --brace_depth;
            break;
        case '<':
            ++angle_depth;
            break;
        case '>':
            --angle_depth;
            break;
        case '[':
            ++bracket_depth;
            break;
        case ']':
            --bracket_depth;
            break;
        default:
            break;
        }

        if (text[i] == delimiter && paren_depth == 0 && brace_depth == 0 && angle_depth == 0 && bracket_depth == 0) {
            auto part = trim_copy(text.substr(start, i - start));
            if (!part.empty()) {
                parts.push_back(std::move(part));
            }
            start = i + 1;
        }
    }

    auto tail = trim_copy(text.substr(start));
    if (!tail.empty()) {
        parts.push_back(std::move(tail));
    }

    return parts;
}

std::vector<std::string> split_top_level_fields(std::string_view text)
{
    std::vector<std::string> parts;
    size_t start = 0;
    int paren_depth = 0;
    int brace_depth = 0;
    int angle_depth = 0;
    int bracket_depth = 0;

    for (size_t i = 0; i < text.size(); ++i) {
        switch (text[i]) {
        case '(':
            ++paren_depth;
            break;
        case ')':
            --paren_depth;
            break;
        case '{':
            ++brace_depth;
            break;
        case '}':
            --brace_depth;
            break;
        case '<':
            ++angle_depth;
            break;
        case '>':
            --angle_depth;
            break;
        case '[':
            ++bracket_depth;
            break;
        case ']':
            --bracket_depth;
            break;
        default:
            break;
        }

        bool is_separator = (text[i] == ';' || text[i] == ',') && paren_depth == 0 && brace_depth == 0 && angle_depth == 0 && bracket_depth == 0;
        if (is_separator) {
            auto part = trim_copy(text.substr(start, i - start));
            if (!part.empty()) {
                parts.push_back(std::move(part));
            }
            start = i + 1;
        }
    }

    auto tail = trim_copy(text.substr(start));
    if (!tail.empty()) {
        parts.push_back(std::move(tail));
    }

    return parts;
}

std::optional<Module_id> imported_module_id(const Compiler_context &ctx, Module_id current_module_id, const std::string &alias)
{
    if (current_module_id.is_null()) {
        return std::nullopt;
    }

    return ctx.workspace.get_module(current_module_id).resolve_import(alias);
}

types::Type_id resolve_symbol_type(Compiler_context &ctx, Symbol_id sym_id, Semantic_checker &checker)
{
    auto &sym = ctx.registry.get_symbol(sym_id);
    if (!ctx.tt.is_unknown(sym.type)) {
        return sym.type;
    }

    auto try_resolve = [&](const std::string &name) -> std::optional<types::Type_id> {
        if (auto native_t = ctx.type_env.get_native_type_str(name)) {
            return checker.parse_type_string(*native_t, {});
        }
        if (sym.kind == Symbol_kind::Native_func) {
            if (auto signatures = ctx.type_env.get_native_signatures(name); signatures && !signatures->empty()) {
                std::vector<types::Type_id> params;
                params.reserve(signatures->front().params.size());
                for (const auto &param : signatures->front().params) {
                    if (param.is_variadic) {
                        break;
                    }
                    params.push_back(checker.parse_type_string(param.type_str, {}));
                }
                return ctx.tt.function(params, checker.parse_type_string(signatures->front().ret_type_str, {}));
            }
        }
        return std::nullopt;
    };

    // 1. Try exact name match
    if (auto t = try_resolve(std::string(ctx.registry.resolve(sym.name)))) {
        return sym.type = *t;
    }

    // 2. Try namespace reconstruction (Fixes selective imports)
    if (!sym.owner_module.is_null()) {
        auto &mod = ctx.workspace.get_module(sym.owner_module);
        if (auto t = try_resolve(mod.logical_namespace + "::" + std::string(ctx.registry.resolve(sym.name)))) {
            return sym.type = *t;
        }
    }

    if (sym.kind == Symbol_kind::Phos_func) {
        auto resolve_phos = [&](const std::string &name) -> bool {
            if (auto func_data = ctx.type_env.get_function(name)) {
                if (auto *decl = std::get_if<ast::Function_stmt>(&ctx.tree.get(func_data->declaration).node)) {
                    std::vector<types::Type_id> params;
                    params.reserve(decl->parameters.size());
                    for (const auto &param : decl->parameters) {
                        params.push_back(param.type);
                    }
                    sym.type = ctx.tt.function(params, declared_return_types(decl->returns, ctx.tt));
                    return true;
                }
            }
            return false;
        };

        if (!resolve_phos(std::string(ctx.registry.resolve(sym.name))) && !sym.owner_module.is_null()) {
            auto &mod = ctx.workspace.get_module(sym.owner_module);
            resolve_phos(mod.logical_namespace + "::" + std::string(ctx.registry.resolve(sym.name)));
        }
    }

    return sym.type;
}

} // namespace phos
