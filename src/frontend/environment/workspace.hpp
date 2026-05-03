#pragma once

#include "core/core_types.hpp"

#include <filesystem>
#include <format>
#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace phos {

struct Module_unit
{
    Module_id id;
    std::string logical_namespace;
    std::filesystem::path os_path;

    std::vector<ast::Stmt_id> ast_roots;
    std::unordered_map<std::string, Module_id> imports;
    std::unordered_map<std::string, Module_id> exported_modules;
    std::unordered_map<std::string, Symbol_id> exported_symbols;

    bool is_native = false;

    void add_ast_root(ast::Stmt_id stmt)
    {
        ast_roots.push_back(stmt);
    }

    void add_private_import(const std::string &alias, Module_id mod_id)
    {
        imports[alias] = mod_id;
    }

    void add_public_export(const std::string &alias, Module_id mod_id)
    {
        exported_modules[alias] = mod_id;
    }

    void add_public_symbol(const std::string &name, Symbol_id sym_id)
    {
        exported_symbols[name] = sym_id;
    }

    std::optional<Module_id> resolve_import(const std::string &alias) const
    {
        if (auto it = imports.find(alias); it != imports.end()) {
            return it->second;
        }
        if (auto it = exported_modules.find(alias); it != exported_modules.end()) {
            return it->second;
        }
        return std::nullopt;
    }

    std::optional<Symbol_id> resolve_exported_symbol(const std::string &name) const
    {
        if (auto it = exported_symbols.find(name); it != exported_symbols.end()) {
            return it->second;
        }
        return std::nullopt;
    }
};

class Workspace
{
public:
    std::vector<Module_unit> modules;

private:
    std::unordered_map<std::string, Module_id> path_to_module_;
    std::unordered_map<std::string, Module_id> namespace_to_module_;

    static std::string normalize_path_key(const std::filesystem::path &path)
    {
        return path.lexically_normal().generic_string();
    }

public:
    Module_id create_module(std::string logical_ns, std::filesystem::path path)
    {
        Module_id id{modules.size()};
        std::string path_str = normalize_path_key(path);

        modules.push_back(Module_unit{
            .id = id,
            .logical_namespace = std::move(logical_ns),
            .os_path = std::move(path),
            .ast_roots = {},
            .imports = {},
            .exported_modules = {},
            .exported_symbols = {},
            .is_native = false
        });

        path_to_module_[path_str] = id;
        namespace_to_module_[modules.back().logical_namespace] = id;
        return id;
    }

    decltype(auto) get_module(this auto &&self, Module_id id)
    {
        return self.modules[id.value];
    }

    bool is_loaded(const std::filesystem::path &path) const
    {
        return path_to_module_.contains(normalize_path_key(path));
    }

    std::optional<Module_id> get_module_by_path(const std::filesystem::path &path) const
    {
        if (auto it = path_to_module_.find(normalize_path_key(path)); it != path_to_module_.end()) {
            return it->second;
        }
        return std::nullopt;
    }

    std::optional<Module_id> get_module_by_namespace(const std::string &logical_namespace) const
    {
        if (auto it = namespace_to_module_.find(logical_namespace); it != namespace_to_module_.end()) {
            return it->second;
        }
        return std::nullopt;
    }

    auto begin()
    {
        return modules.begin();
    }
    auto end()
    {
        return modules.end();
    }
    auto begin() const
    {
        return modules.begin();
    }
    auto end() const
    {
        return modules.end();
    }

    std::vector<Module_id> get_topological_order() const
    {
        std::vector<Module_id> result;
        std::unordered_set<Module_id> visited;

        // We use a lambda for the recursive Depth-First Search
        auto visit = [&](auto &&self, Module_id id) -> void {
            if (visited.contains(id)) {
                return;
            }
            visited.insert(id);

            const auto &module = this->get_module(id);
            for (const auto &[alias, dep_id] : module.imports) {
                self(self, dep_id);
            }

            result.push_back(id);
        };

        for (const auto &mod : modules) {
            visit(visit, mod.id);
        }

        return result;
    }
};

} // namespace phos

namespace std {

template <>
struct formatter<phos::Module_unit, char>
{
    constexpr auto parse(format_parse_context &ctx)
    {
        return ctx.begin();
    }

    template <typename FormatContext>
    auto format(const phos::Module_unit &m, FormatContext &ctx) const
    {
        return format_to(
            ctx.out(),
            "[Module {}] '{}' ({}) -> {} AST nodes, {} Imports, {} Exports",
            m.id.value,
            m.logical_namespace,
            m.os_path.string(),
            m.ast_roots.size(),
            m.imports.size(),
            m.exported_symbols.size());
    }
};

template <>
struct formatter<phos::Workspace, char>
{
    constexpr auto parse(format_parse_context &ctx)
    {
        return ctx.begin();
    }

    template <typename FormatContext>
    auto format(const phos::Workspace &ws, FormatContext &ctx) const
    {
        auto out = format_to(ctx.out(), "Workspace ({} Modules)\n", ws.modules.size());
        for (const auto &mod : ws.modules) {
            out = format_to(out, "{}\n", mod);
        }
        return out;
    }
};

} // namespace std
