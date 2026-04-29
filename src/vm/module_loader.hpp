#pragma once

#include "../lexer/lexer.hpp"
#include "../memory/arena.hpp"
#include "../parser/ast.hpp"
#include "../parser/parser.hpp"
#include "../value/type.hpp"

#include <filesystem>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

namespace phos {

class Module_loader
{
public:
    // Resolves an import path like ["std", "math"] to a physical file and parses it.
    static std::vector<ast::Stmt_id> load_and_parse(
        const std::vector<std::string> &path_parts,
        const std::string &current_file_path,
        types::Type_table &shared_type_table,
        ast::Ast_tree &shared_tree,
        mem::Arena &shared_arena,
        err::Engine &shared_diagnostics)
    {
        // 1. Resolve relative path
        std::filesystem::path current_dir = std::filesystem::path(current_file_path).parent_path();
        std::filesystem::path target_path;
        for (const auto &part : path_parts) {
            target_path /= part;
        }
        target_path += ".phos";

        std::filesystem::path full_path = current_dir / target_path;
        std::string canonical_path;

        try {
            // Check if file exists before trying to canonicalize to avoid throw overhead
            if (!std::filesystem::exists(full_path)) {
                shared_diagnostics.error(0, 0, current_file_path, "Module file not found: {}", full_path.string());
                return {};
            }
            canonical_path = std::filesystem::canonical(full_path).string();
        } catch (...) {
            shared_diagnostics.error(0, 0, current_file_path, "Error resolving path: {}", full_path.string());
            return {};
        }

        // 2. Read File
        std::ifstream file(canonical_path);
        if (!file.is_open()) {
            shared_diagnostics.error(0, 0, current_file_path, "Failed to open module file: {}", canonical_path);
            return {};
        }

        std::stringstream buffer;
        buffer << file.rdbuf();
        std::string file_content = buffer.str();

        // 3. Lex using SHARED arena
        lex::Lexer lexer(file_content, shared_arena, canonical_path);
        auto lex_result = lexer.tokenize();

        if (lex_result.diagnostics.has_errors()) {
            // Using .append() as defined in your err.hpp
            shared_diagnostics.append(lex_result.diagnostics);
            return {};
        }

        // 4. Parse using SHARED tree and arena
        Parser parser(lex_result.tokens, shared_type_table, shared_tree, shared_arena, canonical_path);
        auto parse_result = parser.parse();

        if (parse_result.diagnostics.has_errors()) {
            shared_diagnostics.append(parse_result.diagnostics);
        }

        return parse_result.statements;
    }
};

} // namespace phos
