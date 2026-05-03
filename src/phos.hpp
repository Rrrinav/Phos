#pragma once

#include "core/arena.hpp"
#include "core/value/value.hpp"
#include "frontend/environment/compiler_context.hpp"
#include "virtual_machine/virtual_machine.hpp"

#include <filesystem>
#include <iostream>
#include <string>
#include <vector>

namespace phos {

class Phos_engine
{
public:
    mem::Arena arena;
    Compiler_context ctx;
    vm::Virtual_machine vm;
    Closure_data main_function;

    std::ostream* out_stream;
    std::ostream* err_stream;

    // Cache the top-level AST so we can print it later if requested
    std::vector<ast::Stmt_id> main_statements;

    explicit Phos_engine(size_t memory_size_bytes = 10 * 1024 * 1024, std::ostream *out = &std::cout, std::ostream *err = &std::cerr);

    // Pipeline entry points
    bool compile_file(const std::filesystem::path &file_path);
    bool compile_source(const std::string &source, const std::string &logical_name, const std::filesystem::path &root_dir);

    // Execution
    void execute();

    // Diagnostics & Tooling
    std::string dump_ir() const;
    std::string dump_ast(bool use_unicode = true) const;
};

} // namespace phos
