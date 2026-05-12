#pragma once

#include "core/arena.hpp"
#include "core/value/value.hpp"
#include "frontend/environment/compiler_context.hpp"
#include "virtual_machine/garbage_collector/gc_heap.hpp"
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

    // The newly added runtime Garbage Collected Heap
    gc::Gc_heap gc_heap;

    vm::Virtual_machine vm;

    std::ostream *out_stream;
    std::ostream *err_stream;

    Closure_data main_function{};
    std::vector<ast::Stmt_id> main_statements;

    Phos_engine(size_t memory_size_bytes = 1024 * 1024 * 10, std::ostream *out = &std::cout, std::ostream *err = &std::cerr);
    ~Phos_engine() = default;

    bool compile_file(const std::filesystem::path &file_path);
    bool compile_source(const std::string &source, const std::string &logical_name, const std::filesystem::path &root_dir);
    void execute();

    std::string dump_ir() const;
    std::string dump_ast(bool use_unicode = false) const;
};

} // namespace phos
