#pragma once

#include "core/arena.hpp"
#include "core/error/err.hpp"
#include "backend/compiler.hpp"
#include "frontend/environment/compiler_context.hpp"
#include "frontend/semantic/semantic_checker.hpp"
#include "virtual_machine/garbage_collector/gc_heap.hpp"
#include "virtual_machine/virtual_machine.hpp"

#include <iosfwd>
#include <string>
#include <vector>

namespace phos {

class Repl
{
public:
    Repl(std::ostream *out = nullptr, std::ostream *err = nullptr);

    void run();

private:
    mem::Arena arena;
    Compiler_context ctx;
    gc::Gc_heap gc_heap;
    vm::Virtual_machine vm;
    Semantic_checker checker;
    vm::Compiler compiler;

    std::ostream *out_;
    std::ostream *err_;

    int entry_index_ = 0;
    bool panicked_ = false;

    enum class Parse_status { Ok, Incomplete, Error };

    struct Attempt
    {
        Parse_status status;
        bool semicolon_sugar = false;
        err::Engine diagnostics;
        std::vector<ast::Stmt_id> statements;
    };

    bool error_at_eof(const err::msg &m, const std::string &text) const;
    Attempt parse_attempt(const std::string &text);
    void submit_entry(Attempt &attempt);
    void execute_closure(const Closure_data &closure);
    void print_banner() const;
    void print_help() const;
};

} // namespace phos