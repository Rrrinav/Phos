#pragma once

#include "backend/compiler.hpp"
#include "core/arena.hpp"
#include "core/error/err.hpp"
#include "frontend/environment/compiler_context.hpp"
#include "frontend/semantic/semantic_checker.hpp"
#include "line_editor.hpp"
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
    Line_editor editor;

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
    Attempt parse_full(const std::string &text);
    void submit_entry(Attempt &attempt);
    void submit_statements(std::vector<ast::Stmt_id> statements);
    void execute_closure(const Closure_data &closure);
    void print_banner() const;
    void print_help() const;

    // Session commands
    void clear_session();
    void print_vars();
    void print_type(const std::string &text);
    void load_file(const std::string &path);
};

} // namespace phos
