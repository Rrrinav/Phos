#include "repl.hpp"

#include "backend/compiler.hpp"
#include "frontend/lexer/lexer.hpp"
#include "frontend/parser/parser.hpp"

#include <cstdio>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>

#ifdef _WIN32
#include <io.h>
#else
#include <unistd.h>
#endif

namespace phos {

namespace {

// True when stdin is an interactive terminal rather than a pipe or file.
bool stdin_is_interactive()
{
#ifdef _WIN32
    return _isatty(_fileno(stdin)) != 0;
#else
    return isatty(STDIN_FILENO) != 0;
#endif
}

} // namespace

Repl::Repl(std::ostream *out, std::ostream *err)
    : arena(1024 * 1024 * 10), ctx(arena), gc_heap(), vm(gc_heap, arena), checker(ctx), compiler(ctx), out_(out ? out : &std::cout),
      err_(err ? err : &std::cerr)
{
    ctx.repl_force_global = true;

    vm.cfg.out = out_;
    vm.cfg.err = err_;

    // Catch VM panics instead of killing the session.
    vm.cfg.panic_handler = [this](const std::string &s) {
        *out_ << "[PANIC]: " << s << "\n";
        panicked_ = true;
    };

    ctx.type_env.register_core_methods();
}

void Repl::print_banner() const
{
    *out_ << "Phos REPL\n"
          << "  Type Phos statements directly. Use print(...) to see output.\n"
          << "  Commands: :help, :quit\n";
}

void Repl::print_help() const
{
    *out_ << "Phos REPL commands:\n"
          << "  :help             show this help\n"
          << "  :quit, :exit      leave the REPL\n"
          << "  :clear            reset all session bindings and functions\n"
          << "  :vars             list session variables and functions\n"
          << "  :type <expr>      show the type of an expression without running it\n"
          << "  :load <file>      run a .phos file in the session\n"
          << "  Ctrl-D            leave the REPL\n"
          << "\n"
          << "Editing: Up/Down history, Left/Right cursor, Home/End, Backspace,\n"
          << "         Ctrl-A/E line edges, Ctrl-U kill line, Ctrl-C abort line.\n"
          << "\n"
          << "Examples:\n"
          << "  >>> let x := 5\n"
          << "  >>> x * 2\n"
          << "  10\n"
          << "  >>> fn fib(n: i32) -> i32 { if n < 2 { return n; } return fib(n - 1) + fib(n - 2); }\n"
          << "  >>> fib(10)\n"
          << "  55\n";
}

// The lexer places the EOF token one past the final character, so an error
// reported at exactly that position means the input ended mid-construct.
bool Repl::error_at_eof(const err::msg &m, const std::string &text) const
{
    size_t eof_line = 1;
    size_t eof_col = 1;
    for (size_t i = 0; i < text.size(); ++i) {
        if (text[i] == '\n') {
            eof_line++;
            eof_col = 1;
        } else {
            eof_col++;
        }
    }
    return m.line == eof_line && m.column >= eof_col;
}

Repl::Attempt Repl::parse_attempt(const std::string &text)
{
    Attempt result;
    result.status = Parse_status::Error;

    lex::Lexer lexer(text, arena, "<repl>");
    auto lexed = lexer.tokenize();

    bool saw_error = false;
    bool at_eof = true;
    for (const auto &m : lexed.diagnostics.all()) {
        if (m.is_error() && m.summary != "Compilation halted due to syntax errors") {
            saw_error = true;
            if (!error_at_eof(m, text)) {
                at_eof = false;
            }
        }
    }

    if (saw_error) {
        result.status = at_eof ? Parse_status::Incomplete : Parse_status::Error;
        result.diagnostics = std::move(lexed.diagnostics);
        return result;
    }

    Parser parser(std::move(lexed.tokens), ctx, "<repl>");
    auto parse_result = parser.parse();

    bool any_error = false;
    at_eof = true;
    bool only_semicolon = true;
    for (const auto &m : parse_result.diagnostics.all()) {
        if (m.is_error() && m.summary != "Compilation halted due to syntax errors") {
            any_error = true;
            if (!error_at_eof(m, text)) {
                at_eof = false;
            }
            if (m.summary.find("';'") == std::string::npos) {
                only_semicolon = false;
            }
        }
    }

    if (parse_result.diagnostics.has_errors()) {
        if (any_error && at_eof && only_semicolon) {
            // A complete construct missing its trailing ';': retry with one.
            result.status = Parse_status::Error;
            result.semicolon_sugar = true;
        } else if (any_error && at_eof) {
            // The input ended mid-construct: keep reading lines.
            result.status = Parse_status::Incomplete;
        } else {
            result.status = Parse_status::Error;
        }
        result.diagnostics = std::move(parse_result.diagnostics);
        return result;
    }

    result.status = Parse_status::Ok;
    result.statements = std::move(parse_result.statements);
    return result;
}

// Like parse_attempt, but treats an unfinished construct as an error instead
// of requesting more input (used by :type and :load, whose input is complete).
Repl::Attempt Repl::parse_full(const std::string &text)
{
    Attempt attempt = parse_attempt(text);
    if (attempt.status == Parse_status::Error && attempt.semicolon_sugar) {
        Attempt retry = parse_attempt(text + ";");
        if (retry.status == Parse_status::Ok) {
            return retry;
        }
        attempt = std::move(retry);
    }
    if (attempt.status == Parse_status::Incomplete) {
        attempt.status = Parse_status::Error;
    }
    return attempt;
}

void Repl::submit_entry(Attempt &attempt)
{
    auto statements = std::move(attempt.statements);

    // Echo: a bare expression entry prints its value.
    if (statements.size() == 1 && std::holds_alternative<ast::Expr_stmt>(ctx.tree.get(statements[0]).node)) {
        auto &expr_stmt = std::get<ast::Expr_stmt>(ctx.tree.get(statements[0]).node);
        ast::Stmt_id print_id = ctx.tree.add_stmt(
            ast::Stmt{ast::Print_stmt{
                .stream = ast::Print_stream::STDOUT,
                .expressions = {expr_stmt.expression},
                .sep = " ",
                .end = "\n",
                .loc = expr_stmt.loc,
            }});
        statements[0] = print_id;
    }

    submit_statements(std::move(statements));
}

void Repl::submit_statements(std::vector<ast::Stmt_id> statements)
{
    Module_id mod_id = ctx.workspace.create_module("", "<repl-" + std::to_string(entry_index_++) + ">");
    for (auto stmt_id : statements) {
        ctx.workspace.get_module(mod_id).add_ast_root(stmt_id);
    }

    auto semantic_errors = checker.check_workspace();
    if (semantic_errors.has_errors()) {
        semantic_errors.print(*err_);
        return;
    }

    Closure_data entry = compiler.compile_module_only(mod_id);

    panicked_ = false;
    execute_closure(entry);
    if (panicked_) {
        *out_ << "\n(execution aborted after panic)\n";
    }
}

void Repl::execute_closure(const Closure_data &closure)
{
    constexpr size_t call_stack_capacity = 256;

    std::vector<vm::Call_frame> frames(call_stack_capacity);
    frames[0] = vm::Call_frame(const_cast<Closure_data *>(&closure), 0);

    std::vector<Value> thread_memory(call_stack_capacity * vm::Virtual_machine::FRAME_REGISTER_WINDOW);

    Green_thread_data main_thread{};
    main_thread.call_stack = frames.data();
    main_thread.call_stack_count = 1;
    main_thread.call_stack_capacity = frames.size();
    main_thread.value_stack = thread_memory.data();
    main_thread.value_stack_capacity = thread_memory.size();
    main_thread.live_value_count = vm::Virtual_machine::FRAME_REGISTER_WINDOW;
    main_thread.is_completed = false;

    vm.execute(&main_thread);
}

void Repl::clear_session()
{
    ctx.workspace.modules.clear();

    ctx.registry.symbols.clear();
    ctx.registry.global_index.clear();
    ctx.registry.next_global_index = 0;

    ctx.type_env.global_types.clear();
    ctx.type_env.functions.clear();
    ctx.type_env.model_data.clear();
    ctx.type_env.union_data.clear();
    ctx.type_env.enum_data.clear();

    checker.hoisted_modules.clear();
    checker.checked_modules.clear();

    compiler.clear_session();

    vm.globals.clear();
}

void Repl::print_vars()
{
    bool any = false;
    for (const auto &sym : ctx.registry.symbols) {
        if (sym.kind != Symbol_kind::Global_var && sym.kind != Symbol_kind::Phos_func && sym.kind != Symbol_kind::Phos_const) {
            continue;
        }
        if (sym.kind == Symbol_kind::Phos_const && sym.type == ctx.tt.get_unknown()) {
            continue;
        }
        std::string type_str;
        if (sym.kind == Symbol_kind::Phos_func) {
            type_str = ctx.tt.to_string(checker.resolve_symbol(sym.id));
        } else {
            type_str = ctx.tt.to_string(sym.type);
        }
        *out_ << sym.name << ": " << type_str << "\n";
        any = true;
    }
    if (!any) {
        *out_ << "(no session bindings)\n";
    }
}

void Repl::print_type(const std::string &text)
{
    Attempt attempt = parse_full(text);
    if (attempt.status == Parse_status::Error) {
        attempt.diagnostics.print(*err_);
        return;
    }

    if (attempt.statements.size() != 1 || !std::holds_alternative<ast::Expr_stmt>(ctx.tree.get(attempt.statements[0]).node)) {
        *out_ << "<expected a single expression>\n";
        return;
    }

    auto &expr_stmt = std::get<ast::Expr_stmt>(ctx.tree.get(attempt.statements[0]).node);
    ast::Expr_id expr_id = expr_stmt.expression;

    // A throwaway module lets the semantic pass resolve and stamp the
    // expression's type without compiling or executing anything.
    Module_id mod_id = ctx.workspace.create_module("", "<repl-type>");
    ast::Stmt_id wrapper = ctx.tree.add_stmt(
        ast::Stmt{ast::Print_stmt{
            .stream = ast::Print_stream::STDOUT,
            .expressions = {expr_id},
            .sep = " ",
            .end = "\n",
            .loc = expr_stmt.loc,
        }});
    ctx.workspace.get_module(mod_id).add_ast_root(wrapper);

    auto semantic_errors = checker.check_workspace();
    if (semantic_errors.has_errors()) {
        semantic_errors.print(*err_);
        return;
    }

    // Cast_expr carries its result in target_type; every other expression
    // node stores its resolved type in .type.
    types::Type_id expr_type = std::visit(
        [](const auto &node) -> types::Type_id {
            if constexpr (std::is_same_v<std::decay_t<decltype(node)>, ast::Cast_expr>) {
                return node.target_type;
            } else {
                return node.type;
            }
        },
        ctx.tree.get(expr_id).node);
    *out_ << ctx.tt.to_string(expr_type) << "\n";
}

void Repl::load_file(const std::string &path)
{
    std::ifstream file(path);
    if (!file.is_open()) {
        *err_ << "<repl>: error: cannot open file '" << path << "'\n";
        return;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();

    Attempt attempt = parse_full(buffer.str());
    if (attempt.status == Parse_status::Error) {
        attempt.diagnostics.print(*err_);
        return;
    }

    submit_statements(std::move(attempt.statements));
}

void Repl::run()
{
    print_banner();

    editor.set_interactive(stdin_is_interactive());

    std::string buffer;
    std::string line;

    for (;;) {
        std::string prompt = buffer.empty() ? ">>> " : "... ";
        auto result = editor.read(*out_, prompt);
        if (result.status == Line_editor::Result::Status::Eof) {
            if (!editor.interactive()) {
                *out_ << "\n";
            }
            break;
        }
        if (result.status == Line_editor::Result::Status::Interrupt) {
            buffer.clear();
            continue;
        }
        line = std::move(result.line);

        if (buffer.empty()) {
            if (line == ":quit" || line == ":exit") {
                break;
            }
            if (line == ":help") {
                print_help();
                continue;
            }
            if (line == ":clear") {
                clear_session();
                *out_ << "(session cleared)\n";
                continue;
            }
            if (line == ":vars") {
                print_vars();
                continue;
            }
            if (line.starts_with(":type ")) {
                print_type(line.substr(6));
                continue;
            }
            if (line.starts_with(":load ")) {
                load_file(line.substr(6));
                continue;
            }
            if (line.empty()) {
                continue;
            }
            if (line[0] == ':') {
                *err_ << "Unknown command '" << line << "'. Try :help.\n";
                continue;
            }
        }

        buffer += line;
        buffer += "\n";

        Attempt attempt = parse_attempt(buffer);

        if (attempt.status == Parse_status::Incomplete) {
            continue;
        }

        if (attempt.status == Parse_status::Error && attempt.semicolon_sugar) {
            Attempt retry = parse_attempt(buffer + ";");
            if (retry.status == Parse_status::Ok) {
                submit_entry(retry);
                buffer.clear();
                continue;
            }
            if (retry.status == Parse_status::Incomplete) {
                continue;
            }
            attempt = std::move(retry);
        }

        if (attempt.status == Parse_status::Ok) {
            submit_entry(attempt);
        } else if (attempt.status == Parse_status::Error && !attempt.diagnostics.empty()) {
            attempt.diagnostics.print(*err_);
        }
        buffer.clear();
    }
}

} // namespace phos
