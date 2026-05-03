#pragma once

#include <iostream>
#include <string>
#include <string_view>
#include <vector>

namespace phos::cli {

struct Phos_config
{
    // AST Subcommands & Options
    bool cmd_ast = false;
    bool cmd_ast_print = false;
    bool ast_print_unicode = false;

    // Global Options
    bool print_ir = false;
    std::string ir_out_file = "ir.phosasm";

    bool trace_vm = false;
    bool help = false;

    // Positional
    std::string input_file = "";

    // Future proofing: args to pass to the running Phos script
    std::vector<std::string> program_args;
};

struct Parse_result
{
    Phos_config config;
    bool success;
    std::string error_message;
};

inline void print_help()
{
    std::cout << "== Phos Compiler & Virtual Machine\n\n"
              << "USAGE: phos [SUBCOMMAND] [OPTIONS] <file>\n\n"
              << "SUBCOMMANDS:\n"
              << "  ast             AST manipulation tools\n"
              << "    print         Print the AST\n"
              << "      --unicode   Use unicode characters for AST printing\n\n"
              << "OPTIONS:\n"
              << "  --print_ir      Print Intermediate Representation (IR)\n"
              << "  -o <file>       Specify output file for IR (default: ir.phosasm)\n"
              << "  --trace_vm      Enable VM execution tracing\n"
              << "  -h, --help      Print this help message\n\n"
              << "EXAMPLE:\n"
              << "  phos ast print --unicode main.phos\n"
              << "  phos --print_ir -o custom.phosasm main.phos\n";
}

inline Parse_result parse_args(int argc, char *argv[])
{
    Phos_config cfg;
    std::vector<std::string_view> args;

    for (int i = 1; i < argc; ++i) {
        args.push_back(argv[i]);
    }

    // State machine context
    enum class Context { Global, Ast, AstPrint };
    Context ctx = Context::Global;

    for (size_t i = 0; i < args.size(); ++i) {
        auto arg = args[i];

        if (arg == "-h" || arg == "--help") {
            cfg.help = true;
            return {cfg, true, ""}; // Early exit, help overrides everything
        } else if (arg == "ast" && ctx == Context::Global) {
            cfg.cmd_ast = true;
            ctx = Context::Ast;
        } else if (arg == "print" && ctx == Context::Ast) {
            cfg.cmd_ast_print = true;
            ctx = Context::AstPrint;
        } else if (arg == "--unicode" && ctx == Context::AstPrint) {
            cfg.ast_print_unicode = true;
        } else if (arg == "--print_ir") {
            cfg.print_ir = true;
        } else if (arg == "-o") {
            if (i + 1 < args.size() && !args[i + 1].starts_with("-")) {
                cfg.ir_out_file = std::string(args[++i]);
            } else {
                return {cfg, false, "Error: '-o' requires an output filename."};
            }
        } else if (arg == "--trace_vm") {
            cfg.trace_vm = true;
        } else if (arg == "--") {
            // Everything after '--' gets forwarded to the running Phos program
            for (size_t j = i + 1; j < args.size(); ++j) {
                cfg.program_args.push_back(std::string(args[j]));
            }
            break;
        } else if (arg.starts_with("-")) {
            return {cfg, false, "Error: Unknown option '" + std::string(arg) + "'"};
        } else {
            // If it doesn't match a command and doesn't start with '-', it's the input file
            if (cfg.input_file.empty()) {
                cfg.input_file = std::string(arg);
            } else {
                return {cfg, false, "Error: Multiple input files specified ('" + cfg.input_file + "' and '" + std::string(arg) + "')."};
            }
        }
    }

    // Enforce Rule 5: Cannot run without a file (unless -h was passed)
    if (!cfg.help && cfg.input_file.empty()) {
        return {cfg, false, "Error: No input file specified."};
    }

    return {cfg, true, ""};
}

} // namespace phos::cli
