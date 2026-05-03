#include "cli-args.hpp"
#include "phos.hpp"

#include <cstdlib>
#include <fstream>
#include <print>

int main(int argc, char *argv[])
{
    if (argc == 1) {
        phos::cli::print_help();
        return 1;
    }

    auto [config, success, err_msg] = phos::cli::parse_args(argc, argv);

    if (!success) {
        std::println(std::cerr, "{}", err_msg);
        std::println(std::cerr, "Run 'phos --help' for usage.");
        return EXIT_FAILURE;
    }

    if (config.help) {
        phos::cli::print_help();
        return 0;
    }

    phos::Phos_engine engine;
    engine.vm.cfg.trace_execution = config.trace_vm;

    engine.vm.cfg.out = &std::cout;
    engine.vm.cfg.out = &std::cerr;

    if (!engine.compile_file(config.input_file)) {
        return EXIT_FAILURE;
    }

    if (config.cmd_ast_print) {
        std::println("{}", engine.dump_ast(config.ast_print_unicode));
    }

    if (config.print_ir) {
        std::ofstream out_file(config.ir_out_file);
        if (out_file.is_open()) {
            out_file << engine.dump_ir();
        }
        return 0;
    }

    engine.execute();

    return 0;
}
