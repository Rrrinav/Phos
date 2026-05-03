#pragma once

#include "core/arena.hpp"
#include "core/value/value.hpp"

#include <cstdlib>
#include <format>
#include <functional>
#include <iostream>
#include <ostream>
#include <print>

namespace phos::vm {

class Virtual_machine
{
public:
    struct Config
    {
        std::ostream *out = &std::cout;
        std::ostream *err = &std::cerr;
        std::function<void(const std::string &)> panic_handler;
        bool trace_execution = false;
    };

    Config cfg;

private:
    mem::Arena &arena;

    // The templated inner loop.
    // The compiler will generate two versions of this function!
    template <bool Is_Tracing>
    void execute_loop(Green_thread_data *thread);

public:
    Virtual_machine(phos::mem::Arena &arena_) : arena(arena_)
    {
        cfg.panic_handler = [this](const std::string &s) {
            std::println(*this->cfg.err, "{}", s);
            std::exit(EXIT_FAILURE);
        };
    }

    ~Virtual_machine() = default;

    template <typename... Args>
    [[noreturn]] inline void panic(std::format_string<Args...> fmt, Args &&...args)
    {
        std::string message = std::format("[PANIC]: {}", std::format(fmt, std::forward<Args>(args)...));
        if (cfg.panic_handler) {
            cfg.panic_handler(message);
        }
        std::exit(EXIT_FAILURE);
    }

    // Public API: Checks the config flag exactly ONCE and routes to the correct optimized loop
    void execute(Green_thread_data *thread)
    {
        if (cfg.trace_execution) {
            execute_loop<true>(thread);
        } else {
            execute_loop<false>(thread);
        }
    }

    auto bini64_op(int64_t a, int64_t b, Opcode op) -> int64_t;
    auto binu64_op(uint64_t a, uint64_t b, Opcode op) -> uint64_t;
    auto binf64_op(double a, double b, Opcode op) -> double;
};

} // namespace phos::vm
