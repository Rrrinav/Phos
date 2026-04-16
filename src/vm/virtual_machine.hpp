#pragma once

#include "../memory/arena.hpp"
#include "../value/value.hpp"

#include <cstdlib>
#include <format>
#include <functional>
#include <iostream>
#include <ostream>
#include <print>

namespace phos::vm {

class Virtual_machine
{
    mem::Arena &arena;

public:
    // Boot with the Arena
    Virtual_machine(phos::mem::Arena &arena_) : arena(arena_)
    {
        cfg.panic_handler = [this](const std::string &s) {
            std::println(this->cfg.err, "{}", s);
            std::exit(EXIT_FAILURE);
        };
    }

    ~Virtual_machine() = default;

    // The main execution loop. It takes a Green Thread and runs it.
    auto execute(Green_thread_data *thread) -> void;

    auto bini64_op(int64_t a, int64_t b, Opcode op) -> int64_t;
    auto binu64_op(uint64_t a, uint64_t b, Opcode op) -> uint64_t;
    auto binf64_op(double a, double b, Opcode op) -> double;

public:
    struct Config
    {
        std::ostream &out = std::cout;
        std::ostream &err = std::cerr;
        bool trace_execution = false;
        std::function<void(const std::string &)> panic_handler;
    };

    Config cfg;

    template <typename... Args>
    [[noreturn]] inline void panic(std::format_string<Args...> fmt, Args &&...args)
    {
        std::string message = std::format("[PANIC]: {}", std::format(fmt, std::forward<Args>(args)...));

        if (cfg.panic_handler) {
            cfg.panic_handler(message);
        }
        // Fallback safety: A panic function is marked [[noreturn]] and must never return.
        // If a user provides a custom handler that doesn't throw or exit, we catch it here.
        std::exit(EXIT_FAILURE);
    }
};

} // namespace phos::vm
