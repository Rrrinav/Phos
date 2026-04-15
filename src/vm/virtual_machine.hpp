#pragma once

#include "../memory/arena.hpp"
#include "../value/value.hpp"

#include <format>
#include <iostream>

namespace phos::vm {

class Virtual_machine
{
private:
    // The central memory manager for all heap objects
    mem::Arena arena;

public:
    // Boot with a 10MB slab for testing
    Virtual_machine() : arena()
    {}
    ~Virtual_machine() = default;

    // Halts execution and prints a formatted trace
    template <typename... Args>
    [[noreturn]] inline void panic(std::format_string<Args...> fmt, Args &&...args)
    {
        std::string message = std::format(fmt, std::forward<Args>(args)...);

        std::cerr << "\n[PANIC] " << message << "\n\n";

        std::exit(EXIT_FAILURE);
    }

    // The main execution loop. It takes a Green Thread and runs it.
    auto execute(Green_thread_data *thread) -> void;

    auto bini64_op(int64_t a, int64_t b, Opcode op) -> int64_t;
    auto binu64_op(uint64_t a, uint64_t b, Opcode op) -> uint64_t;
    auto binf64_op(double a, double b, Opcode op) -> double;
};

} // namespace phos::vm
