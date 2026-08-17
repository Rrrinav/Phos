#pragma once

#include "core/arena.hpp"
#include "core/value/value.hpp"
#include "virtual_machine/garbage_collector/gc_heap.hpp"
#include "virtual_machine/vm_context.hpp"

#include <cstdlib>
#include <format>
#include <functional>
#include <iostream>
#include <optional>
#include <ostream>
#include <print>

namespace phos::vm {

class Virtual_machine
{
public:
    static constexpr size_t FRAME_REGISTER_WINDOW = vm::Vm_context::FRAME_REGISTER_WINDOW;

    struct Config
    {
        std::ostream *out = &std::cout;
        std::ostream *err = &std::cerr;
        std::function<void(const std::string &)> panic_handler;
        bool trace_execution = false;
    };

    Config cfg;

private:
    gc::Gc_heap &gc;
    mem::Arena &arena;

    // The templated inner loop.
    // The compiler will generate two versions of this function!
    template <bool Is_Tracing>
    void execute_loop(Green_thread_data *thread);

public:
    std::vector<std::string> cmd_args{};
    std::vector<Value> globals;

    Virtual_machine(gc::Gc_heap &gc_, phos::mem::Arena &arena_) : gc(gc_), arena(arena_)
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

    gc::Gc_heap &gc_ref() noexcept
    {
        return gc;
    }

    // Per-type-family opcode helpers. The arithmetic and comparison families
    // are contiguous in the Opcode enum, so each family is one template.
    template <typename T>
    T binary_op(T a, T b, Opcode op, Opcode family_base);

    template <typename T>
    bool compare_op(T a, T b, Opcode op, Opcode family_base);

    std::optional<types::Primitive_kind> cast_target_kind(Opcode op);
};

} // namespace phos::vm
