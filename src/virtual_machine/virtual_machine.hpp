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
#include <vector>

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

    // The three heaviest opcode handlers, extracted so execute_loop stays a
    // readable dispatch skeleton. Each takes the loop's cached frame state by
    // reference and leaves it ready for the next iteration; ip/base also flow
    // through ctx's pointers. always_inline keeps the call free even in debug
    // builds (single call site each); no behavior change vs the inline cases.
    __attribute__((always_inline)) inline void op_call(
        Instruction inst, Vm_context &ctx, Call_frame *&frame, const Instruction *&code, const Value *&constants, size_t &ip, size_t &base);
    // Returns true when the thread completed (caller must return from the loop).
    __attribute__((always_inline)) inline bool op_return(
        Instruction inst, Vm_context &ctx, Call_frame *&frame, const Instruction *&code, const Value *&constants, size_t &ip, size_t &base);
    __attribute__((always_inline)) inline void op_make_closure(Instruction inst, Vm_context &ctx, const Instruction *code);

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

    // Runs a single closure on a fresh interpreter thread (fresh call stack
    // plus register window). Shared by the file runner and the REPL, which
    // previously duplicated this bootstrap.
    static void run_closure(Virtual_machine &vm, Closure_data *closure)
    {
        constexpr size_t call_stack_capacity = 256;

        std::vector<Call_frame> frames(call_stack_capacity);
        frames[0] = Call_frame(closure, 0);

        std::vector<Value> thread_memory(call_stack_capacity * Virtual_machine::FRAME_REGISTER_WINDOW);

        Green_thread_data thread{};
        thread.call_stack = frames.data();
        thread.call_stack_count = 1;
        thread.call_stack_capacity = frames.size();
        thread.value_stack = thread_memory.data();
        thread.value_stack_capacity = thread_memory.size();
        thread.is_completed = false;

        vm.execute(&thread);
    }

    gc::Gc_heap &gc_ref() noexcept
    {
        return gc;
    }

    std::optional<types::Primitive_kind> cast_target_kind(Opcode op);
};

} // namespace phos::vm
