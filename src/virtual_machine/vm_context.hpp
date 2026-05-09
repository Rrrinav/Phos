#pragma once

#include "core/arena.hpp"
#include "core/value/value.hpp"
#include "virtual_machine/garbage_collector/gc_heap.hpp"

#include <cstddef>
#include <cstdint>
#include <limits>
#include <vector>

namespace phos::vm {

class Virtual_machine;
struct Call_frame;

struct Root_guard
{
    gc::Gc_heap *heap = nullptr;

    Root_guard() = default;
    Root_guard(gc::Gc_heap &h, phos::Value *v) : heap(&h)
    {
        heap->push_root(v);
    }
    ~Root_guard() noexcept
    {
        if (heap != nullptr) {
            heap->pop_root();
        }
    }

    Root_guard(const Root_guard &) = delete;
    Root_guard &operator=(const Root_guard &) = delete;

    Root_guard(Root_guard &&other) noexcept : heap(other.heap)
    {
        other.heap = nullptr;
    }
    Root_guard &operator=(Root_guard &&other) noexcept
    {
        if (this == &other) {
            return *this;
        }
        if (heap != nullptr) {
            heap->pop_root();
        }
        heap = other.heap;
        other.heap = nullptr;
        return *this;
    }
};

// VM-aware execution context passed to native functions.
//
// It exposes the GC heap and scratch arena like the old GC context, but it
// also carries the currently executing VM state so FFI can inspect and mutate
// runtime state intentionally when needed.
struct Vm_context
{
    static constexpr size_t FRAME_REGISTER_WINDOW = static_cast<size_t>(std::numeric_limits<uint8_t>::max()) + 1;

    Virtual_machine *machine = nullptr;
    gc::Gc_heap *heap = nullptr;
    mem::Arena *scratch = nullptr;
    phos::Green_thread_data *thread = nullptr;
    Call_frame *frame = nullptr;
    phos::Value *registers = nullptr;
    std::vector<phos::Value> *globals = nullptr;
    size_t *ip = nullptr;
    size_t *frame_base = nullptr;

    [[nodiscard]] Root_guard protect(phos::Value *value) const
    {
        return Root_guard{*heap, value};
    }

    template<typename T>
    [[nodiscard]] T *alloc(size_t payload_bytes, uint8_t kind) const
    {
        return static_cast<T *>(heap->alloc(payload_bytes, kind));
    }

    [[nodiscard]] Virtual_machine &vm() const noexcept
    {
        return *machine;
    }

    [[nodiscard]] gc::Gc_heap &gc_heap() const noexcept
    {
        return *heap;
    }

    [[nodiscard]] mem::Arena &scratch_arena() const noexcept
    {
        return *scratch;
    }

    [[nodiscard]] phos::Green_thread_data &current_thread() const noexcept
    {
        return *thread;
    }

    [[nodiscard]] Call_frame &current_frame() const noexcept
    {
        return *frame;
    }

    [[nodiscard]] std::vector<phos::Value> &global_values() const noexcept
    {
        return *globals;
    }

    [[nodiscard]] phos::Value &register_at(size_t slot) const noexcept
    {
        return registers[*frame_base + slot];
    }

    [[nodiscard]] size_t instruction_pointer() const noexcept
    {
        return *ip;
    }

    [[nodiscard]] size_t current_base() const noexcept
    {
        return *frame_base;
    }

    [[nodiscard]] size_t active_stack_limit() const noexcept
    {
        const size_t live_limit = *frame_base + FRAME_REGISTER_WINDOW;
        return (live_limit < thread->value_stack_capacity) ? live_limit : thread->value_stack_capacity;
    }

    [[nodiscard]] bool should_collect() const noexcept
    {
        return heap->needs_gc();
    }

    void collect_garbage() const
    {
        frame->ip = *ip;
        thread->live_value_count = active_stack_limit();
        heap->collect(*thread, *globals);
    }
};

} // namespace phos::vm
