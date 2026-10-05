#pragma once

#include "virtual_machine/garbage_collector/gc_heap.hpp"

#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

namespace phos {
struct Value;
struct Green_thread_data;
} // namespace phos
namespace phos::mem {
class Arena;
}

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

struct Vm_context
{
    static constexpr size_t FRAME_REGISTER_WINDOW = 256;
    static constexpr bool is_gc = true;

    Virtual_machine *machine = nullptr;
    gc::Gc_heap *heap = nullptr;
    mem::Arena *scratch = nullptr;
    phos::Green_thread_data *thread = nullptr;
    Call_frame *frame = nullptr;
    phos::Value *registers = nullptr;
    std::vector<phos::Value> *globals = nullptr;
    size_t *ip = nullptr;
    size_t *frame_base = nullptr;
    // Borrowed from Virtual_machine::cmd_args (never null at runtime).
    // A pointer avoids copying the whole argv vector on every Call/Return.
    const std::vector<std::string> *cmd_args = nullptr;

    [[nodiscard]] Root_guard protect(phos::Value *value) const
    {
        return Root_guard{*heap, value};
    }

    void *allocate_bytes(size_t payload_bytes, size_t /*alignment*/, uint8_t kind) const
    {
        return heap->allocate_bytes(payload_bytes, 0, kind);
    }

    template <typename T>
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

    // Method Declarations (Implementations moved to .cpp to fix incomplete types)
    [[nodiscard]] phos::Green_thread_data &current_thread() const noexcept;
    [[nodiscard]] Call_frame &current_frame() const noexcept;
    [[nodiscard]] std::vector<phos::Value> &global_values() const noexcept;
    [[nodiscard]] phos::Value &register_at(size_t slot) const noexcept;
    [[nodiscard]] size_t instruction_pointer() const noexcept;
    [[nodiscard]] size_t current_base() const noexcept;
};

} // namespace phos::vm
