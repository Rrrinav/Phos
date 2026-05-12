#pragma once

#include "gc_heap.hpp"
#include "virtual_machine/vm_context.hpp"

namespace phos::gc {

using Root_guard = vm::Root_guard;

// Full definition of Gc_context — a thin allocator/root facade over Gc_heap.
// value.cpp and virtual_machine.cpp call alloc<T>(), add_external_bytes(),
// and protect() on this type, so it must be complete wherever those templates
// are instantiated.
struct Gc_context
{
    Gc_heap &heap;

    explicit Gc_context(Gc_heap &h) : heap(h)
    {}

    template <typename T>
    [[nodiscard]] T *alloc(size_t payload_bytes, uint8_t kind) const
    {
        return static_cast<T *>(heap.alloc(payload_bytes, kind));
    }

    void add_external_bytes(size_t bytes) const
    {
        heap.add_external_bytes(bytes);
    }

    void remove_external_bytes(size_t bytes) const
    {
        heap.remove_external_bytes(bytes);
    }

    [[nodiscard]] Root_guard protect(phos::Value *v) const
    {
        return Root_guard{heap, v};
    }
};

} // namespace phos::gc
