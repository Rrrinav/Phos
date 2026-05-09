#pragma once

#include "gc_cell.hpp"

#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <vector>

// Forward declarations — we must not include value.hpp here to avoid circular deps.
// value.hpp includes gc_context.hpp, which includes gc_heap.hpp.
namespace phos {
struct Value;
struct String_data;
struct Array_data;
struct Model_data;
struct Union_data;
struct Closure_data;
struct Iterator_data;
struct Green_thread_data;
struct Upvalue_data;
} // namespace phos

namespace phos::gc {

class Gc_heap
{
public:
    // Start collecting when this many bytes are live on the GC heap.
    // The threshold auto-grows after each collection (x1.5) so long-running
    // programs don't thrash the GC.
    static constexpr size_t INITIAL_THRESHOLD = 1024 * 1024; // 1 MB

    Gc_heap() = default;
    ~Gc_heap()
    {
        destroy_all();
    }

    Gc_heap(const Gc_heap &) = delete;
    Gc_heap &operator=(const Gc_heap &) = delete;

    // Allocation
    // Allocate `payload_bytes` bytes of GC-managed memory.
    // Returns a pointer to the PAYLOAD region (what Value stores).
    // The Gc_cell header lives immediately before the returned pointer.
    // `kind` is the uint8_t cast of Value_tag — used by the tracer.
    void *alloc(size_t payload_bytes, uint8_t kind);

    // Root management
    //
    // The GC needs to know every Value* that is a root (reachable from C++
    // stack / native call).  The VM registers its register stack and globals
    // via collect(); native functions use push_root/pop_root via Gc_context.
    void push_root(phos::Value *v)
    {
        extra_roots_.push_back(v);
    }
    void pop_root() noexcept
    {
        extra_roots_.pop_back();
    }

    // Collection
    // Full stop-the-world collection.
    //
    // `thread`  — the currently executing thread state
    // `globals` — the VM's global Value vector
    //
    // Caller must ensure that all Values reachable only from C++ locals
    // (e.g. temporaries inside native functions) are registered via
    // push_root before calling.
    void collect(phos::Green_thread_data &thread, std::vector<phos::Value> &globals);

    // True when the heap is over threshold and should trigger a collection.
    bool needs_gc() const noexcept
    {
        return bytes_allocated_ >= threshold_;
    }

    size_t bytes_allocated() const noexcept
    {
        return bytes_allocated_;
    }
    size_t threshold() const noexcept
    {
        return threshold_;
    }
    size_t object_count() const noexcept
    {
        return object_count_;
    }

    // Track dynamically allocated memory inside objects (like Array_data::elements)
    void add_allocated_bytes(size_t bytes) noexcept
    {
        bytes_allocated_ += bytes;
    }
    void remove_allocated_bytes(size_t bytes) noexcept
    {
        bytes_allocated_ = (bytes_allocated_ > bytes) ? bytes_allocated_ - bytes : 0;
    }

private:
    Gc_cell *head_ = nullptr;
    size_t bytes_allocated_ = 0;
    size_t threshold_ = INITIAL_THRESHOLD;
    size_t object_count_ = 0;

    // Gray worklist — Gc_cells waiting for their children to be traced.
    std::vector<Gc_cell *> gray_;

    // Extra roots registered by native functions via Gc_context.
    std::vector<phos::Value *> extra_roots_;

    // Mark phase helpers
    void mark_value(phos::Value &v);
    void mark_cell(Gc_cell *cell); // White→Gray, push to worklist
    void trace_gray();             // Drain worklist: Gray→Black, mark children

    // Trace children of each object kind
    void trace_string(Gc_cell *cell);
    void trace_array(Gc_cell *cell);
    void trace_model(Gc_cell *cell);
    void trace_union_(Gc_cell *cell); // trailing _ avoids conflict with keyword
    void trace_closure(Gc_cell *cell);
    void trace_iterator(Gc_cell *cell);
    void trace_thread(Gc_cell *cell);
    void trace_upvalue(Gc_cell *cell);

    // Sweep phase
    void sweep();

    // Free every cell unconditionally (destructor path)
    void destroy_all();
};

} // namespace phos::gc
