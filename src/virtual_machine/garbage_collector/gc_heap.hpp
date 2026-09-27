#pragma once

#include "gc_cell.hpp"

#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <vector>

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
    static constexpr size_t INITIAL_THRESHOLD = 1024 * 1024; // 1 MB

    // Segregated free lists: swept cells below kMaxPooledSize are recycled by
    // size class instead of returned to malloc, so steady-state programs stop
    // calling malloc/free every cycle. Pooled blocks are always allocated at
    // exactly their class size, so any block in a pool fits any request that
    // maps to it. Larger cells still use exact-size malloc directly.
    static constexpr size_t kPoolClasses[] = {32, 64, 128, 256, 512};
    static constexpr size_t kMaxPooledSize = 512;

    Gc_heap() = default;
    ~Gc_heap()
    {
        destroy_all();
    }

    Gc_heap(const Gc_heap &) = delete;
    Gc_heap &operator=(const Gc_heap &) = delete;

    static constexpr bool is_gc = true;

    void *alloc(size_t payload_bytes, uint8_t kind);

    // Payload-sized allocation; the cell header is prepended by the heap.
    // Alignment is guaranteed to be at least alignof(Gc_cell) for the payload.
    void *allocate_bytes(size_t payload_bytes, size_t /*alignment*/, uint8_t kind)
    {
        return alloc(payload_bytes, kind);
    }

    void push_root(phos::Value *v)
    {
        extra_roots_.push_back(v);
    }
    void pop_root() noexcept
    {
        extra_roots_.pop_back();
    }

    void collect(phos::Green_thread_data &thread, std::vector<phos::Value> &globals);

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

private:
    Gc_cell *head_ = nullptr;
    size_t bytes_allocated_ = 0;
    size_t threshold_ = INITIAL_THRESHOLD;
    size_t object_count_ = 0;

    std::vector<Gc_cell *> gray_;
    std::vector<phos::Value *> extra_roots_;

    // Recycled cells by size class index into kPoolClasses (nullptr = empty).
    // Cells in these lists are NOT on the heap list and carry no accounting.
    Gc_cell *pools_[sizeof(kPoolClasses) / sizeof(kPoolClasses[0])] = {};

    static size_t pool_index(size_t total_bytes) noexcept
    {
        for (size_t i = 0; i < sizeof(kPoolClasses) / sizeof(kPoolClasses[0]); ++i) {
            if (total_bytes <= kPoolClasses[i]) {
                return i;
            }
        }
        return sizeof(kPoolClasses) / sizeof(kPoolClasses[0]); // too big: malloc
    }

    void mark_value(phos::Value &v);
    void mark_cell(Gc_cell *cell);
    // Marks only each frame's compiled register footprint instead of the
    // whole value stack, so dead slots above a frame's footprint are never
    // scanned (and never need wiping beyond the footprint either).
    void mark_thread_values(phos::Green_thread_data &thread);
    void trace_gray();

    void trace_string(Gc_cell *cell);
    void trace_array(Gc_cell *cell);
    void trace_model(Gc_cell *cell);
    void trace_union_(Gc_cell *cell);
    void trace_closure(Gc_cell *cell);
    void trace_iterator(Gc_cell *cell);
    void trace_thread(Gc_cell *cell);
    void trace_upvalue(Gc_cell *cell);

    void sweep();
    void destroy_all();
};

} // namespace phos::gc
