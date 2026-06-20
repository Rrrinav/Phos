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

    Gc_heap() = default;
    ~Gc_heap()
    {
        destroy_all();
    }

    Gc_heap(const Gc_heap &) = delete;
    Gc_heap &operator=(const Gc_heap &) = delete;

    void *alloc(size_t payload_bytes, uint8_t kind);

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

    // --- Unified Tracking Interface ---
    void add_external_bytes(size_t bytes) noexcept
    {
        bytes_allocated_ += bytes;
    }
    void remove_external_bytes(size_t bytes) noexcept
    {
        bytes_allocated_ = (bytes_allocated_ > bytes) ? bytes_allocated_ - bytes : 0;
    }

private:
    Gc_cell *head_ = nullptr;
    size_t bytes_allocated_ = 0;
    size_t threshold_ = INITIAL_THRESHOLD;
    size_t object_count_ = 0;

    std::vector<Gc_cell *> gray_;
    std::vector<phos::Value *> extra_roots_;

    void mark_value(phos::Value &v);
    void mark_cell(Gc_cell *cell);
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
