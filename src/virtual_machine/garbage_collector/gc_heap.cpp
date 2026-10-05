#include "gc_heap.hpp"

#include "core/value/value.hpp"

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <new>

namespace phos::gc {

void *Gc_heap::alloc(size_t payload_bytes, uint8_t kind)
{
    size_t total = sizeof(Gc_cell) + payload_bytes;

    size_t pool = pool_index(total);
    if (pool < sizeof(kPoolClasses) / sizeof(kPoolClasses[0])) {
        // Pooled sizes are class-rounded (not actual use): every block in a
        // pool is then guaranteed to fit every request its class serves.
        // bytes_allocated_ over-counts by at most the class granularity.
        size_t block = kPoolClasses[pool];
        if (pools_[pool] != nullptr) {
            Gc_cell *cell = pools_[pool];
            pools_[pool] = cell->next;
            cell->next = head_;
            cell->size = static_cast<uint32_t>(block);
            cell->color = Color::White;
            cell->kind = kind;
            head_ = cell;
            bytes_allocated_ += block;
            ++object_count_;
            return cell->payload();
        }

        void *raw = std::malloc(block);
        if (!raw) {
            throw std::bad_alloc{};
        }

        Gc_cell *cell = new (raw) Gc_cell{};
        cell->next = head_;
        cell->size = static_cast<uint32_t>(block);
        cell->color = Color::White;
        cell->kind = kind;

        head_ = cell;
        bytes_allocated_ += block;
        ++object_count_;

        return cell->payload();
    }

    void *raw = std::malloc(total);
    if (!raw) {
        throw std::bad_alloc{};
    }

    Gc_cell *cell = new (raw) Gc_cell{};
    cell->next = head_;
    cell->size = static_cast<uint32_t>(total);
    cell->color = Color::White;
    cell->kind = kind;

    head_ = cell;
    bytes_allocated_ += total;
    ++object_count_;

    return cell->payload();
}

void Gc_heap::collect(phos::Green_thread_data &thread, std::vector<phos::Value> &globals)
{
    mark_thread_values(thread);

    for (size_t i = 0; i < thread.call_stack_count; ++i) {
        if (thread.call_stack[i].closure != nullptr && !thread.call_stack[i].closure->is_prototype) {
            mark_cell(Gc_cell::from_payload(thread.call_stack[i].closure));
        }
    }

    phos::Upvalue_data *open_upvalue = thread.open_upvalues;
    while (open_upvalue != nullptr) {
        if (open_upvalue->location != nullptr) {
            mark_value(*open_upvalue->location);
        }
        mark_cell(Gc_cell::from_payload(open_upvalue));
        open_upvalue = open_upvalue->next;
    }

    for (auto &v : globals) {
        mark_value(v);
    }

    for (auto *vp : extra_roots_) {
        if (vp) {
            mark_value(*vp);
        }
    }

    trace_gray();

    sweep();

    threshold_ = static_cast<size_t>(static_cast<double>(bytes_allocated_) * 1.5);
    if (threshold_ < INITIAL_THRESHOLD) {
        threshold_ = INITIAL_THRESHOLD;
    }
}

void Gc_heap::mark_value(phos::Value &v)
{
    if (!v.is_gc()) {
        return;
    }

    void *payload = nullptr;
    switch (v.tag()) {
    case phos::Value_tag::String:
        payload = v.as_string_data();
        break;
    case phos::Value_tag::Array:
        payload = v.as_array();
        break;
    case phos::Value_tag::Model:
        payload = v.as_model();
        break;
    case phos::Value_tag::Union:
        payload = v.as_union();
        break;
    case phos::Value_tag::Closure:
        payload = v.as_closure();
        break;
    case phos::Value_tag::Iterator:
        payload = v.as_iterator();
        break;
    case phos::Value_tag::Green_thread:
        payload = v.as_green_thread();
        break;
    case phos::Value_tag::Upvalue:
        payload = v.as_upvalue();
        break;
    default:
        return;
    }

    if (!payload) {
        return;
    }
    mark_cell(Gc_cell::from_payload(payload));
}

void Gc_heap::mark_cell(Gc_cell *cell)
{
    if (cell->color != Color::White) {
        return;
    }
    cell->color = Color::Gray;
    gray_.push_back(cell);
}

void Gc_heap::mark_thread_values(phos::Green_thread_data &thread)
{
    for (size_t f = 0; f < thread.call_stack_count; ++f) {
        const auto &frame = thread.call_stack[f];
        if (frame.frame_base >= thread.value_stack_capacity) {
            continue;
        }
        size_t footprint = (frame.closure != nullptr) ? frame.closure->register_footprint : 0;
        size_t end = frame.frame_base + footprint;
        if (end > thread.value_stack_capacity) {
            end = thread.value_stack_capacity;
        }
        for (size_t i = frame.frame_base; i < end; ++i) {
            mark_value(thread.value_stack[i]);
        }
    }
}

void Gc_heap::trace_gray()
{
    while (!gray_.empty()) {
        Gc_cell *cell = gray_.back();
        gray_.pop_back();

        switch (static_cast<phos::Value_tag>(cell->kind)) {
        case phos::Value_tag::String:
            trace_string(cell);
            break;
        case phos::Value_tag::Array:
            trace_array(cell);
            break;
        case phos::Value_tag::Model:
            trace_model(cell);
            break;
        case phos::Value_tag::Union:
            trace_union_(cell);
            break;
        case phos::Value_tag::Closure:
            trace_closure(cell);
            break;
        case phos::Value_tag::Iterator:
            trace_iterator(cell);
            break;
        case phos::Value_tag::Green_thread:
            trace_thread(cell);
            break;
        case phos::Value_tag::Upvalue:
            trace_upvalue(cell);
            break;
        default:
            break;
        }

        cell->color = Color::Black;
    }
}

void Gc_heap::trace_string(Gc_cell * /*cell*/)
{}

void Gc_heap::trace_array(Gc_cell *cell)
{
    auto *arr = static_cast<phos::Array_data *>(cell->payload());
    // A separately allocated element buffer is its own GC cell: keep it alive
    // alongside the array (trailing storage needs no such handling).
    if (arr->elements_on_heap && arr->elements != nullptr) {
        mark_cell(Gc_cell::from_payload(arr->elements));
    }
    for (uint32_t i = 0; i < arr->count; ++i) {
        mark_value(arr->elements[i]);
    }
}

void Gc_heap::trace_model(Gc_cell *cell)
{
    auto *m = static_cast<phos::Model_data *>(cell->payload());
    if (m->fields_on_heap && m->fields != nullptr) {
        mark_cell(Gc_cell::from_payload(m->fields));
    }
    for (uint32_t i = 0; i < m->field_count; ++i) {
        mark_value(m->fields[i]);
    }
}

void Gc_heap::trace_union_(Gc_cell *cell)
{
    auto *u = static_cast<phos::Union_data *>(cell->payload());
    if (u->payload) {
        mark_value(*u->payload);
    }
}

void Gc_heap::trace_closure(Gc_cell *cell)
{
    auto *c = static_cast<phos::Closure_data *>(cell->payload());

    for (size_t i = 0; i < c->constant_count; ++i) {
        mark_value(c->constants[i]);
    }

    for (size_t i = 0; i < c->upvalue_count; ++i) {
        if (c->upvalues[i] && c->upvalues[i]->location) {
            mark_value(*c->upvalues[i]->location);
            mark_cell(Gc_cell::from_payload(c->upvalues[i]));
        }
    }
}

void Gc_heap::trace_iterator(Gc_cell *cell)
{
    auto *it = static_cast<phos::Iterator_data *>(cell->payload());
    mark_value(it->collection);
}

void Gc_heap::trace_thread(Gc_cell *cell)
{
    auto *gt = static_cast<phos::Green_thread_data *>(cell->payload());

    mark_thread_values(*gt);

    for (size_t i = 0; i < gt->call_stack_count; ++i) {
        if (gt->call_stack[i].closure && !gt->call_stack[i].closure->is_prototype) {
            mark_cell(Gc_cell::from_payload(gt->call_stack[i].closure));
        }
    }

    phos::Upvalue_data *uv = gt->open_upvalues;
    while (uv) {
        if (uv->location) {
            mark_value(*uv->location);
        }
        mark_cell(Gc_cell::from_payload(uv));
        uv = uv->next;
    }
}

void Gc_heap::trace_upvalue(Gc_cell *cell)
{
    auto *uv = static_cast<phos::Upvalue_data *>(cell->payload());
    if (uv->location) {
        mark_value(*uv->location);
    }
}

void Gc_heap::sweep()
{
    Gc_cell **cursor = &head_;
    while (*cursor) {
        Gc_cell *cell = *cursor;
        if (cell->color == Color::White) {
            *cursor = cell->next;
            bytes_allocated_ -= cell->size;
            --object_count_;
            size_t pool = pool_index(cell->size);
            if (pool < sizeof(kPoolClasses) / sizeof(kPoolClasses[0])) {
                cell->next = pools_[pool];
                pools_[pool] = cell;
            } else {
                std::free(cell);
            }
        } else {
            cell->color = Color::White;
            cursor = &cell->next;
        }
    }
}

void Gc_heap::destroy_all()
{
    Gc_cell *cell = head_;
    while (cell) {
        Gc_cell *next = cell->next;
        std::free(cell);
        cell = next;
    }
    for (auto *&pooled : pools_) {
        while (pooled) {
            Gc_cell *next = pooled->next;
            std::free(pooled);
            pooled = next;
        }
    }
    head_ = nullptr;
    bytes_allocated_ = 0;
    object_count_ = 0;
    threshold_ = INITIAL_THRESHOLD;
}

} // namespace phos::gc
