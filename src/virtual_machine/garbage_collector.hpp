// Tricolor, mark and sweep GC.
#pragma once

#include "core/value/value.hpp"
#include <vector>
#include <cstddef>

namespace phos::vm {

// Forward declarations
class VM;

// Assuming you have a base struct for heap allocations or can cast to one
// struct Obj { bool is_marked; Obj* next; ... };
struct Obj; 

class Garbage_collector {
public:
    explicit Garbage_collector(VM& vm);

    // Main entry point: triggers a full garbage collection cycle.
    void collect_garbage();

    // Registers a newly allocated object to be managed by the GC.
    void track_object(Obj* obj);

    // Configuration / Statistics
    size_t bytes_allocated() const { return bytes_allocated_; }
    void add_allocated_bytes(size_t bytes);
    void force_next_collection();

private:
    VM& vm_;
    
    // The "Gray Stack" for the Tricolor abstraction.
    // Objects here are marked (known to be alive) but their children haven't been traced yet.
    std::vector<Obj*> gray_stack_;
    
    // Intrusive linked list of all heap allocations currently active
    Obj* first_object_ = nullptr; 

    size_t bytes_allocated_ = 0;
    size_t next_gc_threshold_ = 1024 * 1024; // Starts at 1MB

    void mark_roots();
    void trace_references();
    void sweep();

    void mark_value(Value value);
    void mark_object(Obj* obj);
    void blacken_object(Obj* obj);
};

} // namespace phos::vm
