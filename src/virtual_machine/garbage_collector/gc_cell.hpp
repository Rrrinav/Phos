#pragma once

#include <cstdint>

namespace phos::gc {

// Every GC-managed heap object has this header prepended in memory.
// Layout in memory:  [ Gc_cell | payload (String_data / Array_data / ...) ]
//
// The Value struct stores only the payload pointer (same as before).
// Use cell_of(ptr) to get back to the header for marking.
enum class Color : uint8_t {
    White = 0, // Not yet reached — candidate for collection
    Gray = 1,  // Reached, children not yet traced
    Black = 2, // Fully traced — definitely live
};

struct Gc_cell
{
    Gc_cell *next = nullptr; // Intrusive linked list through all heap objects
    uint32_t size = 0;       // Total bytes: sizeof(Gc_cell) + payload bytes
    Color color = Color::White;
    uint8_t kind = 0; // Value_tag of the payload — used by tracer.
                      // Raw_buffer_kind (below) marks plain byte buffers with
                      // no traceable children; the tracer and sweeper skip it.
    uint16_t _pad = 0;
    static Gc_cell *from_payload(void *payload) noexcept
    {
        return static_cast<Gc_cell *>(payload) - 1;
    }
    void *payload() noexcept
    {
        return this + 1;
    }
    const void *payload() const noexcept
    {
        return this + 1;
    }
};

static_assert(sizeof(Gc_cell) == 16, "Gc_cell must be 16 bytes to keep payloads aligned");

// Cell kind for untraced byte buffers (grown array/model element storage).
// Deliberately outside the Value_tag range so every tag switch falls through
// to its default arm; the tracer marks these cells but never descends.
inline constexpr uint8_t kRawBufferKind = 0xFF;

} // namespace phos::gc
