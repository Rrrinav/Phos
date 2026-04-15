#pragma once

#include <cstddef>

// Forward declarations so we don't have to include massive headers
namespace phos {
struct Closure_data;
}

namespace phos::vm {

struct Call_frame
{
    // The function we are currently executing
    // (This holds the instruction array and the constants pool!)
    phos::Closure_data *closure = nullptr;

    // The Instruction Pointer (IP)
    // Tracks exactly which 32-bit instruction we are about to execute
    std::size_t ip = 0;

    // The Register Window Base
    // Where this function's R0 starts in the Green Thread's register stack
    std::size_t frame_base = 0;

    Call_frame() = default;
    Call_frame(phos::Closure_data *cl, size_t base) : closure(cl), ip(0), frame_base(base)
    {}
};

} // namespace phos::vm
