#pragma once

#include "instruction.hpp"
#include "../value/value.hpp"
#include "../memory/arena.hpp"
#include <string>

namespace phos::vm {

class Assembler {
public:
    // Exports the Closure to deterministic text
    static std::string serialize(const Closure_data& closure);
    // Reads deterministic text back into executable VM memory
    static Closure_data deserialize(const std::string& ir_source, mem::Arena& arena);

    static std::string disassemble_instruction(Instruction inst, const Closure_data* closure = nullptr);
};

} // namespace phos::vm
