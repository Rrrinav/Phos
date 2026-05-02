#pragma once

#include "core/arena.hpp"
#include "core/value/value.hpp"
#include "core/instruction/instruction.hpp"

#include <string>

namespace phos::vm {

class Assembler
{
public:
    // Serializes a Closure_data object into a deterministic, human-readable text format.
    // Handles string escaping automatically to ensure safe multiline strings.
    static std::string serialize(const Closure_data &closure);

    // Reads deterministic text back into executable VM memory using the provided Arena.
    // Automatically unescapes strings and links 24-bit jump instructions.
    static Closure_data deserialize(const std::string &ir_source, mem::Arena &arena);

    // Converts a single 32-bit Instruction into an LLVM-style assembly string.
    // If a closure is provided, it will resolve constant pool indices ($K) into inline comments.
    static std::string disassemble_instruction(Instruction inst, const Closure_data *closure = nullptr);
};

} // namespace phos::vm
