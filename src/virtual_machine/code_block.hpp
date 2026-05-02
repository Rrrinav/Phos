#pragma once

#include "core/instruction/instruction.hpp"
#include "core/value/value.hpp"

#include <cstdint>
#include <vector>

namespace phos::vm {

struct Code_block
{
    std::vector<Instruction> instructions;
    std::vector<Value> constants;

    size_t emit(Instruction inst)
    {
        instructions.push_back(inst);
        return instructions.size() - 1;
    }

    uint16_t add_constant(Value val)
    {
        constants.push_back(val);
        return static_cast<uint16_t>(constants.size() - 1);
    }
};

} // namespace phos::vm
