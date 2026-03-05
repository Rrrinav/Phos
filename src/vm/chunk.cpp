#include "./chunk.hpp"

#include <iostream>
#include "opcodes.hpp"

namespace phos::vm
{

void Chunk::disassemble(std::ostream &out, const std::string &name)
{
    out << std::format("; {} \n", name);

    for (size_t offset = 0; offset < code.size();) offset = disassemble_instruction(out, offset);
}

size_t Chunk::disassemble_instruction(std::ostream &out, size_t offset)
{
    out << std::format("{:04} ", offset);

    if (offset > 0 && locs[offset].l == locs[offset - 1].l)
        out << "       | ";
    else
        out << std::format("{:>8} ", std::format("{}:{}", locs[offset].l, locs[offset].c));

    uint8_t instruction = code[offset];
    auto op = static_cast<Op_code>(instruction);

    switch (op)
    {
        case Op_code::Constant:
            return constant_instruction(out, "Constant", offset);
        case Op_code::Add:
        case Op_code::Subtract:
        case Op_code::Multiply:
        case Op_code::Divide:
        case Op_code::Print:
        case Op_code::Return:
        case Op_code::BitAnd:
        case Op_code::BitLShift:
        case Op_code::BitRShift:
        case Op_code::BitOr:
        case Op_code::Negate:
        case Op_code::BitNot:
        case Op_code::Not:
            return simple_instruction(out, op_code_to_string(op), offset);
        case Op_code::Halt:
            return simple_instruction(out, "Halt", offset);
        default:
            out << std::format("Unknown opcode {}\n", (int)instruction);
            return offset + 1;
    }
}

size_t Chunk::simple_instruction(std::ostream &out, const std::string &name, size_t offset)
{
    out << name << "\n";
    return offset + 1;
}

size_t Chunk::constant_instruction(std::ostream &out, const std::string &name, size_t offset)
{
    uint8_t constant_idx = code[offset + 1];
    out << std::format("{:<20} {:>8} '", name, constant_idx);
    out << value_to_string(constants[constant_idx]) << "'\n";
    return offset + 2;
}

}  // namespace phos::vm
