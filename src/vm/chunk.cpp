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
        // 3-byte instructions (Opcode + 2-byte offset)
        case Op_code::Jump:
        case Op_code::Jump_if_false:
        case Op_code::Jump_if_true:
            return jump_instruction(out, op_code_to_string(op), 1, offset);
        case Op_code::Loop:
            return jump_instruction(out, op_code_to_string(op), -1, offset);
        // 2 byte instructions (Opcode + 1-byte index)
        case Op_code::Constant:
        case Op_code::Define_global:
        case Op_code::Get_global:
        case Op_code::Set_global:
            return constant_instruction(out, op_code_to_string(op), offset);
        // 2 byte instructions (Opcode + 1-byte index)
        case Op_code::Get_local:
        case Op_code::Set_local:
            return local_instruction(out, op_code_to_string(op), offset);
        // 1-byte simple instructions
        case Op_code::Pop:
        case Op_code::Nil:
        case Op_code::Add:
        case Op_code::Subtract:
        case Op_code::Multiply:
        case Op_code::Divide:
        case Op_code::Modulo:
        case Op_code::Equal:
        case Op_code::Not_equal:
        case Op_code::Less:
        case Op_code::Less_equal:
        case Op_code::Greater:
        case Op_code::Greater_equal:
        case Op_code::Print:
        case Op_code::Return:
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

size_t Chunk::jump_instruction(std::ostream &out, const std::string &name, int sign, size_t offset)
{
    uint16_t jump = static_cast<uint16_t>(code[offset + 1] << 8);
    jump |= code[offset + 2];
    out << std::format("{:<20} {:>4} -> {}\n", name, offset, offset + 3 + sign * jump);
    return offset + 3;
}

size_t Chunk::local_instruction(std::ostream &out, const std::string &name, size_t offset)
{
    uint8_t slot = code[offset + 1];
    out << std::format("{:<20} {:>8}\n", name, slot);
    return offset + 2;
}

}  // namespace phos::vm
