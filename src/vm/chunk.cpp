#include "./chunk.hpp"

#include <iostream>

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
        out << "   | ";
    else
        out << std::format("{:4} ", locs[offset].l);

    uint8_t instruction = code[offset];
    auto op = static_cast<Op_code>(instruction);

    switch (op)
    {
        case Op_code::Constant:
            return constant_instruction(out, "Constant", offset);
        case Op_code::Add:
            return simple_instruction(out, "Add", offset);
        case Op_code::Subtract:
            return simple_instruction(out, "Subtract", offset);
        case Op_code::Multiply:
            return simple_instruction(out, "Multiply", offset);
        case Op_code::Divide:
            return simple_instruction(out, "Divide", offset);
        case Op_code::Print:
            return simple_instruction(out, "Print", offset);
        case Op_code::Return:
            return simple_instruction(out, "Return", offset);
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
    out << std::format("{:<16} {:>4} '", name, constant_idx);
    out << value_to_string(constants[constant_idx]) << "'\n";
    return offset + 2;
}

}  // namespace phos::vm
