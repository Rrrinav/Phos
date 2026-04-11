#include "./chunk.hpp"

#include "opcodes.hpp"

#include <algorithm>
#include <cctype>
#include <format>
#include <iostream>

namespace phos::vm {

static std::string to_mnemonic(Op_code op)
{
    std::string s = op_code_to_string(op);
    std::transform(s.begin(), s.end(), s.begin(), ::toupper);
    return s;
}

std::string Chunk::format_instruction(size_t offset, const std::string &mnemonic, const std::string &operands, const std::string &comment)
{
    auto loc = offset < locs.size() ? locs[offset] : ast::Source_location{0, 0};
    return std::format("0x{:04X}  {:<18} {:<16} ; {} [{}:{}]\n", offset, mnemonic, operands, comment, loc.l, loc.c);
}

void Chunk::disassemble(std::ostream &out, const std::string &name)
{
    out << std::format("\n=== {} ===\n", name);
    for (size_t offset = 0; offset < code.size();) {
        offset = disassemble_instruction(out, offset);
    }
    out << "\n";
}

size_t Chunk::disassemble_instruction(std::ostream &out, size_t offset)
{
    uint8_t instruction = code[offset];
    auto op = static_cast<Op_code>(instruction);
    std::string mnemonic = to_mnemonic(op);

    switch (op) {
    case Op_code::Jump:
    case Op_code::Jump_if_false:
    case Op_code::Jump_if_true:
        return jump_instruction(out, mnemonic, 1, offset);
    case Op_code::Loop:
        return jump_instruction(out, mnemonic, -1, offset);

    case Op_code::Constant:
    case Op_code::Define_global:
    case Op_code::Get_global:
    case Op_code::Set_global:
        return constant_instruction(out, mnemonic, offset);

    case Op_code::Make_closure: {
        uint8_t constant_idx = code[offset + 1];
        auto closure = get_closure(constants[constant_idx]);
        out << format_instruction(offset, mnemonic, std::format("#{}", constant_idx), closure->name);
        offset += 2;

        // Disassemble the routing bytes visually!
        for (size_t i = 0; i < closure->upvalue_count; i++) {
            uint8_t is_local = code[offset++];
            uint8_t index = code[offset++];
            out << format_instruction(offset - 2, ".route", std::format("{} {}", is_local, index), "upvalue capture");
        }
        return offset;
    }

    case Op_code::Construct_model: {
        uint8_t field_count = code[offset + 1];
        uint8_t name_idx = code[offset + 2];
        out << format_instruction(offset, mnemonic, std::format("{} #{}", field_count, name_idx), "");
        return offset + 3;
    }
    case Op_code::Construct_union: {
        uint8_t u_idx = code[offset + 1];
        uint8_t v_idx = code[offset + 2];
        out << format_instruction(offset, mnemonic, std::format("#{} #{}", u_idx, v_idx), "");
        return offset + 3;
    }
    case Op_code::Match_variant: {
        uint8_t v_idx = code[offset + 1];
        out << format_instruction(offset, mnemonic, std::format("#{}", v_idx), "");
        return offset + 2;
    }
    case Op_code::Call: {
        uint8_t arg_count = code[offset + 1];
        out << format_instruction(offset, mnemonic, std::format("{}", arg_count), "args");
        return offset + 2;
    }

    case Op_code::Get_field:
    case Op_code::Set_field:
    case Op_code::Get_local:
    case Op_code::Set_local:
    case Op_code::Get_upvalue:
    case Op_code::Set_upvalue:
        return local_instruction(out, mnemonic, offset);

    case Op_code::Pop:
    case Op_code::Nil:
    case Op_code::True:
    case Op_code::False:
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
    case Op_code::Print_err:
    case Op_code::Return:
        return simple_instruction(out, mnemonic, offset);
    case Op_code::Halt:
        return simple_instruction(out, "HALT", offset);
    default:
        out << std::format("0x{:04X}  UNKNOWN_{} \n", offset, (int)instruction);
        return offset + 1;
    }
}

size_t Chunk::simple_instruction(std::ostream &out, const std::string &mnemonic, size_t offset)
{
    out << format_instruction(offset, mnemonic, "", "");
    return offset + 1;
}

size_t Chunk::constant_instruction(std::ostream &out, const std::string &mnemonic, size_t offset)
{
    uint8_t constant_idx = code[offset + 1];
    std::string val_str = value_to_string(constants[constant_idx]);
    if (val_str.length() > 30) {
        val_str = val_str.substr(0, 27) + "...";
    }
    out << format_instruction(offset, mnemonic, std::format("#{}", constant_idx), "'" + val_str + "'");
    return offset + 2;
}

size_t Chunk::jump_instruction(std::ostream &out, const std::string &mnemonic, int sign, size_t offset)
{
    uint16_t jump = static_cast<uint16_t>(code[offset + 1] << 8) | code[offset + 2];
    out << format_instruction(offset, mnemonic, std::format("{}", jump), "bytes");
    return offset + 3;
}

size_t Chunk::local_instruction(std::ostream &out, const std::string &mnemonic, size_t offset)
{
    uint8_t slot = code[offset + 1];
    out << format_instruction(offset, mnemonic, std::format("%{}", slot), "");
    return offset + 2;
}

} // namespace phos::vm
