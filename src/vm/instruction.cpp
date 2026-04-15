#include "instruction.hpp"

std::string phos::vm::opcode_to_string(Opcode code)
{
    switch (code) {
    case Opcode::Load_const:    return "Load_const";
    case Opcode::Load_nil:      return "Load_nil";
    case Opcode::Load_true:     return "Load_true";
    case Opcode::Load_false:    return "Load_false";
    case Opcode::Move:          return "Move";
    case Opcode::Add_i64:       return "Add_i64";
    case Opcode::Add_u64:       return "Add_u64";
    case Opcode::Add_f64:       return "Add_f64";
    case Opcode::Jump:          return "Jump";
    case Opcode::Jump_if_false: return "Jump_if_false";
    case Opcode::Unwrap_or:     return "Unwrap_or";
    case Opcode::Call:          return "Call";
    case Opcode::Return:        return "Return";
    default: return "unknown opcde";
    }
};
