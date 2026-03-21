#include "opcodes.hpp"

namespace phos::vm
{

std::string op_code_to_string(Op_code code)
{
    switch (code)
    {
        case Op_code::Constant:       return "Constant";
        case Op_code::Nil:            return "Nil";
        case Op_code::True:           return "True";
        case Op_code::False:          return "False";
        case Op_code::Pop:            return "Pop";
        case Op_code::Get_local:      return "Get_local";
        case Op_code::Set_local:      return "Set_local";
        case Op_code::Get_global:     return "Get_global";
        case Op_code::Set_global:     return "Set_global";
        case Op_code::Define_global:  return "Define_global";
        case Op_code::Get_upvalue:    return "Get_upvalue";
        case Op_code::Set_upvalue:    return "Set_upvalue";
        case Op_code::Jump:           return "Jump";
        case Op_code::Jump_if_false:  return "Jump_if_false";
        case Op_code::Jump_if_true:   return "Jump_if_true";
        case Op_code::Loop:           return "Loop";
        case Op_code::Jump_if_nil:    return "Jump_if_nil";
        case Op_code::Unwrap:         return "Unwrap";
        case Op_code::Add:            return "Add";
        case Op_code::Subtract:       return "Subtract";
        case Op_code::Multiply:       return "Multiply";
        case Op_code::Divide:         return "Divide";
        case Op_code::Modulo:         return "Modulo";
        case Op_code::Not:            return "Not";
        case Op_code::BitAnd:         return "BitAnd";
        case Op_code::BitOr:          return "BitOr";
        case Op_code::BitXor:         return "BitXor";
        case Op_code::BitNot:         return "BitNot";
        case Op_code::BitLShift:      return "BitLShift";
        case Op_code::BitRShift:      return "BitRshift";
        case Op_code::Negate:         return "Negate";
        case Op_code::Equal:          return "Equal";
        case Op_code::Not_equal:      return "Not_equal";
        case Op_code::Greater:        return "Greater";
        case Op_code::Greater_equal:  return "Greater_equal";
        case Op_code::Less:           return "Less";
        case Op_code::Less_equal:     return "Less_equal";
        case Op_code::Create_array:   return "Create_array";
        case Op_code::Get_index:      return "Get_index";
        case Op_code::Set_index:      return "Set_index";
        case Op_code::Get_field:      return "Get_field";
        case Op_code::Set_field:      return "Set_field";
        case Op_code::Unpack:         return "Unpack";
        case Op_code::Construct_model:return "Construct_model";
        case Op_code::Cast:           return "Cast";
        case Op_code::Construct_union:return "Construct_union";
        case Op_code::Match_variant:  return "Match_variant";
        case Op_code::Call:           return "Call";
        case Op_code::Tail_call:      return "Tail_call";
        case Op_code::Return:         return "Return";
        case Op_code::Make_closure:   return "Make_closure";
        case Op_code::Spawn:          return "Spawn";
        case Op_code::Yield:          return "Yield";
        case Op_code::Await:          return "Await";
        case Op_code::Print:          return "Print";
        case Op_code::Print_err:      return "Print_err";
        case Op_code::Halt:           return "Halt";
        default:                      return "unknown";
    }
}
}
