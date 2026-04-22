#include "instruction.hpp"

#include <format>

std::string phos::vm::opcode_to_string(Opcode code)
{
    switch (code) {
    case Opcode::Load_const:
        return "Load_const";
    case Opcode::Load_nil:
        return "Load_nil";
    case Opcode::Load_true:
        return "Load_true";
    case Opcode::Load_false:
        return "Load_false";
    case Opcode::Move:
        return "Move";
    case Opcode::Add_i64:
        return "Add_i64";
    case Opcode::Add_u64:
        return "Add_u64";
    case Opcode::Add_f64:
        return "Add_f64";
    case Opcode::Sub_i64:
        return "Sub_i64";
    case Opcode::Sub_u64:
        return "Sub_u64";
    case Opcode::Sub_f64:
        return "Sub_f64";
    case Opcode::Mul_i64:
        return "Mul_i64";
    case Opcode::Mul_u64:
        return "Mul_u64";
    case Opcode::Mul_f64:
        return "Mul_f64";
    case Opcode::Div_i64:
        return "Div_i64";
    case Opcode::Div_u64:
        return "Div_u64";
    case Opcode::Div_f64:
        return "Div_f64";
    case Opcode::Mod_i64:
        return "Mod_i64";
    case Opcode::Mod_u64:
        return "Mod_u64";
    case Opcode::Mod_f64:
        return "Mod_f64";
    case Opcode::Cast_i8:
        return "Cast_i8";
    case Opcode::Cast_i16:
        return "Cast_i16";
    case Opcode::Cast_i32:
        return "Cast_i32";
    case Opcode::Cast_i64:
        return "Cast_i64";
    case Opcode::Cast_u8:
        return "Cast_u8";
    case Opcode::Cast_u16:
        return "Cast_u16";
    case Opcode::Cast_u32:
        return "Cast_u32";
    case Opcode::Cast_u64:
        return "Cast_u64";
    case Opcode::Cast_f16:
        return "Cast_f16";
    case Opcode::Cast_f32:
        return "Cast_f32";
    case Opcode::Cast_f64:
        return "Cast_f64";
    case Opcode::Eq_i64:
        return "Eq_i64";
    case Opcode::Neq_i64:
        return "Neq_i64";
    case Opcode::Lt_i64:
        return "Lt_i64";
    case Opcode::Lte_i64:
        return "Lte_i64";
    case Opcode::Gt_i64:
        return "Gt_i64";
    case Opcode::Gte_i64:
        return "Gte_i64";
    case Opcode::Eq_u64:
        return "Eq_u64";
    case Opcode::Neq_u64:
        return "Neq_u64";
    case Opcode::Lt_u64:
        return "Lt_u64";
    case Opcode::Lte_u64:
        return "Lte_u64";
    case Opcode::Gt_u64:
        return "Gt_u64";
    case Opcode::Gte_u64:
        return "Gte_u64";
    case Opcode::Eq_f64:
        return "Eq_f64";
    case Opcode::Neq_f64:
        return "Neq_f64";
    case Opcode::Lt_f64:
        return "Lt_f64";
    case Opcode::Lte_f64:
        return "Lte_f64";
    case Opcode::Gt_f64:
        return "Gt_f64";
    case Opcode::Gte_f64:
        return "Gte_f64";
    case Opcode::Neg_i64:
        return "Neg_i64";
    case Opcode::Neg_f64:
        return "Neg_f64";
    case Opcode::Not:
        return "Not";
    case Opcode::BitNot_i64:
        return "BitNot_i64";
    case Opcode::BitNot_u64:
        return "BitNot_u64";
    case Opcode::Print:
        return "Print";
    case Opcode::Set_upvalue:
        return "Set_upvalue";
    case Opcode::Get_upvalue:
        return "Get_upvalue";
    case Opcode::Make_closure:
        return "Make_closure";
    case Opcode::Jump:
        return "Jump";
    case Opcode::Jump_if_false:
        return "Jump_if_false";
    case Opcode::Unwrap_or:
        return "Unwrap_or";
    case Opcode::Call:
        return "Call";
    case Opcode::Return:
        return "Return";
    case Opcode::Make_array:
        return "Make_array";
    case Opcode::Load_index:
        return "Load_index";
    case Opcode::Store_index:
        return "Store_index";
    case Opcode::Make_model:
        return "Make_model";
    case Opcode::Load_field:
        return "Load_field";
    case Opcode::Store_field:
        return "Store_field";
    case Opcode::Make_union:
        return "Make_union";
    case Opcode::Test_union:
        return "Test_union";
    case Opcode::Load_union_payload:
        return "Ld_union_payload";
    case Opcode::None:
        return "None";
    }
    return std::format("UNKNOWN_{}", static_cast<uint8_t>(code));
};

phos::vm::Opcode phos::vm::string_to_opcode(std::string code)
{
    if (code == "Load_const") {
        return Opcode::Load_const;
    }
    if (code == "Load_nil") {
        return Opcode::Load_nil;
    }
    if (code == "Load_true") {
        return Opcode::Load_true;
    }
    if (code == "Load_false") {
        return Opcode::Load_false;
    }
    if (code == "Move") {
        return Opcode::Move;
    }
    if (code == "Add_i64") {
        return Opcode::Add_i64;
    }
    if (code == "Add_u64") {
        return Opcode::Add_u64;
    }
    if (code == "Add_f64") {
        return Opcode::Add_f64;
    }
    if (code == "Sub_i64") {
        return Opcode::Sub_i64;
    }
    if (code == "Sub_u64") {
        return Opcode::Sub_u64;
    }
    if (code == "Sub_f64") {
        return Opcode::Sub_f64;
    }
    if (code == "Mul_i64") {
        return Opcode::Mul_i64;
    }
    if (code == "Mul_u64") {
        return Opcode::Mul_u64;
    }
    if (code == "Mul_f64") {
        return Opcode::Mul_f64;
    }
    if (code == "Div_i64") {
        return Opcode::Div_i64;
    }
    if (code == "Div_u64") {
        return Opcode::Div_u64;
    }
    if (code == "Div_f64") {
        return Opcode::Div_f64;
    }
    if (code == "Mod_i64") {
        return Opcode::Mod_i64;
    }
    if (code == "Mod_u64") {
        return Opcode::Mod_u64;
    }
    if (code == "Mod_f64") {
        return Opcode::Mod_f64;
    }
    if (code == "Cast_i8") {
        return Opcode::Cast_i8;
    }
    if (code == "Cast_i16") {
        return Opcode::Cast_i16;
    }
    if (code == "Cast_i32") {
        return Opcode::Cast_i32;
    }
    if (code == "Cast_i64") {
        return Opcode::Cast_i64;
    }
    if (code == "Cast_u8") {
        return Opcode::Cast_u8;
    }
    if (code == "Cast_u16") {
        return Opcode::Cast_u16;
    }
    if (code == "Cast_u32") {
        return Opcode::Cast_u32;
    }
    if (code == "Cast_u64") {
        return Opcode::Cast_u64;
    }
    if (code == "Cast_f16") {
        return Opcode::Cast_f16;
    }
    if (code == "Cast_f32") {
        return Opcode::Cast_f32;
    }
    if (code == "Cast_f64") {
        return Opcode::Cast_f64;
    }
    if (code == "Eq_i64") {
        return Opcode::Eq_i64;
    }
    if (code == "Neq_i64") {
        return Opcode::Neq_i64;
    }
    if (code == "Lt_i64") {
        return Opcode::Lt_i64;
    }
    if (code == "Lte_i64") {
        return Opcode::Lte_i64;
    }
    if (code == "Gt_i64") {
        return Opcode::Gt_i64;
    }
    if (code == "Gte_i64") {
        return Opcode::Gte_i64;
    }
    if (code == "Eq_u64") {
        return Opcode::Eq_u64;
    }
    if (code == "Neq_u64") {
        return Opcode::Neq_u64;
    }
    if (code == "Lt_u64") {
        return Opcode::Lt_u64;
    }
    if (code == "Lte_u64") {
        return Opcode::Lte_u64;
    }
    if (code == "Gt_u64") {
        return Opcode::Gt_u64;
    }
    if (code == "Gte_u64") {
        return Opcode::Gte_u64;
    }
    if (code == "Eq_f64") {
        return Opcode::Eq_f64;
    }
    if (code == "Neq_f64") {
        return Opcode::Neq_f64;
    }
    if (code == "Lt_f64") {
        return Opcode::Lt_f64;
    }
    if (code == "Lte_f64") {
        return Opcode::Lte_f64;
    }
    if (code == "Gt_f64") {
        return Opcode::Gt_f64;
    }
    if (code == "Gte_f64") {
        return Opcode::Gte_f64;
    }
    if (code == "Neg_i64") {
        return Opcode::Neg_i64;
    }
    if (code == "Neg_f64") {
        return Opcode::Neg_f64;
    }
    if (code == "Not") {
        return Opcode::Not;
    }
    if (code == "BitNot_i64") {
        return Opcode::BitNot_i64;
    }
    if (code == "BitNot_u64") {
        return Opcode::BitNot_u64;
    }
    if (code == "Print") {
        return Opcode::Print;
    }
    if (code == "Set_upvalue") {
        return Opcode::Set_upvalue;
    }
    if (code == "Get_upvalue") {
        return Opcode::Get_upvalue;
    }
    if (code == "Make_closure") {
        return Opcode::Make_closure;
    }
    if (code == "Jump") {
        return Opcode::Jump;
    }
    if (code == "Jump_if_false") {
        return Opcode::Jump_if_false;
    }
    if (code == "Unwrap_or") {
        return Opcode::Unwrap_or;
    }
    if (code == "Call") {
        return Opcode::Call;
    }
    if (code == "Return") {
        return Opcode::Return;
    }
    if (code == "Make_array") {
        return Opcode::Make_array;
    }
    if (code == "Load_index") {
        return Opcode::Load_index;
    }
    if (code == "Store_index") {
        return Opcode::Store_index;
    }
    if (code == "Make_model") {
        return Opcode::Make_model;
    }
    if (code == "Load_field") {
        return Opcode::Load_field;
    }
    if (code == "Store_field") {
        return Opcode::Store_field;
    }
    if (code == "Make_union") {
        return Opcode::Make_union;
    }
    if (code == "Test_union") {
        return Opcode::Test_union;
    }
    if (code == "Ld_union_payload") {
        return Opcode::Load_union_payload;
    }
    if (code == "None") {
        return Opcode::None;
    }
    return Opcode::Return;
}
