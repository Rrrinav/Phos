#include "instruction.hpp"

#include <format>
#include <string_view>
#include <utility>

namespace {
constexpr std::pair<std::string_view, phos::vm::Opcode> kOpcodeTable[] = {
    {"Load_const", phos::vm::Opcode::Load_const},
    {"Load_nil", phos::vm::Opcode::Load_nil},
    {"Load_true", phos::vm::Opcode::Load_true},
    {"Load_false", phos::vm::Opcode::Load_false},
    {"Move", phos::vm::Opcode::Move},
    {"Load_global", phos::vm::Opcode::Load_global},
    {"Store_global", phos::vm::Opcode::Store_global},
    {"Concat_str", phos::vm::Opcode::Concat_str},
    {"Add_i64", phos::vm::Opcode::Add_i64},
    {"Sub_i64", phos::vm::Opcode::Sub_i64},
    {"Mul_i64", phos::vm::Opcode::Mul_i64},
    {"Div_i64", phos::vm::Opcode::Div_i64},
    {"Mod_i64", phos::vm::Opcode::Mod_i64},
    {"BitAnd_i64", phos::vm::Opcode::BitAnd_i64},
    {"BitOr_i64", phos::vm::Opcode::BitOr_i64},
    {"BitXor_i64", phos::vm::Opcode::BitXor_i64},
    {"Shl_i64", phos::vm::Opcode::Shl_i64},
    {"Shr_i64", phos::vm::Opcode::Shr_i64},
    {"Add_u64", phos::vm::Opcode::Add_u64},
    {"Sub_u64", phos::vm::Opcode::Sub_u64},
    {"Mul_u64", phos::vm::Opcode::Mul_u64},
    {"Div_u64", phos::vm::Opcode::Div_u64},
    {"Mod_u64", phos::vm::Opcode::Mod_u64},
    {"BitAnd_u64", phos::vm::Opcode::BitAnd_u64},
    {"BitOr_u64", phos::vm::Opcode::BitOr_u64},
    {"BitXor_u64", phos::vm::Opcode::BitXor_u64},
    {"Shl_u64", phos::vm::Opcode::Shl_u64},
    {"Shr_u64", phos::vm::Opcode::Shr_u64},
    {"Add_f64", phos::vm::Opcode::Add_f64},
    {"Sub_f64", phos::vm::Opcode::Sub_f64},
    {"Mul_f64", phos::vm::Opcode::Mul_f64},
    {"Div_f64", phos::vm::Opcode::Div_f64},
    {"Mod_f64", phos::vm::Opcode::Mod_f64},
    {"Cast_i8", phos::vm::Opcode::Cast_i8},
    {"Cast_i16", phos::vm::Opcode::Cast_i16},
    {"Cast_i32", phos::vm::Opcode::Cast_i32},
    {"Cast_i64", phos::vm::Opcode::Cast_i64},
    {"Cast_u8", phos::vm::Opcode::Cast_u8},
    {"Cast_u16", phos::vm::Opcode::Cast_u16},
    {"Cast_u32", phos::vm::Opcode::Cast_u32},
    {"Cast_u64", phos::vm::Opcode::Cast_u64},
    {"Cast_f16", phos::vm::Opcode::Cast_f16},
    {"Cast_f32", phos::vm::Opcode::Cast_f32},
    {"Cast_f64", phos::vm::Opcode::Cast_f64},
    {"Cast_str_to_arr", phos::vm::Opcode::Cast_str_to_arr},
    {"Cast_arr_to_str", phos::vm::Opcode::Cast_arr_to_str},
    {"Sat_cast_i8", phos::vm::Opcode::Sat_cast_i8},
    {"Sat_cast_i16", phos::vm::Opcode::Sat_cast_i16},
    {"Sat_cast_i32", phos::vm::Opcode::Sat_cast_i32},
    {"Sat_cast_i64", phos::vm::Opcode::Sat_cast_i64},
    {"Sat_cast_u8", phos::vm::Opcode::Sat_cast_u8},
    {"Sat_cast_u16", phos::vm::Opcode::Sat_cast_u16},
    {"Sat_cast_u32", phos::vm::Opcode::Sat_cast_u32},
    {"Sat_cast_u64", phos::vm::Opcode::Sat_cast_u64},
    {"Eq_i64", phos::vm::Opcode::Eq_i64},
    {"Neq_i64", phos::vm::Opcode::Neq_i64},
    {"Lt_i64", phos::vm::Opcode::Lt_i64},
    {"Lte_i64", phos::vm::Opcode::Lte_i64},
    {"Gt_i64", phos::vm::Opcode::Gt_i64},
    {"Gte_i64", phos::vm::Opcode::Gte_i64},
    {"Eq_u64", phos::vm::Opcode::Eq_u64},
    {"Neq_u64", phos::vm::Opcode::Neq_u64},
    {"Lt_u64", phos::vm::Opcode::Lt_u64},
    {"Lte_u64", phos::vm::Opcode::Lte_u64},
    {"Gt_u64", phos::vm::Opcode::Gt_u64},
    {"Gte_u64", phos::vm::Opcode::Gte_u64},
    {"Eq_f64", phos::vm::Opcode::Eq_f64},
    {"Neq_f64", phos::vm::Opcode::Neq_f64},
    {"Lt_f64", phos::vm::Opcode::Lt_f64},
    {"Lte_f64", phos::vm::Opcode::Lte_f64},
    {"Gt_f64", phos::vm::Opcode::Gt_f64},
    {"Gte_f64", phos::vm::Opcode::Gte_f64},
    {"Neg_i64", phos::vm::Opcode::Neg_i64},
    {"Neg_f64", phos::vm::Opcode::Neg_f64},
    {"Not", phos::vm::Opcode::Not},
    {"BitNot_i64", phos::vm::Opcode::BitNot_i64},
    {"BitNot_u64", phos::vm::Opcode::BitNot_u64},
    {"Print", phos::vm::Opcode::Print},
    {"Set_upvalue", phos::vm::Opcode::Set_upvalue},
    {"Get_upvalue", phos::vm::Opcode::Get_upvalue},
    {"Make_closure", phos::vm::Opcode::Make_closure},
    {"Jump", phos::vm::Opcode::Jump},
    {"Jump_if_false", phos::vm::Opcode::Jump_if_false},
    {"Unwrap_or", phos::vm::Opcode::Unwrap_or},
    {"Call", phos::vm::Opcode::Call},
    {"Return", phos::vm::Opcode::Return},
    {"Eq_str", phos::vm::Opcode::Eq_str},
    {"Neq_str", phos::vm::Opcode::Neq_str},
    {"Len", phos::vm::Opcode::Len},
    {"Make_array", phos::vm::Opcode::Make_array},
    {"Load_index", phos::vm::Opcode::Load_index},
    {"Store_index", phos::vm::Opcode::Store_index},
    {"Make_range_ex", phos::vm::Opcode::Make_range_ex},
    {"Make_range_in", phos::vm::Opcode::Make_range_in},
    {"Make_iter", phos::vm::Opcode::Make_iter},
    {"Iter_next", phos::vm::Opcode::Iter_next},
    {"Iter_prev", phos::vm::Opcode::Iter_prev},
    {"Make_model", phos::vm::Opcode::Make_model},
    {"Load_field", phos::vm::Opcode::Load_field},
    {"Store_field", phos::vm::Opcode::Store_field},
    {"Make_union", phos::vm::Opcode::Make_union},
    {"Test_union", phos::vm::Opcode::Test_union},
    {"Load_union_payload", phos::vm::Opcode::Load_union_payload},
    {"Wrap_option", phos::vm::Opcode::Wrap_option},
    {"Unwrap_option", phos::vm::Opcode::Unwrap_option},
    {"Test_nil", phos::vm::Opcode::Test_nil},
    {"Test_val", phos::vm::Opcode::Test_val},
    {"Panic", phos::vm::Opcode::Panic},
    {"None", phos::vm::Opcode::None},
};
} // namespace


std::string phos::vm::opcode_to_string(Opcode code)
{
    for (const auto &[name, op] : kOpcodeTable) {
        if (op == code) {
            return std::string(name);
        }
    }
    return std::format("UNKNOWN_{}", static_cast<uint8_t>(code));
}

phos::vm::Opcode phos::vm::string_to_opcode(std::string code)
{
    for (const auto &[name, op] : kOpcodeTable) {
        if (name == code) {
            return op;
        }
    }
    return Opcode::Return;
}
