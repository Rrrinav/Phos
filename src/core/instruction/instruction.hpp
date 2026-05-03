#pragma once

/*
Phos VM - 32-bit Instruction Encoding
Each instruction is exactly 4 bytes.

Concepts:
- R(x): value stored in register x (runtime storage)
- K[x]: value stored in constant table (read-only, compile-time data)

Instruction formats:

1) RRR (3 registers)
   [ opcode | dst | src_a | src_b ]
   Example:
       Add_i64 r0, r1, r2
       => R(0) = R(1) + R(2)

2) RI (register + unsigned immediate)
   [ opcode | dst | imm_u16 ]
   Example:
       Load_const r0, 5
       => R(0) = K[5]

3) RS (register + signed immediate)
   [ opcode | dst | imm_s16 ]
   Used for relative jumps:
       Jump_if_false r0, -2
       => if (!R(0)) IP += -2

Notes:
- All jumps are relative to the instruction pointer (IP)
- Registers are virtual and managed by the VM
*/

#include <cstdint>
#include <string>

namespace phos::vm {

// Opcodes define behavior; encoding is handled separately
enum class Opcode : uint8_t {
    // Constants / Moves
    Load_const, // dst, imm_u16      : R(dst) = K[imm]
    Load_nil,   // dst               : R(dst) = Nil
    Load_true,  // dst               : R(dst) = True
    Load_false, // dst               : R(dst) = False
    Move,       // dst, src_a        : R(dst) = R(src_a)

    Load_global, Store_global,

    // Arithmetic
    Add_i64, Add_u64, Add_f64, Sub_i64, Sub_u64, Sub_f64, // dst, src_a, src_b
    Mul_i64, Mul_u64, Mul_f64, Div_i64, Div_u64, Div_f64, // dst, src_a, src_b

    Mod_i64, Mod_u64, Mod_f64, // dst, src_a, src_b

    // Casts
    Cast_i8, Cast_i16, Cast_i32, Cast_i64, Cast_u8,
    Cast_u16, Cast_u32, Cast_u64, Cast_f16, Cast_f32, Cast_f64,
    Cast_str_to_arr, // dst, src_a (string), src_b (1 for signed i8, 0 for unsigned u8)
    Cast_arr_to_str, // dst, src_a (array), src_b (unused)

    // Comparison
    Eq_i64, Neq_i64, Lt_i64, Lte_i64, Gt_i64, Gte_i64, // dest, src_a, src_b
    Eq_u64, Neq_u64, Lt_u64, Lte_u64, Gt_u64, Gte_u64, // dest, src_a, src_b
    Eq_f64, Neq_f64, Lt_f64, Lte_f64, Gt_f64, Gte_f64, // dest, src_a, src_b
    BitAnd_i64, BitOr_i64, BitXor_i64, Shl_i64, Shr_i64,
    BitAnd_u64, BitOr_u64, BitXor_u64, Shl_u64, Shr_u64,

    // Unary
    Neg_i64, Neg_f64, Not, BitNot_i64, BitNot_u64,

    // Print
    Print,

    // Closures
    Set_upvalue, Get_upvalue, Make_closure,

    // Control flow
    Jump,          // imm_s16           : IP += offset
    Jump_if_false, // dst, imm_s16      : if (!R(dst)) IP += offset
    Unwrap_or,     // dst, src_a, imm_s16
    Call,          // dst, arg_base, ret_count
    Return,        // dst

    Eq_str, Neq_str, // dst, src_a, src_b
    Len,             // dst, src_a

    // Array
    Make_array,  // R[dst] = Array( R[src_a] ... R[src_a + src_b - 1] )
    Load_index,  // R[dst] = R[src_a][ R[src_b] ]
    Store_index, // R[dst][R[src_a]] = R[src_b]

    Make_range_ex,
    Make_range_in,
    Make_iter,     // dst, src_a (collection)
    Iter_next,     // dst (optional_val), src_a (iterator)
    Iter_prev,     // dst (optional_val), src_a (iterator)

    // Model
    Make_model,  // R[dst] = Model( R[src_a] ... R[src_a + src_b - 1] )
    Load_field,  // R[dst] = R[src_a].field[ src_b ]
    Store_field, // R[dst].field[ src_a ] = R[src_b]

    // Union
    Make_union,         // dest, src_a (names_base), src_b (payload)
    Test_union,         // dest, src_a (union_obj), src_b (variant_string)
    Load_union_payload, // dest, src_a (union_obj)

    Wrap_option,   // dest, src_a, src_b (wrap count)
    Unwrap_option, // dest, src_a
    Test_nil,      // dest, src_a
    Test_val,      // dest (bool), src_a

    Panic, // dst (unused), src_a (string register), src_b (unused)

    None
};

// 32-bit instruction
union Instruction {
    uint32_t raw;

    // RRR format (register-register-register)
    struct
    {
        Opcode op;
        uint8_t dst;
        uint8_t src_a;
        uint8_t src_b;
    } rrr;

    // RI format (register + unsigned immediate)
    struct
    {
        Opcode op;
        uint8_t dst;
        uint16_t imm;
    } ri;

    // RS format (register + signed immediate)
    struct
    {
        Opcode op;
        uint8_t dst;
        int16_t imm;
    } rs;

    struct
    {
        Opcode op;
        uint32_t imm : 24;
    } i;

    inline static Instruction make_rrr(Opcode op, uint8_t dst, uint8_t a, uint8_t b)
    {
        Instruction i{};
        i.rrr.op    = op;
        i.rrr.dst   = dst;
        i.rrr.src_a = a;
        i.rrr.src_b = b;
        return i;
    }

    inline static Instruction make_ri(Opcode op, uint8_t dst, uint16_t imm)
    {
        Instruction i{};
        i.ri.op  = op;
        i.ri.dst = dst;
        i.ri.imm = imm;
        return i;
    }

    inline static Instruction make_rs(Opcode op, uint8_t dst, int16_t imm)
    {
        Instruction i{};
        i.rs.op  = op;
        i.rs.dst = dst;
        i.rs.imm = imm;
        return i;
    }

    inline static Instruction make_i(Opcode op, uint32_t imm)
    {
        Instruction inst{};
        inst.i.op = op;
        inst.i.imm = imm & 0xFFFFFF;
        return inst;
    }
};

std::string opcode_to_string(Opcode code);
Opcode string_to_opcode(std::string code);

static_assert(sizeof(Instruction) == 4, "Instruction must be 32-bit");

} // namespace phos::vm
