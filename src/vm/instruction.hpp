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
    Load_const,         // dst, imm_u16      : R(dst) = K[imm]
    Load_nil,           // dst               : R(dst) = Nil
    Load_true,          // dst               : R(dst) = True
    Load_false,         // dst               : R(dst) = False
    Move,               // dst, src_a        : R(dst) = R(src_a)

    // Arithmetic
    Add_i64, Add_u64, Add_f64, Sub_i64, Sub_u64, Sub_f64, // dst, src_a, src_b
    Mul_i64, Mul_u64, Mul_f64, Div_i64, Div_u64, Div_f64, // dst, src_a, src_b

    Mod_i64, Mod_u64, Mod_f64, // dst, src_a, src_b

    // Comparison
    Eq_i64, Neq_i64, Lt_i64, Lte_i64, Gt_i64, Gte_i64, // dest, src_a, src_b
    Eq_f64, Neq_f64, Lt_f64, Lte_f64, Gt_f64, Gte_f64, // dest, src_a, src_b

    // Print
    Print,

    // Control flow
    Jump,               // imm_s16           : IP += offset
    Jump_if_false,      // dst, imm_s16      : if (!R(dst)) IP += offset
    Unwrap_or,          // dst, src_a, imm_s16
    Call,               // dst, arg_base, ret_count
    Return,             // dst
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
};

std::string opcode_to_string(Opcode code);
Opcode string_to_opcode(std::string code);

static_assert(sizeof(Instruction) == 4, "Instruction must be 32-bit");

} // namespace phos::vm
