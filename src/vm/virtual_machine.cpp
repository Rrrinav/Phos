#include "virtual_machine.hpp"

#include "assembler.hpp"

#include <cmath>
#include <cstdlib>
#include <utility>

namespace phos::vm {

template <bool Is_Tracing>
void Virtual_machine::execute_loop(Green_thread_data *thread)
{
    // Safety check: is there actually code to run?
    if (thread->call_stack_count == 0 || thread->is_completed) {
        return;
    }

    // Grab the current active Call Frame
    Call_frame *frame = &thread->call_stack[thread->call_stack_count - 1];

    const Instruction *code = frame->closure->code;
    const Value *constants = frame->closure->constants;
    Value *registers = thread->value_stack;
    size_t ip = frame->ip;
    size_t base = frame->frame_base;

    while (true) {
        // Fetch the 32-bit instruction
        Instruction inst = code[ip];

        if constexpr (Is_Tracing) {
            std::string disassembled = Assembler::disassemble_instruction(inst, frame->closure);
            std::println(cfg.out, "TRACE: {:04} | {}", ip, disassembled);
        }

        ip++;

        // Dispatch based on the Opcode
        switch (inst.rrr.op) {
        // CONSTANTS
        case Opcode::Load_const: {
            registers[base + inst.ri.dst] = constants[inst.ri.imm];
            break;
        }

        case Opcode::Move: {
            registers[base + inst.rrr.dst] = registers[base + inst.rrr.src_a];
            break;
        }

        // BINARY OPERATIONS
        case Opcode::Add_i64:
        case Opcode::Sub_i64:
        case Opcode::Mul_i64:
        case Opcode::Div_i64:
        case Opcode::Mod_i64: {
            int64_t a = registers[base + inst.rrr.src_a].as_int();
            int64_t b = registers[base + inst.rrr.src_b].as_int();

            if (inst.rrr.op == Opcode::Div_i64 && b == 0) {
                panic("Division by zero at IP: {}", ip - 1);
            }

            int64_t result = this->bini64_op(a, b, inst.rrr.op);
            registers[base + inst.rrr.dst] = Value(result);
            break;
        }

        case Opcode::Add_u64:
        case Opcode::Sub_u64:
        case Opcode::Mul_u64:
        case Opcode::Div_u64:
        case Opcode::Mod_u64: {
            uint64_t a = registers[base + inst.rrr.src_a].as_uint();
            uint64_t b = registers[base + inst.rrr.src_b].as_uint();

            if (inst.rrr.op == Opcode::Div_u64 && b == 0) {
                panic("Division by zero at IP: {}", ip - 1);
            }

            uint64_t result = this->binu64_op(a, b, inst.rrr.op);
            registers[base + inst.rrr.dst] = Value(result);
            break;
        }

        case Opcode::Add_f64:
        case Opcode::Sub_f64:
        case Opcode::Mul_f64:
        case Opcode::Div_f64:
        case Opcode::Mod_f64: {
            double a = registers[base + inst.rrr.src_a].as_float();
            double b = registers[base + inst.rrr.src_b].as_float();

            if (inst.rrr.op == Opcode::Div_f64 && b == 0.0) {
                panic("Division by zero at IP: {}", ip - 1);
            }

            double result = this->binf64_op(a, b, inst.rrr.op);
            registers[base + inst.rrr.dst] = Value(result);
            break;
        }

        case Opcode::Cast_i8:
        case Opcode::Cast_i16:
        case Opcode::Cast_i32:
        case Opcode::Cast_i64:
        case Opcode::Cast_u8:
        case Opcode::Cast_u16:
        case Opcode::Cast_u32:
        case Opcode::Cast_u64:
        case Opcode::Cast_f16:
        case Opcode::Cast_f32:
        case Opcode::Cast_f64: {
            types::Primitive_kind target_kind = types::Primitive_kind::I64;
            switch (inst.rrr.op) {
            case Opcode::Cast_i8:
                target_kind = types::Primitive_kind::I8;
                break;
            case Opcode::Cast_i16:
                target_kind = types::Primitive_kind::I16;
                break;
            case Opcode::Cast_i32:
                target_kind = types::Primitive_kind::I32;
                break;
            case Opcode::Cast_i64:
                target_kind = types::Primitive_kind::I64;
                break;
            case Opcode::Cast_u8:
                target_kind = types::Primitive_kind::U8;
                break;
            case Opcode::Cast_u16:
                target_kind = types::Primitive_kind::U16;
                break;
            case Opcode::Cast_u32:
                target_kind = types::Primitive_kind::U32;
                break;
            case Opcode::Cast_u64:
                target_kind = types::Primitive_kind::U64;
                break;
            case Opcode::Cast_f16:
                target_kind = types::Primitive_kind::F16;
                break;
            case Opcode::Cast_f32:
                target_kind = types::Primitive_kind::F32;
                break;
            case Opcode::Cast_f64:
                target_kind = types::Primitive_kind::F64;
                break;
            default:
                std::unreachable();
            }

            auto casted = registers[base + inst.rrr.dst].cast_numeric(target_kind);
            if (!casted) {
                panic("Invalid numeric cast at IP: {}", ip - 1);
            }
            registers[base + inst.rrr.dst] = *casted;
            break;
        }
        case Opcode::Eq_i64:
        case Opcode::Neq_i64:
        case Opcode::Lt_i64:
        case Opcode::Lte_i64:
        case Opcode::Gt_i64:
        case Opcode::Gte_i64: {
            int64_t a = registers[base + inst.rrr.src_a].as_int();
            int64_t b = registers[base + inst.rrr.src_b].as_int();

            bool result = false;
            switch (inst.rrr.op) {
            case Opcode::Eq_i64:  result = (a == b); break;
            case Opcode::Neq_i64: result = (a != b); break;
            case Opcode::Lt_i64:  result = (a < b);  break;
            case Opcode::Lte_i64: result = (a <= b); break;
            case Opcode::Gt_i64:  result = (a > b);  break;
            case Opcode::Gte_i64: result = (a >= b); break;
            default: std::unreachable();
            }
            registers[base + inst.rrr.dst] = Value(result);
            break;
        }

        case Opcode::Eq_u64:
        case Opcode::Neq_u64:
        case Opcode::Lt_u64:
        case Opcode::Lte_u64:
        case Opcode::Gt_u64:
        case Opcode::Gte_u64: {
            uint64_t a = registers[base + inst.rrr.src_a].as_uint();
            uint64_t b = registers[base + inst.rrr.src_b].as_uint();

            bool result = false;
            switch (inst.rrr.op) {
            case Opcode::Eq_u64:  result = (a == b); break;
            case Opcode::Neq_u64: result = (a != b); break;
            case Opcode::Lt_u64:  result = (a < b);  break;
            case Opcode::Lte_u64: result = (a <= b); break;
            case Opcode::Gt_u64:  result = (a > b);  break;
            case Opcode::Gte_u64: result = (a >= b); break;
            default: std::unreachable();
            }
            registers[base + inst.rrr.dst] = Value(result);
            break;
        }

        case Opcode::Eq_f64:
        case Opcode::Neq_f64:
        case Opcode::Lt_f64:
        case Opcode::Lte_f64:
        case Opcode::Gt_f64:
        case Opcode::Gte_f64: {
            double a = registers[base + inst.rrr.src_a].as_float();
            double b = registers[base + inst.rrr.src_b].as_float();

            bool result = false;
            switch (inst.rrr.op) {
            case Opcode::Eq_f64:  result = (a == b); break;
            case Opcode::Neq_f64: result = (a != b); break;
            case Opcode::Lt_f64:  result = (a < b);  break;
            case Opcode::Lte_f64: result = (a <= b); break;
            case Opcode::Gt_f64:  result = (a > b);  break;
            case Opcode::Gte_f64: result = (a >= b); break;
            default: std::unreachable();
            }

            registers[base + inst.rrr.dst] = Value(result);
            break;
        }

        case Opcode::Jump: {
            ip = inst.i.imm;
            break;
        }

        case Opcode::Jump_if_false: {
            if (!registers[base + inst.ri.dst].as_bool()) {
                ip = inst.ri.imm;
            }
            break;
        }

        case Opcode::Return: {
            thread->is_completed = true;
            frame->ip = ip;
            return;
        }

        case Opcode::Print: {
            Value result = registers[base + inst.rrr.dst];
            uint8_t stream_flag = inst.rrr.src_a;

            std::string text_to_print = result.to_string();

            if (stream_flag == 1) {
                this->cfg.err << text_to_print;
            } else {
                this->cfg.out << text_to_print;
            }
            break;
        }

        default: {
            panic("Unrecognized or unimplemented Opcode: {}", opcode_to_string(inst.rrr.op));
            break;
        }
        }
    }
}

auto Virtual_machine::bini64_op(int64_t a, int64_t b, Opcode op) -> int64_t
{
    switch (op) {
    case Opcode::Add_i64:
        return a + b;
    case Opcode::Sub_i64:
        return a - b;
    case Opcode::Mul_i64:
        return a * b;
    case Opcode::Div_i64:
        return a / b;
    case Opcode::Mod_i64:
        return a % b;
    default:
        std::unreachable();
    }
};

auto Virtual_machine::binu64_op(uint64_t a, uint64_t b, Opcode op) -> uint64_t
{
    switch (op) {
    case Opcode::Add_u64:
        return a + b;
    case Opcode::Sub_u64:
        return a - b;
    case Opcode::Mul_u64:
        return a * b;
    case Opcode::Div_u64:
        return a / b;
    case Opcode::Mod_u64:
        return a % b;
    default:
        std::unreachable();
    }
};

auto Virtual_machine::binf64_op(double a, double b, Opcode op) -> double
{
    switch (op) {
    case Opcode::Add_f64:
        return a + b;
    case Opcode::Sub_f64:
        return a - b;
    case Opcode::Mul_f64:
        return a * b;
    case Opcode::Div_f64:
        return a / b;
    case Opcode::Mod_f64:
        return fmod(a, b);
    default:
        std::unreachable();
    }
};

// Explicit template instantiations so the linker can find both versions
template void Virtual_machine::execute_loop<true>(Green_thread_data *);
template void Virtual_machine::execute_loop<false>(Green_thread_data *);

} // namespace phos::vm
