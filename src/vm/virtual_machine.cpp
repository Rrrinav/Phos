#include "virtual_machine.hpp"

#include <cmath>
#include <cstdlib>
#include <iostream>
#include <utility>

namespace phos::vm {

void Virtual_machine::execute(Green_thread_data *thread)
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
        // Fetch and Decode the 32-bit instruction
        Instruction inst = code[ip++];

        // Dispatch based on the Opcode
        switch (inst.rrr.op) {
        // CONSTANTS
        case Opcode::Load_const: {
            // RI Format: dst, imm
            // R(dst) = K[imm]
            registers[base + inst.ri.dst] = constants[inst.ri.imm];
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
        // CONTROL FLOW
        case Opcode::Return: {
            // NOTE: This is just for current debugging purposes.
            Value result = registers[base + inst.rrr.dst];
            std::cout << "Execution Finished. Result: " << result.to_debug_string() << "\n";

            thread->is_completed = true;
            frame->ip = ip;
            return;
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

} // namespace phos::vm
