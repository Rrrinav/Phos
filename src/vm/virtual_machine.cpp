#include "virtual_machine.hpp"

#include "assembler.hpp"

#include <cmath>
#include <cstdlib>
#include <utility>

namespace phos::vm {

static Upvalue_data *capture_upvalue(Green_thread_data *thread, size_t absolute_stack_index, mem::Arena &arena)
{
    Upvalue_data *prev_upvalue = nullptr;
    Upvalue_data *upvalue = thread->open_upvalues;

    // Get the physical memory address of the register
    Value *target_location = &thread->value_stack[absolute_stack_index];

    // Traverse the linked list to keep it sorted by stack address
    while (upvalue != nullptr && upvalue->location > target_location) {
        prev_upvalue = upvalue;
        upvalue = upvalue->next;
    }

    // Reuse existing upvalue if already captured
    if (upvalue != nullptr && upvalue->location == target_location) {
        return upvalue;
    }

    // Allocate a new open upvalue and point it at the stack
    Upvalue_data *created_upvalue = arena.allocate<Upvalue_data>();
    new (created_upvalue) Upvalue_data();
    created_upvalue->location = target_location;
    created_upvalue->next = upvalue;

    if (prev_upvalue == nullptr) {
        thread->open_upvalues = created_upvalue;
    } else {
        prev_upvalue->next = created_upvalue;
    }

    return created_upvalue;
}

static void close_upvalues(Green_thread_data *thread, size_t last_stack_index, mem::Arena &arena)
{
    Value *limit_location = &thread->value_stack[last_stack_index];

    while (thread->open_upvalues != nullptr && thread->open_upvalues->location >= limit_location) {
        Upvalue_data *upvalue = thread->open_upvalues;

        // Allocate a permanent 16-byte home on the heap
        Value *permanent_home = arena.allocate<Value>();

        // Copy the physical data from the stack into the heap
        *permanent_home = *(upvalue->location);

        // Reroute the pointer to the heap
        upvalue->location = permanent_home;

        // Advance the list
        thread->open_upvalues = upvalue->next;
    }
}

template <bool Is_Tracing>
void Virtual_machine::execute_loop(Green_thread_data *thread)
{
    if (thread->call_stack_count == 0 || thread->is_completed) {
        return;
    }

    Call_frame *frame = &thread->call_stack[thread->call_stack_count - 1];

    const Instruction *code = frame->closure->code;
    const Value *constants = frame->closure->constants;
    Value *registers = thread->value_stack;
    size_t ip = frame->ip;
    size_t base = frame->frame_base;

    while (true) {
        Instruction inst = code[ip];

        if constexpr (Is_Tracing) {
            std::string disassembled = Assembler::disassemble_instruction(inst, frame->closure);
            std::println(cfg.out, "TRACE: {:04} | {}", ip, disassembled);
        }

        ip++;

        switch (inst.rrr.op) {

        case Opcode::Load_const: {
            /* R[dst] := K[imm] */
            registers[base + inst.ri.dst] = constants[inst.ri.imm];
            break;
        }

        case Opcode::Load_nil: {
            /* R[dst] := nil */
            registers[base + inst.rrr.dst] = Value();
            break;
        }

        case Opcode::Load_true: {
            /* R[dst] := true */
            registers[base + inst.rrr.dst] = Value(true);
            break;
        }

        case Opcode::Load_false: {
            /* R[dst] := false */
            registers[base + inst.rrr.dst] = Value(false);
            break;
        }

        case Opcode::Move: {
            /* R[dst] := R[src_a] */
            registers[base + inst.rrr.dst] = registers[base + inst.rrr.src_a];
            break;
        }

        case Opcode::Add_i64:
        case Opcode::Sub_i64:
        case Opcode::Mul_i64:
        case Opcode::Div_i64:
        case Opcode::Mod_i64: {
            /* R[dst] := R[src_a] OP R[src_b] */
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
            /* R[dst] := R[src_a] OP R[src_b] */
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
            /* R[dst] := R[src_a] OP R[src_b] */
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
            /* R[dst] := (Target_Type) R[dst] */
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
            /* R[dst] := (R[src_a] OP R[src_b]) */
            int64_t a = registers[base + inst.rrr.src_a].as_int();
            int64_t b = registers[base + inst.rrr.src_b].as_int();

            bool result = false;
            switch (inst.rrr.op) {
            case Opcode::Eq_i64:
                result = (a == b);
                break;
            case Opcode::Neq_i64:
                result = (a != b);
                break;
            case Opcode::Lt_i64:
                result = (a < b);
                break;
            case Opcode::Lte_i64:
                result = (a <= b);
                break;
            case Opcode::Gt_i64:
                result = (a > b);
                break;
            case Opcode::Gte_i64:
                result = (a >= b);
                break;
            default:
                std::unreachable();
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
            /* R[dst] := (R[src_a] OP R[src_b]) */
            uint64_t a = registers[base + inst.rrr.src_a].as_uint();
            uint64_t b = registers[base + inst.rrr.src_b].as_uint();

            bool result = false;
            switch (inst.rrr.op) {
            case Opcode::Eq_u64:
                result = (a == b);
                break;
            case Opcode::Neq_u64:
                result = (a != b);
                break;
            case Opcode::Lt_u64:
                result = (a < b);
                break;
            case Opcode::Lte_u64:
                result = (a <= b);
                break;
            case Opcode::Gt_u64:
                result = (a > b);
                break;
            case Opcode::Gte_u64:
                result = (a >= b);
                break;
            default:
                std::unreachable();
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
            /* R[dst] := (R[src_a] OP R[src_b]) */
            double a = registers[base + inst.rrr.src_a].as_float();
            double b = registers[base + inst.rrr.src_b].as_float();

            bool result = false;
            switch (inst.rrr.op) {
            case Opcode::Eq_f64:
                result = (a == b);
                break;
            case Opcode::Neq_f64:
                result = (a != b);
                break;
            case Opcode::Lt_f64:
                result = (a < b);
                break;
            case Opcode::Lte_f64:
                result = (a <= b);
                break;
            case Opcode::Gt_f64:
                result = (a > b);
                break;
            case Opcode::Gte_f64:
                result = (a >= b);
                break;
            default:
                std::unreachable();
            }

            registers[base + inst.rrr.dst] = Value(result);
            break;
        }

        case Opcode::Jump: {
            /* IP := imm */
            ip = inst.i.imm;
            break;
        }

        case Opcode::Jump_if_false: {
            /* if (!R[dst]) IP := imm */
            if (!registers[base + inst.ri.dst].as_bool()) {
                ip = inst.ri.imm;
            }
            break;
        }

        case Opcode::Call: {
            /* R[dst] := call R[src_a](args...) */
            Value callee = registers[base + inst.rrr.src_a];
            uint8_t arg_count = inst.rrr.src_b;

            if (!callee.is_closure()) {
                panic("Attempted to call a non-function value at IP: {}", ip - 1);
            }

            Closure_data *target_closure = callee.as_closure();

            if (target_closure->arity != arg_count) {
                panic("Arity mismatch. Expected {} arguments, got {} at IP: {}", target_closure->arity, arg_count, ip - 1);
            }
            if (target_closure->native_func.has_value()) {
                std::span<Value> args(&registers[base + inst.rrr.src_a + 1], arg_count);
                Value result = (*target_closure->native_func)(args);
                registers[base + inst.rrr.dst] = result;
                break;
            }

            if (thread->call_stack_count >= 256) {
                panic("Stack overflow! Maximum call depth exceeded.");
            }

            frame->ip = ip;
            size_t new_base = base + inst.rrr.src_a + 1;

            thread->call_stack[thread->call_stack_count++] = Call_frame(target_closure, new_base);

            frame = &thread->call_stack[thread->call_stack_count - 1];
            code = frame->closure->code;
            constants = frame->closure->constants;
            base = frame->frame_base;
            ip = 0;
            break;
        }

        case Opcode::Return: {
            /* return R[dst] */
            Value result = registers[base + inst.rrr.dst];

            close_upvalues(thread, base, arena);
            thread->call_stack_count--;

            if (thread->call_stack_count == 0) {
                thread->is_completed = true;
                return; // Replaced break with return to actually exit execution loop
            }

            frame = &thread->call_stack[thread->call_stack_count - 1];
            code = frame->closure->code;
            constants = frame->closure->constants;

            ip = frame->ip;

            // Route the return value to the caller's destination register
            Instruction caller_inst = code[ip - 1];
            base = frame->frame_base;
            registers[base + caller_inst.rrr.dst] = result;
            break;
        }

        case Opcode::Print: {
            /* Output(R[dst]) -> Stream(src_a) */
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

        case Opcode::Get_upvalue: {
            /* R[dst] := Upvalue[src_a] */
            Upvalue_data *uv = frame->closure->upvalues[inst.rrr.src_a];
            registers[base + inst.rrr.dst] = *(uv->location);
            break;
        }

        case Opcode::Set_upvalue: {
            /* Upvalue[dst] := R[src_a] */
            Upvalue_data *uv = frame->closure->upvalues[inst.rrr.dst];
            *(uv->location) = registers[base + inst.rrr.src_a];
            break;
        }

        case Opcode::Make_closure: {
            /* R[dst] := Closure(K[imm], routing...) */
            Value prototype_val = frame->closure->constants[inst.ri.imm];
            Closure_data *prototype = prototype_val.as_closure();

            Closure_data *instance = arena.allocate<Closure_data>();
            *instance = *prototype;

            if (instance->upvalue_count > 0) {
                instance->upvalues = arena.allocate<Upvalue_data *>(instance->upvalue_count);
            }

            // Wire up the upvalues based on following routing instructions
            for (std::size_t i = 0; i < instance->upvalue_count; ++i) {
                Instruction route = code[ip++];
                bool is_local = (route.rrr.src_a == 1);
                uint8_t index = route.rrr.src_b;

                if (is_local) {
                    instance->upvalues[i] = capture_upvalue(thread, base + index, arena);
                } else {
                    instance->upvalues[i] = frame->closure->upvalues[index];
                }
            }

            registers[base + inst.ri.dst] = Value::make_closure(instance, 0);
            break;
        }

        case Opcode::Make_array: {
            uint8_t base_reg = inst.rrr.src_a;
            uint8_t count = inst.rrr.src_b;

            // Allocate the array payload using your new factory
            Value arr_val = Value::make_array(arena, count, 0);
            Array_data *arr = arr_val.as_array();

            // Bulk copy the evaluated elements from the VM registers into the Arena array
            for (uint8_t i = 0; i < count; ++i) {
                arr->elements[i] = registers[base + base_reg + i];
            }
            arr->count = count;

            registers[base + inst.rrr.dst] = arr_val;
            break;
        }

        case Opcode::Load_index: {
            Value arr_val = registers[base + inst.rrr.src_a];
            Value idx_val = registers[base + inst.rrr.src_b];

            if (!arr_val.is_array()) {
                panic("Attempted to index a non-array value at IP: {}", ip - 1);
            }

            Array_data *arr = arr_val.as_array();
            int64_t index = idx_val.as_int();

            if (index < 0 || index >= static_cast<int64_t>(arr->count)) {
                panic("Array index out of bounds (index: {}, size: {}) at IP: {}", index, arr->count, ip - 1);
            }

            registers[base + inst.rrr.dst] = arr->elements[index];
            break;
        }

        case Opcode::Store_index: {
            Value arr_val = registers[base + inst.rrr.dst];
            Value idx_val = registers[base + inst.rrr.src_a];
            Value val = registers[base + inst.rrr.src_b];

            if (!arr_val.is_array()) {
                panic("Attempted to index assign a non-array value at IP: {}", ip - 1);
            }

            Array_data *arr = arr_val.as_array();
            int64_t index = idx_val.as_int();

            if (index < 0 || index >= static_cast<int64_t>(arr->count)) {
                panic("Array index out of bounds (index: {}, size: {}) at IP: {}", index, arr->count, ip - 1);
            }

            arr->elements[index] = val;
            break;
        }

        default: {
            /* ??? */
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
