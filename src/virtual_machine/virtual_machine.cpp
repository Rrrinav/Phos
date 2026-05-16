#include "virtual_machine.hpp"

#include "backend/assembler.hpp"

#include <cmath>
#include <cstdlib>
#include <cstring>
#include <utility>

namespace phos::vm {

// --- VM Context Implementations ---
phos::Green_thread_data &Vm_context::current_thread() const noexcept
{
    return *thread;
}
Call_frame &Vm_context::current_frame() const noexcept
{
    return *frame;
}
std::vector<phos::Value> &Vm_context::global_values() const noexcept
{
    return *globals;
}
phos::Value &Vm_context::register_at(size_t slot) const noexcept
{
    return registers[*frame_base + slot];
}
size_t Vm_context::instruction_pointer() const noexcept
{
    return *ip;
}
size_t Vm_context::current_base() const noexcept
{
    return *frame_base;
}

size_t Vm_context::active_stack_limit() const noexcept
{
    const size_t live_limit = *frame_base + FRAME_REGISTER_WINDOW;
    return (live_limit < thread->value_stack_capacity) ? live_limit : thread->value_stack_capacity;
}

bool Vm_context::should_collect() const noexcept
{
    return heap->needs_gc();
}

void Vm_context::collect_garbage() const
{
    frame->ip = *ip;
    thread->live_value_count = active_stack_limit();
    heap->collect(*thread, *globals);
}

// Internal VM Helpers
static size_t get_active_stack_limit(const Green_thread_data &thread, size_t frame_base)
{
    const size_t live_limit = frame_base + Vm_context::FRAME_REGISTER_WINDOW;
    return (live_limit < thread.value_stack_capacity) ? live_limit : thread.value_stack_capacity;
}

static void refresh_live_stack_count(Green_thread_data &thread, size_t frame_base)
{
    thread.live_value_count = get_active_stack_limit(thread, frame_base);
}

static Vm_context make_vm_context(
    Virtual_machine &machine,
    gc::Gc_heap &gc,
    mem::Arena &arena,
    Green_thread_data &thread,
    Call_frame *frame,
    Value *registers,
    std::vector<Value> &globals,
    size_t &ip,
    size_t &frame_base)
{
    return Vm_context{
        .machine = &machine,
        .heap = &gc,
        .scratch = &arena,
        .thread = &thread,
        .frame = frame,
        .registers = registers,
        .globals = &globals,
        .ip = &ip,
        .frame_base = &frame_base,
        .cmd_args = machine.cmd_args
    };
}

static Upvalue_data *capture_upvalue(Green_thread_data *thread, size_t absolute_stack_index, Vm_context &ctx)
{
    Upvalue_data *prev_upvalue = nullptr;
    Upvalue_data *upvalue = thread->open_upvalues;

    Value *target_location = &thread->value_stack[absolute_stack_index];

    while (upvalue != nullptr && upvalue->location > target_location) {
        prev_upvalue = upvalue;
        upvalue = upvalue->next;
    }

    if (upvalue != nullptr && upvalue->location == target_location) {
        return upvalue;
    }

    Upvalue_data *created_upvalue = ctx.alloc<Upvalue_data>(sizeof(Upvalue_data), static_cast<uint8_t>(Value_tag::Upvalue));
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

static void close_upvalues(Green_thread_data *thread, size_t last_stack_index, [[maybe_unused]]gc::Gc_heap &gc_heap)
{
    Value *limit_location = &thread->value_stack[last_stack_index];

    while (thread->open_upvalues != nullptr && thread->open_upvalues->location >= limit_location) {
        Upvalue_data *upvalue = thread->open_upvalues;

        upvalue->closed = *(upvalue->location);
        upvalue->location = &upvalue->closed;
        upvalue->is_closed = true;

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
    refresh_live_stack_count(*thread, base);

    Vm_context ctx = make_vm_context(*this, gc, arena, *thread, frame, registers, globals, ip, base);

    while (true) {
        if (gc.needs_gc()) {
            frame->ip = ip;
            refresh_live_stack_count(*thread, base);
            gc.collect(*thread, globals);
        }

        Instruction inst = code[ip];

        if constexpr (Is_Tracing) {
            std::string disassembled = Assembler::disassemble_instruction(inst, frame->closure);
            std::println(*cfg.out, "TRACE: {:04} | {}", ip, disassembled);
            cfg.out->flush();
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

        case Opcode::Load_global: {
            uint16_t global_idx = inst.ri.imm;
            if (global_idx >= globals.size()) {
                globals.resize(global_idx + 1, Value(nullptr));
            }
            registers[base + inst.ri.dst] = globals[global_idx];
            break;
        }

        case Opcode::Store_global: {
            uint16_t global_idx = inst.ri.imm;
            if (global_idx >= globals.size()) {
                globals.resize(global_idx + 1, Value(nullptr));
            }
            globals[global_idx] = registers[base + inst.ri.dst];
            break;
        }
        case Opcode::Concat_str: {
            /* R[dst] := R[src_a] .. R[src_b] */
            const Value &va = registers[base + inst.rrr.src_a];
            const Value &vb = registers[base + inst.rrr.src_b];
            // Standard library string objects use Trailing Storage.
            // We create a new contiguous string block.
            std::string res = std::string(va.as_string()) + std::string(vb.as_string());
            registers[base + inst.rrr.dst] = Value::make_string(ctx, res);
            break;
        }

        case Opcode::Add_i64:
        case Opcode::Sub_i64:
        case Opcode::Mul_i64:
        case Opcode::Div_i64:
        case Opcode::Mod_i64:
        case Opcode::BitAnd_i64:
        case Opcode::BitOr_i64:
        case Opcode::BitXor_i64:
        case Opcode::Shl_i64:
        case Opcode::Shr_i64: {
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
        case Opcode::Mod_u64:
        case Opcode::BitAnd_u64:
        case Opcode::BitOr_u64:
        case Opcode::BitXor_u64:
        case Opcode::Shl_u64:
        case Opcode::Shr_u64: {
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

        case Opcode::Neg_i64: {
            int64_t val = registers[base + inst.rrr.src_a].as_int();
            registers[base + inst.rrr.dst] = Value(-val);
            break;
        }
        case Opcode::Neg_f64: {
            double val = registers[base + inst.rrr.src_a].as_float();
            registers[base + inst.rrr.dst] = Value(-val);
            break;
        }
        case Opcode::Not: {
            bool val = registers[base + inst.rrr.src_a].as_bool();
            registers[base + inst.rrr.dst] = Value(!val);
            break;
        }
        case Opcode::BitNot_i64: {
            int64_t val = registers[base + inst.rrr.src_a].as_int();
            registers[base + inst.rrr.dst] = Value(~val);
            break;
        }
        case Opcode::BitNot_u64: {
            uint64_t val = registers[base + inst.rrr.src_a].as_uint();
            registers[base + inst.rrr.dst] = Value(~val);
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

        case Opcode::Call: {
            Value callee = registers[base + inst.rrr.src_a];
            uint8_t arg_count = inst.rrr.src_b;

            if (!callee.is_closure()) {
                panic("Attempted to call a non-function value at IP: {}", ip - 1);
            }

            Closure_data *target_closure = callee.as_closure();

            if (target_closure->native_func.has_value()) {
                if (target_closure->is_variadic) {
                    if (arg_count < target_closure->min_arity) {
                        panic(
                            "Arity mismatch. Expected at least {} arguments, got {} at IP: {}",
                            target_closure->min_arity,
                            arg_count,
                            ip - 1);
                    }
                } else if (target_closure->arity != arg_count) {
                    panic("Arity mismatch. Expected {} arguments, got {} at IP: {}", target_closure->arity, arg_count, ip - 1);
                }

                frame->ip = ip;
                std::span<Value> args(&registers[base + inst.rrr.src_a + 1], arg_count);
                Value result = (*target_closure->native_func)(ctx, args);
                registers[base + inst.rrr.dst] = result;
                break;
            }

            if (target_closure->arity != arg_count) {
                panic("Arity mismatch. Expected {} arguments, got {} at IP: {}", target_closure->arity, arg_count, ip - 1);
            }

            if (thread->call_stack_count >= thread->call_stack_capacity) {
                panic("Stack overflow! Maximum call depth exceeded.");
            }

            frame->ip = ip;
            size_t new_base = base + inst.rrr.src_a + 1;
            if (new_base + Vm_context::FRAME_REGISTER_WINDOW > thread->value_stack_capacity) {
                panic("Register stack overflow! Need {} slots, have {}.", new_base + Vm_context::FRAME_REGISTER_WINDOW, thread->value_stack_capacity);
            }

            thread->call_stack[thread->call_stack_count++] = Call_frame(target_closure, new_base);

            frame = &thread->call_stack[thread->call_stack_count - 1];
            code = frame->closure->code;
            constants = frame->closure->constants;
            base = frame->frame_base;
            ip = 0;
            refresh_live_stack_count(*thread, base);
            ctx = make_vm_context(*this, gc, arena, *thread, frame, registers, globals, ip, base);
            break;
        }

        case Opcode::Return: {
            /* return R[dst] */
            Value result = registers[base + inst.rrr.dst];

            close_upvalues(thread, base, gc);
            thread->call_stack_count--;

            if (thread->call_stack_count == 0) {
                thread->is_completed = true;
                return;
            }

            size_t old_base = base;

            frame = &thread->call_stack[thread->call_stack_count - 1];
            code = frame->closure->code;
            constants = frame->closure->constants;
            ip = frame->ip;
            base = frame->frame_base;

            // CLEANUP FIRST!
            // Wipe the returning frame's window to prevent GC ghost references.
            size_t clear_limit = get_active_stack_limit(*thread, old_base);
            for (size_t i = old_base; i < clear_limit; ++i) {
                registers[i] = Value();
            }

            // 2. ASSIGN RESULT SECOND!
            // Since base is now the caller's base, and we know the Call instruction was at ip-1.
            Instruction caller_inst = code[ip - 1];
            registers[base + caller_inst.rrr.dst] = result;

            refresh_live_stack_count(*thread, base);
            ctx = make_vm_context(*this, gc, arena, *thread, frame, registers, globals, ip, base);
            break;
        }

        case Opcode::Print: {
            Value result = registers[base + inst.rrr.dst];
            uint8_t stream_flag = inst.rrr.src_a;

            if (stream_flag == 1) {
                *this->cfg.err << result.to_debug_string();
            } else {
                *this->cfg.out << result.to_debug_string();
            }
            break;
        }

        case Opcode::Get_upvalue: {
            Upvalue_data *uv = frame->closure->upvalues[inst.rrr.src_a];
            registers[base + inst.rrr.dst] = *(uv->location);
            break;
        }

        case Opcode::Set_upvalue: {
            Upvalue_data *uv = frame->closure->upvalues[inst.rrr.dst];
            *(uv->location) = registers[base + inst.rrr.src_a];
            break;
        }

        case Opcode::Make_closure: {
            Value prototype_val = frame->closure->constants[inst.ri.imm];
            Closure_data *prototype = prototype_val.as_closure();

            Closure_data *instance = ctx.alloc<Closure_data>(sizeof(Closure_data), static_cast<uint8_t>(Value_tag::Closure));
            *instance = *prototype;
            instance->is_prototype = false;
            Value instance_val = Value::make_closure(instance, 0);
            auto guard = ctx.protect(&instance_val);

            if (instance->upvalue_count > 0) {
                size_t alloc_bytes = instance->upvalue_count * sizeof(Upvalue_data *);
                instance->upvalues = static_cast<Upvalue_data **>(std::malloc(alloc_bytes));
                if (instance->upvalues == nullptr) {
                    throw std::bad_alloc{};
                }
                std::memset(instance->upvalues, 0, alloc_bytes);
                gc.add_external_bytes(alloc_bytes);
            }

            for (std::size_t i = 0; i < instance->upvalue_count; ++i) {
                Instruction route = code[ip++];
                bool is_local = (route.rrr.src_a == 1);
                uint8_t index = route.rrr.src_b;

                if (is_local) {
                    instance->upvalues[i] = capture_upvalue(thread, base + index, ctx);
                } else {
                    instance->upvalues[i] = frame->closure->upvalues[index];
                }
            }

            registers[base + inst.ri.dst] = instance_val;
            break;
        }

        case Opcode::Eq_str: {
            Value left = registers[base + inst.rrr.src_a];
            Value right = registers[base + inst.rrr.src_b];
            bool match = (left.as_string() == right.as_string());
            registers[base + inst.rrr.dst] = Value(match);
            break;
        }
        case Opcode::Neq_str: {
            Value left = registers[base + inst.rrr.src_a];
            Value right = registers[base + inst.rrr.src_b];
            bool match = (left.as_string() != right.as_string());
            registers[base + inst.rrr.dst] = Value(match);
            break;
        }
        case Opcode::Len: {
            Value val = registers[base + inst.rrr.src_a];
            if (val.is_string()) {
                registers[base + inst.rrr.dst] = Value(static_cast<uint64_t>(val.as_string_data()->length));
            } else if (val.is_array()) {
                registers[base + inst.rrr.dst] = Value(static_cast<uint64_t>(val.as_array()->count));
            } else {
                panic("Attempted to get length of an unsupported type at IP: {}", ip - 1);
            }
            break;
        }
        case Opcode::Cast_str_to_arr: {
            Value str_val = registers[base + inst.rrr.src_a];
            if (!str_val.is_string()) {
                panic("Expected a string for cast to byte array at IP: {}", ip - 1);
            }

            String_data *str = str_val.as_string_data();
            bool is_signed = (inst.rrr.src_b == 1);

            Value arr_val = Value::make_array(ctx, str->length, 0);
            Array_data *arr = arr_val.as_array();
            arr->count = str->length;

            for (size_t i = 0; i < str->length; ++i) {
                if (is_signed) {
                    arr->elements[i] = Value(static_cast<int64_t>(static_cast<int8_t>(str->chars[i])));
                } else {
                    arr->elements[i] = Value(static_cast<uint64_t>(static_cast<uint8_t>(str->chars[i])));
                }
            }

            registers[base + inst.rrr.dst] = arr_val;
            break;
        }

        case Opcode::Cast_arr_to_str: {
            Value arr_val = registers[base + inst.rrr.src_a];
            if (!arr_val.is_array()) {
                panic("Expected an array for cast to string at IP: {}", ip - 1);
            }

            Array_data *arr = arr_val.as_array();
            std::string s;
            s.reserve(arr->count);

            for (size_t i = 0; i < arr->count; ++i) {
                s.push_back(static_cast<char>(arr->elements[i].as_int()));
            }

            registers[base + inst.rrr.dst] = Value::make_string(ctx, s);
            break;
        }

        case Opcode::Make_array: {
            uint8_t base_reg = inst.rrr.src_a;
            uint8_t count = inst.rrr.src_b;

            Value arr_val = Value::make_array(ctx, count, 0);
            Array_data *arr = arr_val.as_array();

            for (uint8_t i = 0; i < count; ++i) {
                arr->elements[i] = registers[base + base_reg + i];
            }
            arr->count = count;

            registers[base + inst.rrr.dst] = arr_val;
            break;
        }

        case Opcode::Load_index: {
            Value collection_val = registers[base + inst.rrr.src_a];
            Value idx_val = registers[base + inst.rrr.src_b];
            int64_t index = idx_val.as_int();

            if (collection_val.is_array()) {
                Array_data *arr = collection_val.as_array();
                if (index < 0 || index >= static_cast<int64_t>(arr->count)) {
                    panic("Array index out of bounds (index: {}, size: {}) at IP: {}", index, arr->count, ip - 1);
                }
                registers[base + inst.rrr.dst] = arr->elements[index];

            } else if (collection_val.is_string()) {
                String_data *str = collection_val.as_string_data();
                if (index < 0 || index >= static_cast<int64_t>(str->length)) {
                    panic("String index out of bounds (index: {}, size: {}) at IP: {}", index, str->length, ip - 1);
                }
                registers[base + inst.rrr.dst] = Value::make_string(ctx, std::string_view(&str->chars[index], 1));

            } else {
                panic("Attempted to index a non-collection at IP: {}", ip - 1);
            }
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

        case Opcode::Make_model: {
            uint8_t count = inst.rrr.src_b;
            Value model_val = Value::make_model(ctx, {}, count);

            for (uint8_t i = 0; i < count; i++) {
                model_val.as_model()->fields[i] = registers[base + inst.rrr.src_a + i];
            }

            registers[base + inst.rrr.dst] = model_val;
            break;
        }

        case Opcode::Load_field: {
            Value model_val = registers[base + inst.rrr.src_a];
            uint8_t index = inst.rrr.src_b;

            if (!model_val.is_model()) {
                panic("Attempted to access a field on a non-model value at IP: {}", ip - 1);
            }

            Model_data *model = model_val.as_model();

            if (index >= model->field_count) {
                panic("Model field index out of bounds (index: {}, size: {}) at IP: {}", index, model->field_count, ip - 1);
            }

            registers[base + inst.rrr.dst] = model->fields[index];
            break;
        }

        case Opcode::Store_field: {
            Value model_val = registers[base + inst.rrr.dst];
            uint8_t index = inst.rrr.src_a;
            Value val = registers[base + inst.rrr.src_b];

            if (!model_val.is_model()) {
                panic("Attempted to assign a field on a non-model value at IP: {}", ip - 1);
            }

            Model_data *model = model_val.as_model();

            if (index >= model->field_count) {
                panic("Model field index out of bounds (index: {}, size: {}) at IP: {}", index, model->field_count, ip - 1);
            }

            model->fields[index] = val;
            break;
        }
        case Opcode::Make_union: {
            Value union_name_val = registers[base + inst.rrr.src_a];
            Value variant_name_val = registers[base + inst.rrr.src_a + 1];
            Value payload_val = registers[base + inst.rrr.src_b];

            registers[base + inst.rrr.dst] = Value::make_union(ctx, union_name_val.as_string_data(), variant_name_val.as_string_data(), payload_val);
            break;
        }

        case Opcode::Test_union: {
            Value union_val = registers[base + inst.rrr.src_a];
            Value target_variant = registers[base + inst.rrr.src_b];

            if (!union_val.is_union()) {
                panic("Attempted .has() on a non-union value at IP: {}", ip - 1);
            }

            String_data *active_variant = union_val.as_union()->variant_name;
            String_data *target_str = target_variant.as_string_data();

            bool match = (active_variant->length == target_str->length)
                && (std::strncmp(active_variant->chars, target_str->chars, active_variant->length) == 0);

            registers[base + inst.rrr.dst] = Value(match);
            break;
        }

        case Opcode::Load_union_payload: {
            Value union_val = registers[base + inst.rrr.src_a];
            if (!union_val.is_union()) {
                panic("Attempted to load payload from non-union at IP: {}", ip - 1);
            }
            registers[base + inst.rrr.dst] = *(union_val.as_union()->payload);
            break;
        }

        case Opcode::Wrap_option: {
            Value val = registers[base + inst.rrr.src_a];
            registers[base + inst.rrr.dst] = val.wrap_optional(inst.rrr.src_b);
            break;
        }
        case Opcode::Unwrap_option: {
            Value val = registers[base + inst.rrr.src_a];

            if (val.is_nil() && val.depth() == 0) {
                panic("Attempted to unwrap a nil value at IP: {}", ip - 1);
            }

            registers[base + inst.rrr.dst] = val.unwrap_optional();
            break;
        }
        case Opcode::Test_nil: {
            Value val = registers[base + inst.rrr.src_a];
            registers[base + inst.rrr.dst] = Value(val.depth() <= 1 && val.is_nil());
            break;
        }
        case Opcode::Test_val: {
            Value src = registers[base + inst.rrr.src_a];
            registers[base + inst.rrr.dst] = Value(src.depth() > 1 || !src.is_nil());
            break;
        }
        case Opcode::Iter_next: {
            Value iter_val = registers[base + inst.rrr.src_a];
            Iterator_data *iter = iter_val.as_iterator();

            iter->cursor++;

            bool valid = false;
            switch (iter->state_type) {
            case Iterator_data::State_type::Empty:
                valid = false;
                break;
            case Iterator_data::State_type::Singleton:
                valid = (iter->cursor == 0);
                break;
            case Iterator_data::State_type::Interval:
                valid = iter->inclusive ? (iter->cursor <= iter->end) : (iter->cursor < iter->end);
                break;
            case Iterator_data::State_type::Array:
                valid = (iter->cursor < static_cast<int64_t>(iter->collection.as_array()->count));
                break;
            case Iterator_data::State_type::String:
                valid = (iter->cursor < static_cast<int64_t>(iter->collection.as_string_data()->length));
                break;
            }

            if (!valid) {
                registers[base + inst.rrr.dst] = Value();
            } else {
                Value val;
                switch (iter->state_type) {
                case Iterator_data::State_type::Singleton:
                    val = iter->collection;
                    break;
                case Iterator_data::State_type::Interval:
                    val = Value(iter->cursor);
                    break;
                case Iterator_data::State_type::Array:
                    val = iter->collection.as_array()->elements[iter->cursor];
                    break;
                case Iterator_data::State_type::String:
                    val = Value::make_string(ctx, std::string_view(&iter->collection.as_string_data()->chars[iter->cursor], 1));
                    break;
                default:
                    std::unreachable();
                }

                registers[base + inst.rrr.dst] = val.wrap_optional(1);
            }
            break;
        }

        case Opcode::Iter_prev: {
            Value iter_val = registers[base + inst.rrr.src_a];
            Iterator_data *iter = iter_val.as_iterator();

            iter->cursor--;

            bool valid = false;
            switch (iter->state_type) {
            case Iterator_data::State_type::Empty:
                valid = false;
                break;
            case Iterator_data::State_type::Singleton:
                valid = (iter->cursor == 0);
                break;
            case Iterator_data::State_type::Interval:
                valid = (iter->cursor >= iter->start);
                break;
            case Iterator_data::State_type::Array:
                valid = (iter->cursor >= 0);
                break;
            case Iterator_data::State_type::String:
                valid = (iter->cursor >= 0);
                break;
            }

            if (!valid) {
                registers[base + inst.rrr.dst] = Value();
            } else {
                Value val;
                switch (iter->state_type) {
                case Iterator_data::State_type::Singleton:
                    val = iter->collection;
                    break;
                case Iterator_data::State_type::Interval:
                    val = Value(iter->cursor);
                    break;
                case Iterator_data::State_type::Array:
                    val = iter->collection.as_array()->elements[iter->cursor];
                    break;
                case Iterator_data::State_type::String:
                    val = Value::make_string(ctx, std::string_view(&iter->collection.as_string_data()->chars[iter->cursor], 1));
                    break;
                default:
                    std::unreachable();
                }
                registers[base + inst.rrr.dst] = val.wrap_optional(1);
            }
            break;
        }

        case Opcode::Make_range_ex:
        case Opcode::Make_range_in: {
            int64_t start = registers[base + inst.rrr.src_a].as_int();
            int64_t end = registers[base + inst.rrr.src_b].as_int();

            Value iter_val = Value::make_iterator(ctx);
            Iterator_data *iter = iter_val.as_iterator();

            iter->state_type = Iterator_data::State_type::Interval;
            iter->start = start;
            iter->end = end;
            iter->inclusive = (inst.rrr.op == Opcode::Make_range_in);
            iter->cursor = start;

            registers[base + inst.rrr.dst] = iter_val;
            break;
        }

        case Opcode::Make_iter: {
            Value collection = registers[base + inst.rrr.src_a];

            if (collection.is_iterator()) {
                registers[base + inst.rrr.dst] = collection;
                break;
            }

            Value iter_val = Value::make_iterator(ctx);
            Iterator_data *iter = iter_val.as_iterator();
            iter->cursor = -1;

            auto setup_iter = [&](Value val) {
                iter->collection = val;
                if (val.is_array()) {
                    iter->state_type = Iterator_data::State_type::Array;
                } else if (val.is_string()) {
                    iter->state_type = Iterator_data::State_type::String;
                } else if (val.is_nil() && val.depth() == 0) {
                    iter->state_type = Iterator_data::State_type::Empty;
                } else {
                    iter->state_type = Iterator_data::State_type::Singleton;
                }
            };

            if (collection.depth() > 0) {
                Value unwrapped = collection.unwrap_optional();
                setup_iter(unwrapped);
            } else {
                setup_iter(collection);
            }

            registers[base + inst.rrr.dst] = iter_val;
            break;
        }

        case Opcode::Panic: {
            Value msg_val = registers[base + inst.rrr.src_a];
            panic("Panic at IP {}: {}", ip - 1, msg_val.as_string());
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
    case Opcode::BitAnd_i64:
        return a & b;
    case Opcode::BitOr_i64:
        return a | b;
    case Opcode::BitXor_i64:
        return a ^ b;
    case Opcode::Shl_i64:
        return a << b;
    case Opcode::Shr_i64:
        return a >> b;
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
    case Opcode::BitAnd_u64:
        return a & b;
    case Opcode::BitOr_u64:
        return a | b;
    case Opcode::BitXor_u64:
        return a ^ b;
    case Opcode::Shl_u64:
        return a << b;
    case Opcode::Shr_u64:
        return a >> b;
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

template void Virtual_machine::execute_loop<true>(Green_thread_data *);
template void Virtual_machine::execute_loop<false>(Green_thread_data *);

} // namespace phos::vm
