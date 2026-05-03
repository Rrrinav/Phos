#include "virtual_machine.hpp"

#include "backend/assembler.hpp"

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
            std::println(*cfg.out, "TRACE: {:04} | {}", ip, disassembled);
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
            // Note: For Store_global, inst.ri.dst actually holds our SOURCE register
            globals[global_idx] = registers[base + inst.ri.dst];
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
        case Opcode::Mod_u64:
        case Opcode::BitAnd_u64:
        case Opcode::BitOr_u64:
        case Opcode::BitXor_u64:
        case Opcode::Shl_u64:
        case Opcode::Shr_u64: {

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
                Value result = (*target_closure->native_func)(this->arena, args);
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

            if (stream_flag == 1) {
                *this->cfg.err << result.to_debug_string();
            } else {
                *this->cfg.out << result.to_debug_string();
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

            // Allocate a new array on the Arena
            Value arr_val = Value::make_array(arena, str->length, 0);
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
                // Grab the lower 8 bits and push them directly into the string buffer
                s.push_back(static_cast<char>(arr->elements[i].as_int()));
            }

            // Allocate the new string on the Arena
            registers[base + inst.rrr.dst] = Value::make_string(arena, s);
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
                // Extract 1 character and allocate a new string for it
                registers[base + inst.rrr.dst] = Value::make_string(arena, std::string_view(&str->chars[index], 1));

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
            Value model_val = Value::make_model(arena, {}, count);

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

            registers[base + inst.rrr.dst] =
                Value::make_union(arena, union_name_val.as_string_data(), variant_name_val.as_string_data(), payload_val);
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

            // Only pure nil means None
            if (val.is_nil() && val.depth() == 0) {
                panic("Attempted to unwrap a nil value at IP: {}", ip - 1);
            }

            registers[base + inst.rrr.dst] = val.unwrap_optional();
            break;
        }
        case Opcode::Test_nil: {
            Value val = registers[base + inst.rrr.src_a];
            // It is only "empty" if it is Depth 1 or 0, AND the core payload is nil.
            registers[base + inst.rrr.dst] = Value(val.depth() <= 1 && val.is_nil());
            break;
        }
        case Opcode::Test_val: {
            Value src = registers[base + inst.rrr.src_a];
            // It HAS a value if it's a nested container (Depth > 1) OR the core payload is not nil.
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
                valid = iter->state.interval.inclusive ? (iter->cursor <= iter->state.interval.end) : (iter->cursor < iter->state.interval.end);
                break;
            case Iterator_data::State_type::Array:
                valid = (iter->cursor < static_cast<int64_t>(iter->state.array->count));
                break;
            case Iterator_data::State_type::String:
                valid = (iter->cursor < static_cast<int64_t>(iter->state.string->length));
                break;
            }

            if (!valid) {
                registers[base + inst.rrr.dst] = Value(); // Return pure nil (depth=0) to signal end
            } else {
                Value val;
                switch (iter->state_type) {
                case Iterator_data::State_type::Singleton:
                    val = *(iter->state.singleton_val);
                    break;
                case Iterator_data::State_type::Interval:
                    val = Value(iter->cursor);
                    break;
                case Iterator_data::State_type::Array:
                    val = iter->state.array->elements[iter->cursor];
                    break;
                case Iterator_data::State_type::String:
                    val = Value::make_string(arena, std::string_view(&iter->state.string->chars[iter->cursor], 1));
                    break;
                default:
                    std::unreachable();
                }
                // FIXED: Wrap the value to indicate "has value", but preserve what's inside
                // This means Some(nil) if array element is nil, Some(5) if array element is 5
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
                valid = (iter->cursor >= iter->state.interval.start);
                break;
            case Iterator_data::State_type::Array:
                valid = (iter->cursor >= 0);
                break;
            case Iterator_data::State_type::String:
                valid = (iter->cursor >= 0);
                break;
            }

            if (!valid) {
                registers[base + inst.rrr.dst] = Value(); // Return pure nil
            } else {
                Value val;
                switch (iter->state_type) {
                case Iterator_data::State_type::Singleton:
                    val = *(iter->state.singleton_val);
                    break;
                case Iterator_data::State_type::Interval:
                    val = Value(iter->cursor);
                    break;
                case Iterator_data::State_type::Array:
                    val = iter->state.array->elements[iter->cursor];
                    break;
                case Iterator_data::State_type::String:
                    val = Value::make_string(arena, std::string_view(&iter->state.string->chars[iter->cursor], 1));
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

            Value iter_val = Value::make_iterator(arena);
            Iterator_data *iter = iter_val.as_iterator();

            iter->state_type = Iterator_data::State_type::Interval;
            iter->state.interval.start = start;
            iter->state.interval.end = end;
            iter->state.interval.inclusive = (inst.rrr.op == Opcode::Make_range_in);
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

            Value iter_val = Value::make_iterator(arena);
            Iterator_data *iter = iter_val.as_iterator();

            iter->cursor = -1;

            if (collection.is_array()) {
                iter->state_type = Iterator_data::State_type::Array;
                iter->state.array = collection.as_array();
            } else if (collection.is_string()) {
                iter->state_type = Iterator_data::State_type::String;
                iter->state.string = collection.as_string_data();
            } else if (collection.depth() > 0) {
                Value unwrapped = collection.unwrap_optional();
                if (unwrapped.is_nil()) {
                    iter->state_type = Iterator_data::State_type::Empty;
                } else {
                    // The unwrapped value might itself be an array, string, or singleton
                    // Recursively handle it
                    if (unwrapped.is_array()) {
                        iter->state_type = Iterator_data::State_type::Array;
                        iter->state.array = unwrapped.as_array();
                    } else if (unwrapped.is_string()) {
                        iter->state_type = Iterator_data::State_type::String;
                        iter->state.string = unwrapped.as_string_data();
                    } else {
                        // Singleton (including numbers, models, etc.)
                        iter->state_type = Iterator_data::State_type::Singleton;
                        iter->state.singleton_val = arena.allocate<Value>();
                        *(iter->state.singleton_val) = unwrapped;
                    }
                }
            } else if (collection.is_nil()) {
                // Pure nil (depth=0) creates an empty iterator (0 iterations)
                iter->state_type = Iterator_data::State_type::Empty;
            } else {
                // Singleton: iterate once over this value
                iter->state_type = Iterator_data::State_type::Singleton;
                iter->state.singleton_val = arena.allocate<Value>();
                *(iter->state.singleton_val) = collection;
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

// Explicit template instantiations so the linker can find both versions
template void Virtual_machine::execute_loop<true>(Green_thread_data *);
template void Virtual_machine::execute_loop<false>(Green_thread_data *);

} // namespace phos::vm
