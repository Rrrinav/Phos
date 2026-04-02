#include "./virtual_machine.hpp"

#include "../utility/binary_ops.hpp"
#include "../memory/ref_counted.hpp"
#include "../value/value.hpp"
#include <format>

#include "./chunk.hpp"
#include "./opcodes.hpp"

namespace phos::vm
{

void Virtual_machine::push(Value value) { current_thread->stack.push_back(std::move(value)); }

Value Virtual_machine::pop()
{
    Value val = std::move(current_thread->stack.back());
    current_thread->stack.pop_back();
    return val;
}

Value Virtual_machine::peek(int distance) const { return current_thread->stack[current_thread->stack.size() - 1 - distance]; }

ast::Source_location Virtual_machine::get_loc(Call_frame *frame, uint8_t *ip)
{
    size_t offset = (ip - frame->closure->chunk->code.data()) - 1;
    return frame->closure->chunk->locs[offset];
}

Result<void> Virtual_machine::interpret(mem::rc_ptr<Closure_value> script_closure)
{
    current_thread = mem::make_rc<Green_thread_value>();

    push(Value(script_closure));

    Call_frame initial_frame;
    initial_frame.closure = script_closure;
    initial_frame.ip = script_closure->chunk->code.data();
    initial_frame.stack_offset = 0;

    current_thread->frames.push_back(initial_frame);

    return run();
}

Result<void> Virtual_machine::call_closure(mem::rc_ptr<Closure_value> closure, int arg_count, Call_frame *&current_frame, uint8_t *&current_ip)
{
    if (arg_count != closure->arity)
        return std::unexpected(err::msg(std::format("Expected {} arguments but got {}.", closure->arity, arg_count), "vm", 0, 0));

    // --- THE NATIVE C++ INTERCEPT ---
    if (closure->native_func)
    {
        Value result = closure->native_func(this, arg_count);

        // Clean up the stack natively (Drop the args AND the function itself)
        current_thread->stack.resize(current_thread->stack.size() - arg_count - 1);
        push(result);

        return {};  // Return instantly without spinning up a VM frame!
    }
    // ==========================================

    if (current_thread->frames.size() >= 256)
        return std::unexpected(err::msg("Stack overflow.", "vm", 0, 0));

    // Save the old Instruction Pointer before jumping
    current_frame->ip = current_ip;

    Call_frame new_frame;
    new_frame.closure = closure;
    new_frame.ip = closure->chunk->code.data();
    // Start local variables right where the arguments are sitting on the stack
    new_frame.stack_offset = current_thread->stack.size() - arg_count - 1;

    current_thread->frames.push_back(new_frame);

    // Update local cached pointers to instantly switch execution contexts
    current_frame = &current_thread->frames.back();
    current_ip = current_frame->ip;

    return {};
}

Result<void> Virtual_machine::call_value(Value callee, int arg_count, Call_frame *&current_frame, uint8_t *&current_ip)
{
    // Native functions are now unified inside 'Closure_value'
    if (is_closure(callee))
        return call_closure(get_closure(callee), arg_count, current_frame, current_ip);

    return std::unexpected(err::msg("Can only call functions and closures.", "vm", 0, 0));
}

mem::rc_ptr<Upvalue_value> Virtual_machine::capture_upvalue(size_t stack_index)
{
    // 1. If we are already capturing this local variable, return the existing Upvalue!
    for (auto &upvalue : current_thread->open_upvalues)
        if (upvalue->stack_index == stack_index)
            return upvalue;
    // 2. Otherwise, create a new one and track it.
    auto created = mem::make_rc<Upvalue_value>(stack_index);
    current_thread->open_upvalues.push_back(created);
    return created;
}

void Virtual_machine::close_upvalues(size_t last_stack_index)
{
    for (auto it = current_thread->open_upvalues.begin(); it != current_thread->open_upvalues.end();)
    {
        if ((*it)->stack_index >= last_stack_index)
        {
            // Scoop the value off the stack and onto the heap!
            (*it)->closed_value = current_thread->stack[(*it)->stack_index];
            (*it)->is_closed = true;
            it = current_thread->open_upvalues.erase(it);
        }
        else
        {
            ++it;
        }
    }
}

Result<void> Virtual_machine::run()
{
    Call_frame *frame = &current_thread->frames.back();
    uint8_t *ip = frame->ip;

    for (;;)
    {
        uint8_t instruction = *ip++;
#ifdef DEBUG_VM
        std::println(std::cerr, "op: {} stack_size: {}", op_code_to_string(static_cast<Op_code>(instruction)), current_thread->stack.size());
#endif

        switch (static_cast<Op_code>(instruction))
        {
            case Op_code::Constant:
            {
                uint8_t index = *ip++;
                push(frame->closure->chunk->constants[index]);
                break;
            }

            case Op_code::Get_local:
            {
                uint8_t slot = *ip++;
                // Fetch perfectly from THIS function's slice of the stack window
                push(current_thread->stack[frame->stack_offset + slot]);
                break;
            }
            case Op_code::Set_local:
            {
                uint8_t slot = *ip++;
                current_thread->stack[frame->stack_offset + slot] = peek(0);
                break;
            }

            case Op_code::Define_global:
            {
                uint8_t index = *ip++;
                std::string name = get_string(frame->closure->chunk->constants[index]);
                globals[name] = pop();
                break;
            }
            case Op_code::Get_global:
            {
                uint8_t index = *ip++;
                std::string name = get_string(frame->closure->chunk->constants[index]);
                auto it = globals.find(name);
                if (it == globals.end())
                    return std::unexpected(
                    err::msg(std::format("Undefined variable '{}'.", name), "vm", get_loc(frame, ip).l, get_loc(frame, ip).c));
                push(it->second);
                break;
            }
            case Op_code::Set_global:
            {
                uint8_t index = *ip++;
                std::string name = get_string(frame->closure->chunk->constants[index]);
                if (globals.find(name) == globals.end())
                    return std::unexpected(
                    err::msg(std::format("Undefined variable '{}'.", name), "vm", get_loc(frame, ip).l, get_loc(frame, ip).c));
                globals[name] = peek(0);
                break;
            }

            // --- FUNCTION JUMPING ---
            case Op_code::Call:
            {
                uint8_t arg_count = *ip++;
                Value callee = peek(arg_count);

                if (auto err = call_value(callee, arg_count, frame, ip); !err)
                    return err;

                // IMPORTANT: The frame pointer might have been invalidated by vector reallocation!
                // Refresh our local pointers!
                frame = &current_thread->frames.back();
                //ip = frame->ip;
                break;
            }
            case Op_code::Get_upvalue:
            {
                uint8_t slot = *ip++;
                auto upval = frame->closure->upvalues[slot];
                if (upval->is_closed)
                    push(upval->closed_value);
                else
                    push(current_thread->stack[upval->stack_index]);
                break;
            }

            case Op_code::Set_upvalue:
            {
                uint8_t slot = *ip++;
                auto upval = frame->closure->upvalues[slot];
                if (upval->is_closed)
                    upval->closed_value = peek(0);
                else
                    current_thread->stack[upval->stack_index] = peek(0);
                break;
            }

            case Op_code::Make_closure:
            {
                uint8_t constant_idx = *ip++;
                auto func_closure = get_closure(frame->closure->chunk->constants[constant_idx]);

                // Deep copy so each instance gets its own closure scope!
                auto runtime_closure = mem::make_rc<Closure_value>(*func_closure);
                runtime_closure->upvalues.clear();

                for (size_t i = 0; i < func_closure->upvalue_count; ++i)
                {
                    uint8_t is_local = *ip++;
                    uint8_t index = *ip++;
                    if (is_local)
                    {
                        runtime_closure->upvalues.push_back(capture_upvalue(frame->stack_offset + index));
                    }
                    else
                    {
                        // Pass down the upvalue from the parent closure
                        runtime_closure->upvalues.push_back(frame->closure->upvalues[index]);
                    }
                }
                push(Value(runtime_closure));
                break;
            }

            case Op_code::Return:
            {
                Value result = pop();

                close_upvalues(frame->stack_offset);
                size_t old_stack_offset = frame->stack_offset;
                current_thread->frames.pop_back();

                if (current_thread->frames.empty())
                {
                    current_thread->is_completed = true;
                    return {};
                }

                current_thread->stack.resize(old_stack_offset);
                push(result);

                // Refresh our local pointers to the caller's frame!
                frame = &current_thread->frames.back();
                ip = frame->ip;
                break;
            }

            // --- Control Flow ---
            case Op_code::Jump_if_false:
            {
                uint16_t offset = (static_cast<uint16_t>(*ip) << 8) | *(ip + 1);
                ip += 2;
                Value cond = peek(0);
                bool is_true = is_bool(cond) ? get_bool(cond) : !is_nil(cond);
                if (!is_true)
                    ip += offset;
                break;
            }
            case Op_code::Jump_if_true:
            {
                uint16_t offset = (static_cast<uint16_t>(*ip) << 8) | *(ip + 1);
                ip += 2;
                Value cond = peek(0);
                bool is_true = is_bool(cond) ? get_bool(cond) : !is_nil(cond);
                if (is_true)
                    ip += offset;
                break;
            }
            case Op_code::Jump_if_nil:
            {
                uint16_t offset = (static_cast<uint16_t>(*ip) << 8) | *(ip + 1);
                ip += 2;
                if (is_nil(peek(0)))
                    ip += offset;
                break;
            }
            case Op_code::Jump:
            {
                uint16_t offset = (static_cast<uint16_t>(*ip) << 8) | *(ip + 1);
                ip += 2;
                ip += offset;
                break;
            }
            case Op_code::Loop:
            {
                uint16_t offset = (static_cast<uint16_t>(*ip) << 8) | *(ip + 1);
                ip += 2;
                ip -= offset;
                break;
            }

            case Op_code::Construct_model:
            {
                uint8_t field_count = *ip++;
                uint8_t name_idx = *ip++;
                std::string sig_name = get_string(frame->closure->chunk->constants[name_idx]);

                std::vector<Value> fields(field_count);
                // Pop fields backwards because the compiler pushed them left-to-right
                for (int i = field_count - 1; i >= 0; --i) fields[i] = pop();

                // Construct a lightweight signature reference
                types::Model_type sig;
                sig.name = sig_name;
                push(Value(mem::make_rc<Model_value>(sig, std::move(fields))));
                break;
            }

            case Op_code::Get_field:
            {
                uint8_t index = *ip++;
                Value obj = pop();
                auto model_val = get_model(obj);
                push(model_val->fields[index]);
                break;
            }

            case Op_code::Set_field:
            {
                uint8_t index = *ip++;
                Value obj = pop();
                Value val = peek(0);  // The assigned value was pushed first, so it's under the object
                auto model_val = get_model(obj);
                model_val->fields[index] = val;
                // Leave the assigned value right there on the stack as the result of the expression
                break;
            }

            case Op_code::Add:
                if (auto err = execute_binary_op(phos::util::add_op, frame, ip); !err)
                    return err;
                break;
            case Op_code::Subtract:
                if (auto err = execute_binary_op(phos::util::subtract_op, frame, ip); !err)
                    return err;
                break;
            case Op_code::Multiply:
                if (auto err = execute_binary_op(phos::util::multiply_op, frame, ip); !err)
                    return err;
                break;
            case Op_code::Divide:
                if (auto err = execute_binary_op(phos::util::divide_op, frame, ip); !err)
                    return err;
                break;
            case Op_code::Modulo:
                if (auto err = execute_binary_op(phos::util::modulo_op, frame, ip); !err)
                    return err;
                break;
            case Op_code::Equal:
                if (auto err = execute_binary_op(phos::util::equal_op, frame, ip); !err)
                    return err;
                break;
            case Op_code::Not_equal:
                if (auto err = execute_binary_op(phos::util::not_equal_op, frame, ip); !err)
                    return err;
                break;
            case Op_code::Less:
                if (auto err = execute_binary_op(phos::util::less_op, frame, ip); !err)
                    return err;
                break;
            case Op_code::Less_equal:
                if (auto err = execute_binary_op(phos::util::less_equal_op, frame, ip); !err)
                    return err;
                break;
            case Op_code::Greater:
                if (auto err = execute_binary_op(phos::util::greater_op, frame, ip); !err)
                    return err;
                break;
            case Op_code::Greater_equal:
                if (auto err = execute_binary_op(phos::util::greater_equal_op, frame, ip); !err)
                    return err;
                break;
            case Op_code::BitAnd:
                if (auto err = execute_binary_op(phos::util::bitwise_and_op, frame, ip); !err)
                    return err;
                break;
            case Op_code::BitOr:
                if (auto err = execute_binary_op(phos::util::bitwise_or_op, frame, ip); !err)
                    return err;
                break;
            case Op_code::BitXor:
                if (auto err = execute_binary_op(phos::util::bitwise_xor_op, frame, ip); !err)
                    return err;
                break;
            case Op_code::BitLShift:
                if (auto err = execute_binary_op(phos::util::bitwise_lshift_op, frame, ip); !err)
                    return err;
                break;
            case Op_code::BitRShift:
                if (auto err = execute_binary_op(phos::util::bitwise_rshift_op, frame, ip); !err)
                    return err;
                break;

            case Op_code::Not:
                if (auto err = execute_unary_op([](auto v, auto l) { return Result<Value>(Value(!get_bool(v))); }, frame, ip); !err)
                    return err;
                break;
            case Op_code::BitNot:
                if (auto err = execute_unary_op([](auto v, auto l) { return Result<Value>(Value(~get_int(v))); }, frame, ip); !err)
                    return err;
                break;
            case Op_code::Negate:
            {
                Value r = pop();
                if (is_int(r))
                    push(Value(-get_int(r)));
                else if (is_float(r))
                    push(Value(-get_float(r)));
                else
                    return std::unexpected(err::msg("Operand must be a number for '-'", "vm", get_loc(frame, ip).l, get_loc(frame, ip).c));
                break;
            }
            case Op_code::Create_array:
            {
                uint8_t count = *ip++;
                std::vector<Value> elements(count);

                // Pop elements backwards because the compiler pushed them left-to-right
                for (int i = count - 1; i >= 0; --i) elements[i] = pop();

                // Create a lightweight dummy type to satisfy the Array_value constructor
                auto dummy_type = types::Type(mem::make_rc<types::Array_type>(types::Primitive_kind::Any));
                push(Value(mem::make_rc<Array_value>(dummy_type, std::move(elements))));
                break;
            }

            case Op_code::Get_index:
            {
                Value index_val = pop();
                Value array_val = pop();

                auto arr = get_array(array_val);
                int64_t idx = get_int(index_val);

                if (idx < 0 || idx >= static_cast<int64_t>(arr->elements.size()))
                    return std::unexpected(err::msg("Array index out of bounds."));

                push(arr->elements[idx]);
                break;
            }

            case Op_code::Set_index:
            {
                Value index_val = pop();
                Value array_val = pop();
                Value val = peek(0);

                auto arr = get_array(array_val);
                int64_t idx = get_int(index_val);

                if (idx < 0 || idx >= static_cast<int64_t>(arr->elements.size()))
                    return std::unexpected(err::msg("Array assignment index out of bounds."));

                arr->elements[idx] = val;
                // Leave the assigned value on the top of the stack
                break;
            }

            case Op_code::Construct_union:
            {
                uint8_t union_name_idx = *ip++;
                uint8_t variant_name_idx = *ip++;

                std::string union_name = get_string(frame->closure->chunk->constants[union_name_idx]);
                std::string variant_name = get_string(frame->closure->chunk->constants[variant_name_idx]);

                Value payload = pop();

                // Direct allocation! No dummy type needed.
                push(Value(mem::make_rc<Union_value>(std::move(union_name), std::move(variant_name), std::move(payload))));
                break;
            }

            case Op_code::Match_variant:
            {
                uint8_t variant_name_idx = *ip++;
                std::string expected_variant = get_string(frame->closure->chunk->constants[variant_name_idx]);

                // IMPORTANT: Do NOT pop the subject! The match statement needs it for the next arm!
                Value subject = peek(0);

                if (is_union(subject))
                {
                    auto u_val = get_union(subject);
                    if (u_val->variant_name == expected_variant)
                    {
                        push(u_val->payload);  // 1. Push the payload so 's' gets assigned to it
                        push(true);            // 2. Push true so Jump_if_false DOES NOT jump!
                    }
                    else
                    {
                        push(nullptr);  // 1. Push dummy payload
                        push(false);    // 2. Push false so Jump_if_false JUMPS to the next arm
                    }
                }
                else
                {
                    push(nullptr);
                    push(false);
                }
                break;
            }
            case Op_code::Cast:
            {
                uint8_t target_type_byte = *ip++;
                auto target_kind = static_cast<types::Primitive_kind>(target_type_byte);
                Value val = pop();

                switch (target_kind)
                {
                    case types::Primitive_kind::Int:
                        if (is_float(val))
                            push(Value(static_cast<int64_t>(get_float(val))));
                        else if (is_int(val))
                            push(val);  // Already i64
                        else
                            return std::unexpected(err::msg("Invalid cast to i64", "vm", get_loc(frame, ip).l, get_loc(frame, ip).c));
                        break;

                    case types::Primitive_kind::Float:
                        if (is_int(val))
                            push(Value(static_cast<double>(get_int(val))));
                        else if (is_float(val))
                            push(val);  // Already f64
                        else
                            return std::unexpected(err::msg("Invalid cast to f64", "vm", get_loc(frame, ip).l, get_loc(frame, ip).c));
                        break;

                        // As you add i8, u32, etc., just add cases here!

                    default:
                        return std::unexpected(err::msg("Unsupported runtime cast", "vm", get_loc(frame, ip).l, get_loc(frame, ip).c));
                }
                break;
            }

            case Op_code::Pop:
            {
                pop();
                break;
            }
            case Op_code::Nil:
            {
                push(nullptr);
                break;
            }
            case Op_code::True:
            {
                push(true);
                break;
            }
            case Op_code::False:
            {
                push(false);
                break;
            }

            case Op_code::Print:
            {
                *config_.out_stream << value_to_str_debug(pop());
                break;
            }
            case Op_code::Print_err:
            {
                *config_.err_stream << value_to_str_debug(pop());
                break;
            }
            case Op_code::Halt:
            {
                return {};
            }

            default:
                return std::unexpected(
                err::msg(std::format("Unknown opcode {}", instruction), "vm", get_loc(frame, ip).l, get_loc(frame, ip).c));
        }
    }
}

}  // namespace phos::vm
