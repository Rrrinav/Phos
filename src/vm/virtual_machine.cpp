#include "./virtual_machine.hpp"
#include "../utility/binary_ops.hpp"
#include <format>

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

Result<void> Virtual_machine::call_closure(mem::rc_ptr<Closure_value> closure,
                                           int arg_count,
                                           Call_frame *&current_frame,
                                           uint8_t *&current_ip)
{
    if (arg_count != closure->arity)
        return std::unexpected(err::msg(std::format("Expected {} arguments but got {}.", closure->arity, arg_count), "vm", 0, 0));

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
    if (is_closure(callee))
        return call_closure(get_closure(callee), arg_count, current_frame, current_ip);
    return std::unexpected(err::msg("Can only call functions and closures.", "vm", 0, 0));
}

Result<void> Virtual_machine::run()
{
    Call_frame *frame = &current_thread->frames.back();
    uint8_t *ip = frame->ip;

    for (;;)
    {
        uint8_t instruction = *ip++;

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
                break;
            }

            case Op_code::Return:
            {
                Value result = pop();

                size_t old_stack_offset = frame->stack_offset;
                current_thread->frames.pop_back();

                if (current_thread->frames.empty())
                {
                    current_thread->is_completed = true;
                    return {};
                }

                // Erase all locals and parameters for the function that just finished
                current_thread->stack.resize(old_stack_offset);
                push(result);

                // Resume the outer function exactly where it left off
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
                *config_.out_stream << value_to_string(pop()) << "\n";
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
