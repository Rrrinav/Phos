#include "./virtual_machine.hpp"
#include "../utility/binary_ops.hpp"
#include <format>

namespace phos::vm
{

void Virtual_machine::push(Value value) { stack.push_back(std::move(value)); }

Value Virtual_machine::pop()
{
    Value val = std::move(stack.back());
    stack.pop_back();
    return val;
}

Value Virtual_machine::peek(int distance) const { return stack[stack.size() - 1 - distance]; }

ast::Source_location Virtual_machine::get_loc()
{
    size_t offset = (ip - chunk->code.data()) - 1;
    return chunk->locs[offset];
}

Result<void> Virtual_machine::interpret(Chunk *target_chunk)
{
    chunk = target_chunk;
    ip = chunk->code.data();
    return run();
}

Result<void> Virtual_machine::run()
{
    for (;;)
    {
        uint8_t instruction = *ip++;

        switch (static_cast<Op_code>(instruction))
        {
            case Op_code::Constant:
            {
                uint8_t index = *ip++;
                push(chunk->constants[index]);
                break;
            }

            // --- Unary Ops ---
            case Op_code::Not:
            {
                Value r = pop();
                push(!get_bool(r));
                break;
            }
            case Op_code::BitNot:
            {
                Value r = pop();
                push(~get_int(r));
                break;
            }
            case Op_code::Negate:
            {
                Value r = pop();
                if (is_int(r))
                    push(Value(-get_int(r)));
                else if (is_float(r))
                    push(Value(-get_float(r)));
                else
                    return std::unexpected(err::msg("Operand must be a number for '-'", "vm", get_loc().l, get_loc().c));
                break;
            }

            // --- Binary Arithmetic & Logic Ops ---
            case Op_code::Add:
            {
                if (auto err = execute_binary_op(phos::util::add_op); !err)
                    return err;
                break;
            }
            case Op_code::Subtract:
            {
                if (auto err = execute_binary_op(phos::util::subtract_op); !err)
                    return err;
                break;
            }
            case Op_code::Multiply:
            {
                if (auto err = execute_binary_op(phos::util::multiply_op); !err)
                    return err;
                break;
            }
            case Op_code::Divide:
            {
                if (auto err = execute_binary_op(phos::util::divide_op); !err)
                    return err;
                break;
            }
            case Op_code::Modulo:
            {
                if (auto err = execute_binary_op(phos::util::modulo_op); !err)
                    return err;
                break;
            }

            // --- Relational Ops ---
            case Op_code::Equal:
            {
                if (auto err = execute_binary_op(phos::util::equal_op); !err)
                    return err;
                break;
            }
            case Op_code::Not_equal:
            {
                if (auto err = execute_binary_op(phos::util::not_equal_op); !err)
                    return err;
                break;
            }
            case Op_code::Less:
            {
                if (auto err = execute_binary_op(phos::util::less_op); !err)
                    return err;
                break;
            }
            case Op_code::Less_equal:
            {
                if (auto err = execute_binary_op(phos::util::less_equal_op); !err)
                    return err;
                break;
            }
            case Op_code::Greater:
            {
                if (auto err = execute_binary_op(phos::util::greater_op); !err)
                    return err;
                break;
            }
            case Op_code::Greater_equal:
            {
                if (auto err = execute_binary_op(phos::util::greater_equal_op); !err)
                    return err;
                break;
            }

            // --- Bitwise Ops ---
            case Op_code::BitAnd:
            {
                if (auto err = execute_binary_op(phos::util::bitwise_and_op); !err)
                    return err;
                break;
            }
            case Op_code::BitOr:
            {
                if (auto err = execute_binary_op(phos::util::bitwise_or_op); !err)
                    return err;
                break;
            }
            case Op_code::BitXor:
            {
                if (auto err = execute_binary_op(phos::util::bitwise_xor_op); !err)
                    return err;
                break;
            }
            case Op_code::BitLShift:
            {
                if (auto err = execute_binary_op(phos::util::bitwise_lshift_op); !err)
                    return err;
                break;
            }
            case Op_code::BitRShift:
            {
                if (auto err = execute_binary_op(phos::util::bitwise_rshift_op); !err)
                    return err;
                break;
            }

            // --- Globals ---
            case Op_code::Define_global:
            {
                uint8_t index = *ip++;
                std::string name = get_string(chunk->constants[index]);
                globals[name] = pop();
                break;
            }
            case Op_code::Get_global:
            {
                uint8_t index = *ip++;
                std::string name = get_string(chunk->constants[index]);
                auto it = globals.find(name);
                if (it == globals.end())
                    return std::unexpected(err::msg(std::format("Undefined variable '{}'.", name), "vm", get_loc().l, get_loc().c));
                push(it->second);
                break;
            }
            case Op_code::Set_global:
            {
                uint8_t index = *ip++;
                std::string name = get_string(chunk->constants[index]);
                auto it = globals.find(name);
                if (it == globals.end())
                    return std::unexpected(err::msg(std::format("Undefined variable '{}'.", name), "vm", get_loc().l, get_loc().c));
                it->second = peek(0);
                break;
            }
            case Op_code::Jump_if_false:
            {
                uint16_t offset = (static_cast<uint16_t>(*ip) << 8) | *(ip + 1);
                ip += 2;

                Value cond = peek(0);
                // Phos truthiness: nil is false, actual bools are evaluated, everything else is true
                bool is_true = is_bool(cond) ? get_bool(cond) : !is_nil(cond);

                if (!is_true)
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

            // --- General ---
            case Op_code::Pop:
            {
                pop();
                break;
            }
            case Op_code::Print:
            {
                *config_.out_stream << value_to_string(pop()) << "\n";
                break;
            }
            case Op_code::Return:
            case Op_code::Halt:
            {
                return {};
            }
            default:
            {
                return std::unexpected(err::msg(std::format("Unknown opcode {}", instruction), "vm", get_loc().l, get_loc().c));
            }
        }
    }
}

}  // namespace phos::vm
