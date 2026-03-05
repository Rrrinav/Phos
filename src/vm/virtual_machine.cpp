#include "./virtual_machine.hpp"
#include "../utility/binary_ops.hpp"
#include <print>

namespace phos::vm
{

void Virtual_machine::push(Value value) { stack.push_back(std::move(value)); }

Value Virtual_machine::pop()
{
    Value val = std::move(stack.back());
    stack.pop_back();
    return val;
}

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

Result<void> Virtual_machine::execute_binary_op(Binary_op_func op)
{
    Value r = pop();
    Value l = pop();

    auto res = op(l, r, get_loc());
    if (!res)
        return std::unexpected(res.error());

    push(res.value());
    return {};
}

Result<void> Virtual_machine::run()
{
    for (;;)
    {
        uint8_t instruction = *ip++;

        switch (static_cast<Op_code>(instruction)) {

            case Op_code::Constant: {
                uint8_t index = *ip++;
                push(chunk->constants[index]);
                break;
            } break;

            case Op_code::Not: {
                Value r = pop();
                push(!get_bool(r));
            } break;
            case Op_code::BitNot: {
                Value r = pop();
                push(~get_int(r));
            } break;
            case Op_code::Negate: {
                Value r = pop();
                if (is_int(r))
                    push(Value(-get_int(r)));
                else if (is_float(r))
                    push(Value(-get_float(r)));
                else
                    return std::unexpected(err::msg("Operand must be a number for '-'", "vm", 0, 0));
            } break;

            case Op_code::Add: {
                if (auto err = execute_binary_op(phos::util::add_op); !err)
                    return err;
            } break;
            case Op_code::Subtract: {
                if (auto err = execute_binary_op(phos::util::subtract_op); !err)
                    return err;
            } break;
            case Op_code::Multiply: {
                if (auto err = execute_binary_op(phos::util::multiply_op); !err)
                    return err;
            } break;
            case Op_code::Divide: {
                if (auto err = execute_binary_op(phos::util::divide_op); !err)
                    return err;
            } break;
            case Op_code::BitAnd: {
                if (auto err = execute_binary_op(phos::util::bitwise_and_op); !err)
                    return err;
            } break;
            case Op_code::BitOr: {
                if (auto err = execute_binary_op(phos::util::bitwise_or_op); !err)
                    return err;
            } break;
            case Op_code::BitXor: {
                if (auto err = execute_binary_op(phos::util::bitwise_xor_op); !err)
                    return err;
            } break;
            case Op_code::BitLShift: {
                if (auto err = execute_binary_op(phos::util::bitwise_lshift_op); !err)
                    return err;
            } break;
            case Op_code::BitRShift: {
                if (auto err = execute_binary_op(phos::util::bitwise_rshift_op); !err)
                    return err;
            } break;

            case Op_code::Pop: {
                pop();
            } break;

            case Op_code::Print: {
                std::println("{}", value_to_string(pop()));
                break;
            } break;

            case Op_code::Return:
            case Op_code::Halt: {
                return {};
            } break;

            default: {
                return std::unexpected(err::msg(std::format("Unknown opcode {}", instruction), "vm", 0, 0));
            }
        }
    }
}

}  // namespace phos::vm
