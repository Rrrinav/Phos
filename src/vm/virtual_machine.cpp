#include "./virtual_machine.hpp"
#include <print>

namespace phos::vm
{

void Virtual_machine::push(Value value)
{
    stack.push_back(std::move(value));
}

Value Virtual_machine::pop()
{
    Value val = std::move(stack.back());
    stack.pop_back();
    return val;
}

Interpret_result Virtual_machine::interpret(Chunk *target_chunk)
{
    chunk = target_chunk;
    ip = chunk->code.data();
    return run();
}

Interpret_result Virtual_machine::run()
{

#define READ_BYTE() (*ip++)
#define READ_CONSTANT() (chunk->constants[READ_BYTE()])

    for (;;)
    {
        uint8_t instruction = READ_BYTE();
        switch (static_cast<Op_code>(instruction))
        {
            case Op_code::Constant:
            {
                Value constant = READ_CONSTANT();
                push(constant);
                break;
            }

            case Op_code::Add:
            {
                Value b = pop();
                Value a = pop();

                if (is_int(a) && is_int(b))
                {
                    push(Value(get_int(a) + get_int(b)));
                }
                else if (is_float(a) && is_float(b))
                {
                    push(Value(get_float(a) + get_float(b)));
                }
                else
                {
                    return Interpret_result::Runtime_error;
                }
                break;
            }

            case Op_code::Multiply: {
                auto a = pop(), b = pop();
                if (is_int(a) && is_int(b))
                {
                    push(Value(get_int(a) + get_int(b)));
                }
                else if (is_float(a) && is_float(b))
                {
                    push(Value(get_float(a) + get_float(b)));
                }
                else
                {
                    this->err = "Not int or float, cant Multiply";
                    return Interpret_result::Runtime_error;
                }
            } break;

            case Op_code::Print:
            {
                std::println("{}", value_to_string(pop()));
                break;
            }

            case Op_code::Return:
            {
                return Interpret_result::Ok;
            }

            case Op_code::Halt:
            {
                return Interpret_result::Ok;
            }

            default:
                return Interpret_result::Runtime_error;
        }
    }

#undef READ_BYTE
#undef READ_CONSTANT
}

}  // namespace phos::vm
