#pragma once

#include <vector>
#include <string>
#include <unordered_map>
#include <iostream>
#include <optional>

#include "chunk.hpp"
#include "../value/value.hpp"
#include "../error/result.hpp"
#include "../parser/ast.hpp"

namespace phos::vm
{

struct Vm_config
{
    std::ostream *out_stream = &std::cout;
    std::ostream *err_stream = &std::cerr;
};

class Virtual_machine
{
public:
    std::string err{};

    Virtual_machine(Vm_config config = {}) : config_(config) {}

    // THE UPGRADE: Interpret now takes the top-level compiled Script closure
    Result<void> interpret(mem::rc_ptr<Closure_value> script_closure);

    void set_global(const std::string &name, Value val) { globals[name] = std::move(val); }
    std::optional<Value> get_global(const std::string &name) const
    {
        if (auto it = globals.find(name); it != globals.end())
            return it->second;
        return std::nullopt;
    }

private:
    Vm_config config_;

    // THE UPGRADE: The currently executing thread and its Call Frames
    mem::rc_ptr<Green_thread_value> current_thread;

    std::unordered_map<std::string, Value> globals;

    Result<void> run();

    void push(Value value);
    Value pop();
    Value peek(int distance) const;

    // Call Frame Managers
    Result<void> call_value(Value callee, int arg_count, Call_frame *&current_frame, uint8_t *&current_ip);
    Result<void> call_closure(mem::rc_ptr<Closure_value> closure, int arg_count, Call_frame *&current_frame, uint8_t *&current_ip);

    ast::Source_location get_loc(Call_frame *frame, uint8_t *ip);

    template <typename Op>
    Result<void> execute_binary_op(Op &&op, Call_frame *frame, uint8_t *ip);

    template <typename Op>
    Result<void> execute_unary_op(Op &&op, Call_frame *frame, uint8_t *ip);
};

template <typename Op>
Result<void> Virtual_machine::execute_binary_op(Op &&op, Call_frame *frame, uint8_t *ip)
{
    Value r = pop();
    Value l = pop();
    auto res = op(l, r, get_loc(frame, ip));
    if (!res)
        return std::unexpected(res.error());
    push(res.value());
    return {};
}

template <typename Op>
Result<void> Virtual_machine::execute_unary_op(Op &&op, Call_frame *frame, uint8_t *ip)
{
    Value v = pop();
    auto res = op(v, get_loc(frame, ip));
    if (!res)
        return std::unexpected(res.error());
    push(res.value());
    return {};
}

}  // namespace phos::vm
