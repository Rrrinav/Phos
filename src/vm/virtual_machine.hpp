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

    Result<void> interpret(Chunk *target_chunk);

    void set_global(const std::string &name, Value val) { globals[name] = std::move(val); }
    std::optional<Value> get_global(const std::string &name) const
    {
        if (auto it = globals.find(name); it != globals.end())
            return it->second;
        return std::nullopt;
    }

private:
    Vm_config config_;
    Chunk *chunk = nullptr;
    uint8_t *ip = nullptr;

    std::vector<Value> stack;
    std::unordered_map<std::string, Value> globals;

    Result<void> run();

    void push(Value value);
    Value pop();
    Value peek(int distance) const;
    ast::Source_location get_loc();

    // Templated to force aggressive inlining of the operator functions
    template <typename Op>
    Result<void> execute_binary_op(Op &&op);

    template <typename Op>
    Result<void> execute_unary_op(Op &&op);
};

// --- Template Implementations ---

template <typename Op>
Result<void> Virtual_machine::execute_binary_op(Op &&op)
{
    Value r = pop();
    Value l = pop();

    auto res = op(l, r, get_loc());
    if (!res)
        return std::unexpected(res.error());

    push(res.value());
    return {};
}

template <typename Op>
Result<void> Virtual_machine::execute_unary_op(Op &&op)
{
    Value v = pop();
    auto res = op(v, get_loc());
    if (!res)
        return std::unexpected(res.error());

    push(res.value());
    return {};
}

}  // namespace phos::vm
