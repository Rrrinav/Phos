#pragma once

#include <vector>
#include <string>
#include "chunk.hpp"
#include "../value/value.hpp"
#include "../error/result.hpp"
#include "../parser/ast.hpp"

namespace phos::vm {

class Virtual_machine {
public:
    std::string err{};
    Virtual_machine() = default;

    Result<void> interpret(Chunk* target_chunk);

private:
    Chunk* chunk = nullptr;
    uint8_t* ip = nullptr;
    std::vector<Value> stack;

    Result<void> run();

    void push(Value value);
    Value pop();

    ast::Source_location get_loc();

    using Binary_op_func = Result<Value>(*)(const Value&, const Value&, ast::Source_location);
    Result<void> execute_binary_op(Binary_op_func op);
};

} // namespace phos::vm
