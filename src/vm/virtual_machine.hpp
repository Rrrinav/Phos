#pragma once

#include <vector>
#include "chunk.hpp"
#include "../value/value.hpp"

namespace phos::vm {

enum class Interpret_result {
    Ok,
    Runtime_error
};

class Virtual_machine {
public:
    std::string err{};
    Virtual_machine() = default;

    Interpret_result interpret(Chunk* chunk);

private:
    Chunk* chunk = nullptr;
    uint8_t* ip = nullptr;
    std::vector<Value> stack;

    Interpret_result run();

    // Stack operations
    void push(Value value);
    Value pop();
};

} // namespace phos::vm
