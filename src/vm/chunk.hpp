#pragma once

#include <cstdint>
#include <vector>
#include "opcodes.hpp"
#include "../value/value.hpp"

namespace phos::vm {

struct Chunk
{
    std::vector<uint8_t> code{};
    std::vector<Value> constants{};
    std::vector<size_t> lines{};

    Chunk() = default;

    Chunk(const Chunk &other) = default;
    Chunk(Chunk &&other) noexcept = default;

    Chunk &operator=(const Chunk &other) = default;
    Chunk &operator=(Chunk &&other) noexcept = default;

    void write(uint8_t byte, size_t line)
    {
        code.push_back(byte);
        lines.push_back(line);
    }

    void write(Op_code op, size_t line) { write(static_cast<uint8_t>(op), line); }

    size_t add_constant(Value value)
    {
        constants.push_back(std::move(value));
        return constants.size() - 1;
    }

    void disassemble(std::ostream &out, const std::string &name);
    size_t disassemble_instruction(std::ostream &out, size_t offset);

private:
    size_t simple_instruction(std::ostream &out, const std::string &name, size_t offset);
    size_t constant_instruction(std::ostream &out, const std::string &name, size_t offset);
};

}  // namespace phos::vm
