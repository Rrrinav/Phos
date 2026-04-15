#pragma once

#include "../parser/ast.hpp"
#include "../value/value.hpp"
#include "opcodes.hpp"

#include <cstdint>
#include <iostream>
#include <string>
#include <vector>

namespace phos::vm {

struct Chunk
{
    std::vector<uint8_t> code{};
    std::vector<Value> constants{};
    std::vector<phos::ast::Source_location> locs{};

    Chunk() = default;
    Chunk(const Chunk &other) = default;
    Chunk(Chunk &&other) noexcept = default;
    Chunk &operator=(const Chunk &other) = default;
    Chunk &operator=(Chunk &&other) noexcept = default;

    void write(uint8_t byte, phos::ast::Source_location loc)
    {
        code.push_back(byte);
        locs.push_back(loc);
    }

    void write(Op_code op, phos::ast::Source_location loc)
    {
        write(static_cast<uint8_t>(op), loc);
    }

    size_t add_constant(Value value)
    {
        constants.push_back(std::move(value));
        return constants.size() - 1;
    }

    // Optional: Keep this if you want quick runtime terminal debugging
    void disassemble(std::ostream &out, const std::string &name);
    size_t disassemble_instruction(std::ostream &out, size_t offset);

private:
    std::string format_instruction(size_t offset, const std::string &mnemonic, const std::string &operands, const std::string &comment);

    size_t simple_instruction(std::ostream &out, const std::string &name, size_t offset);
    size_t constant_instruction(std::ostream &out, const std::string &name, size_t offset);
    size_t jump_instruction(std::ostream &out, const std::string &name, int sign, size_t offset);
    size_t local_instruction(std::ostream &out, const std::string &name, size_t offset);
};

} // namespace phos::vm
