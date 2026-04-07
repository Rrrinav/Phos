#pragma once

#include "../memory/ref_counted.hpp"
#include "../value/value.hpp"
#include "opcodes.hpp"

#include <string>
#include <unordered_map>

namespace phos::vm {

class Assembler
{
public:
    // 1. Bytecode -> PhosAsm (Zero Cost Export)
    static std::string serialize(mem::rc_ptr<Closure_value> main_closure);

    // 2. PhosAsm -> Bytecode (Zero Cost Import)
    static mem::rc_ptr<Closure_value> assemble(const std::string &ir_source);

private:
    static std::unordered_map<std::string, Op_code> get_opcode_map();
    static std::unordered_map<Op_code, std::string> get_mnemonic_map();
};

} // namespace phos::vm
