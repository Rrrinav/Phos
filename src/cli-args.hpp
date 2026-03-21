#pragma once

#include "utility/cl.hpp"

namespace phos::cli
{

namespace id
{
inline cl::Opt_id ast;
inline cl::Opt_id ast_print;
inline cl::Opt_id ast_print_unicode;
inline cl::Opt_id file;
inline cl::Opt_id print_ir;
inline cl::Opt_id print_ir_op;
}  // namespace id

void add_arguemnts(cl::Parser &p)
{
    id::ast               = p.add_sub_cmd("ast", "syntax tree related commands", 0);
    id::ast_print         = p.add_sub_cmd("print", "print ast", 0, id::ast);
    id::ast_print_unicode = p.add<cl::Flag>(cl::name("", "unicode"), cl::desc("print without unicode characters"), cl::deflt(false), cl::sub_cmd(id::ast_print));
    id::file              = p.positional<cl::Text>(cl::name("input file"), cl::required());
    // IR and IR_OP are mutually includsive "-o" is only valid if -print_ir is set
    id::print_ir          = p.add<cl::Flag>(cl::name("", "print_ir"), cl::desc("print IR"), cl::deflt(false));
    id::print_ir_op       = p.add<cl::Text>(cl::name("o", "output"), cl::desc("Output file for IR code."), cl::deflt("ir.phosasm"));
}
}  // namespace phos::cli
