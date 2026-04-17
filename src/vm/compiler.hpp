#pragma once

#include "../parser/ast.hpp"
#include "../semantic-checker/type_environment.hpp"
#include "../vm/instruction.hpp"
#include "../value/value.hpp"

#include "../memory/arena.hpp"

#include <vector>

namespace phos::vm {
class Compiler
{

    const ast::Ast_tree &tree;
    const Type_environment &env;
    mem::Arena& arena;
public:
    Compiler(const ast::Ast_tree &tree, const Type_environment &env, mem::Arena& arena_);
    Closure_data compile(const std::vector<ast::Stmt_id> &statements);
    uint8_t allocate_register();
    void reset_registers();

    void emit(vm::Instruction inst);
    uint16_t add_constant(Value val);

    void compile_stmt(ast::Stmt_id stmt_id);
    void compile_stmt_node(const ast::Print_stmt &stmt);
    void compile_stmt_node(const ast::Expr_stmt &stmt);
    void compile_stmt_node(const ast::Var_stmt &stmt);
    void compile_stmt_node(const ast::Block_stmt &stmt);
    void compile_stmt_node(const ast::If_stmt &stmt);

    // Expression traversal
    uint8_t compile_expr(ast::Expr_id expr_id);
    uint8_t compile_expr_node(const ast::Literal_expr &expr);
    uint8_t compile_expr_node(const ast::Binary_expr &expr);
    uint8_t compile_expr_node(const ast::Variable_expr &expr);

public:
    struct Local
    {
        std::string name;
        uint8_t reg;
    };
    std::vector<vm::Instruction> code_;
    std::vector<Value> constants_;
    std::vector<Local> locals_;
    uint8_t current_register_ = 0;
};
} // namespace phos::vm
