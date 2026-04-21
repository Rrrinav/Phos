#pragma once

#include "../memory/arena.hpp"
#include "../parser/ast.hpp"
#include "../semantic-checker/type_environment.hpp"
#include "../value/value.hpp"
#include "../vm/instruction.hpp"
#include "code_block.hpp"

#include <cstdint>
#include <string>
#include <unordered_map>
#include <vector>

namespace phos::vm {

struct Compiler_upvalue
{
    uint8_t index;
    bool is_local;
};

struct Local
{
    std::string name;
    uint8_t reg;
};

// The isolated universe for a single function being compiled
struct Compiler_context
{
    Compiler_context *enclosing{nullptr};

    Code_block block; // The bytecode being generated for THIS function
    std::vector<Local> locals;
    uint8_t current_register{0};

    // The Shopping List: Variables this function steals from parents
    std::vector<Compiler_upvalue> upvalues;
};

class Compiler
{
    const ast::Ast_tree &tree;
    const Type_environment &env;
    mem::Arena &arena;

    // The currently active compilation context ---
    Compiler_context *current_ctx_{nullptr};

    std::unordered_map<std::string, uint16_t> function_locations_;

    // Helper to grab the active block (replaces the old blocks_ vector)
    Code_block &current_block()
    {
        return current_ctx_->block;
    }

    // Context Management
    void push_context(Compiler_context *ctx);
    void pop_context();

    // Variable Resolution Magic
    int resolve_local(Compiler_context *ctx, const std::string &name);
    int resolve_upvalue(Compiler_context *ctx, const std::string &name);
    int add_upvalue(Compiler_context *ctx, uint8_t index, bool is_local);

public:
    Compiler(const ast::Ast_tree &tree, const Type_environment &env, mem::Arena &arena_);
    Closure_data compile(const std::vector<ast::Stmt_id> &statements);

    // Register management for the current block
    uint8_t allocate_register();
    void reset_registers();

    // Helpers that proxy to the current active code block
    size_t emit(vm::Instruction inst);
    uint16_t add_constant(Value val);
    void emit_numeric_normalize(uint8_t reg, types::Type_id type);
    vm::Opcode cast_opcode_for(types::Type_id type) const;
    vm::Opcode arithmetic_opcode_for(lex::TokenType op, types::Type_id type) const;
    vm::Opcode comparison_opcode_for(lex::TokenType op, types::Type_id left_type, types::Type_id right_type) const;

    // Statement compilation visitors
    void compile_stmt(ast::Stmt_id stmt_id);
    void compile_stmt_node(const ast::Print_stmt &stmt);
    void compile_stmt_node(const ast::Expr_stmt &stmt);
    void compile_stmt_node(const ast::Block_stmt &stmt);
    void compile_stmt_node(const ast::Var_stmt &stmt);
    void compile_stmt_node(const ast::If_stmt &stmt);
    void compile_stmt_node(const ast::While_stmt &stmt);
    void compile_stmt_node(const ast::For_stmt &stmt);
    void compile_stmt_node(const ast::Function_stmt &stmt);
    void compile_stmt_node(const ast::Return_stmt &stmt);

    // Expression compilation visitors. Returns the register holding the result.
    uint8_t compile_expr(ast::Expr_id expr_id);
    uint8_t compile_expr_node(const ast::Literal_expr &expr);
    uint8_t compile_expr_node(const ast::Variable_expr &expr);
    uint8_t compile_expr_node(const ast::Assignment_expr &expr);
    uint8_t compile_expr_node(const ast::Cast_expr &expr);
    uint8_t compile_expr_node(const ast::Binary_expr &expr);
    uint8_t compile_expr_node(const ast::Closure_expr &expr);
    uint8_t compile_expr_node(const ast::Call_expr &expr);
    uint8_t compile_expr_node(const ast::Array_literal_expr &expr);
    uint8_t compile_expr_node(const ast::Array_access_expr &expr);
    uint8_t compile_expr_node(const ast::Array_assignment_expr &expr);

public:
    struct Local
    {
        std::string name;
        uint8_t reg;
    };

    // Tracks lexical variables in the current block
    std::vector<Local> locals_;
    uint8_t current_register_ = 0;
};

} // namespace phos::vm
