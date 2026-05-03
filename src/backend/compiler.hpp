#pragma once

#include "core/instruction/instruction.hpp"
#include "core/value/value.hpp"
#include "frontend/environment/compiler_context.hpp"
#include "virtual_machine/code_block.hpp"

#include <cstdint>
#include <string>
#include <string_view>
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
struct Function_context
{
    Function_context *enclosing{nullptr};

    Code_block block; // The bytecode being generated for THIS function
    std::vector<Local> locals;
    uint8_t current_register{0};

    // The Shopping List: Variables this function steals from parents
    std::vector<Compiler_upvalue> upvalues;
};

class Compiler
{
    phos::Compiler_context &ctx;

    // The currently active compilation context
    Function_context *current_ctx_{nullptr};
    Function_context *root_ctx_{nullptr};

    std::unordered_map<Symbol_id, Closure_data*> function_locations_;
    std::string current_module_ns_;

    // Helper to grab the active block
    Code_block &current_block()
    {
        return current_ctx_->block;
    }

    // Context Management
    void push_context(Function_context *ctx);
    void pop_context();
    uint16_t function_constant_index(Symbol_id id);
    std::string canonical_function_name(const ast::Function_stmt &stmt) const;
    void set_closure_name(Closure_data &closure, std::string_view name);
    void hoist_function_placeholder(Symbol_id id);
    void hoist_module_functions(const Module_unit &module);

    // Variable Resolution Magic
    int resolve_local(Function_context *ctx, const std::string &name);
    int resolve_upvalue(Function_context *ctx, const std::string &name);
    int add_upvalue(Function_context *ctx, uint8_t index, bool is_local);

public:
    explicit Compiler(phos::Compiler_context &ctx);
    Closure_data compile(const std::vector<ast::Stmt_id> &statements);
    Closure_data compile_workspace(Module_id main_mod);

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
    vm::Opcode bitwise_opcode_for(lex::TokenType op, types::Type_id type) const;

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
    void compile_stmt_node(const ast::Model_stmt &stmt);
    void compile_stmt_node(const ast::Union_stmt &stmt);
    void compile_stmt_node(const ast::Enum_stmt &stmt);
    void compile_stmt_node(const ast::Match_stmt &stmt);

    // Expression compilation visitors. Returns the register holding the result.
    uint8_t compile_expr(ast::Expr_id expr_id);
    uint8_t compile_expr_node(const ast::Literal_expr &expr);
    uint8_t compile_expr_node(const ast::Variable_expr &expr);
    uint8_t compile_expr_node(const ast::Assignment_expr &expr);
    uint8_t compile_expr_node(const ast::Cast_expr &expr);
    uint8_t compile_expr_node(const ast::Binary_expr &expr);
    uint8_t compile_expr_node(const ast::Unary_expr &expr);
    uint8_t compile_expr_node(const ast::Closure_expr &expr);
    uint8_t compile_expr_node(const ast::Call_expr &expr);
    uint8_t compile_expr_node(const ast::Array_literal_expr &expr);
    uint8_t compile_expr_node(const ast::Array_access_expr &expr);
    uint8_t compile_expr_node(const ast::Array_assignment_expr &expr);
    uint8_t compile_expr_node(const ast::Field_access_expr &expr);
    uint8_t compile_expr_node(const ast::Model_literal_expr &expr);
    uint8_t compile_expr_node(const ast::Anon_model_literal_expr &expr);
    uint8_t compile_expr_node(const ast::Method_call_expr &expr);
    uint8_t compile_expr_node(const ast::Field_assignment_expr &expr);
    uint8_t compile_expr_node(const ast::Enum_member_expr &expr);
    uint8_t compile_expr_node(const ast::Static_path_expr &expr);
    uint8_t compile_expr_node(const ast::Range_expr &expr);
    void compile_stmt_node(const ast::For_in_stmt &stmt);

public:
    // Tracks lexical variables in the current block
    std::vector<Local> locals_;
    uint8_t current_register_ = 0;
};

} // namespace phos::vm
