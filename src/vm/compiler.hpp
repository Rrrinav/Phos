#pragma once

#include <vector>
#include <string>
#include "../parser/ast.hpp"
#include "chunk.hpp"
#include "../error/result.hpp"
#include "../value/value.hpp"

namespace phos::vm
{

struct Local
{
    std::string name;
    int depth;
};

struct Compiler_upvalue
{
    uint8_t index;
    bool is_local;
};

// Tracks the chunk and local variables for the currently compiling function
struct Compiler_state
{
    mem::rc_ptr<Closure_value> closure;
    std::vector<Local> locals;
    std::vector<Compiler_upvalue> upvalues;
    int scope_depth = 0;
};

class Compiler
{
public:
    Compiler() = default;

    // The compiler now returns a fully constructed Script function!
    Result<mem::rc_ptr<Closure_value>> compile(const std::vector<ast::Stmt *> &statements);
    std::vector<Compiler_state> states;

    // --- State Accessors ---
    Compiler_state *current() { return &states.back(); }
    Chunk *current_chunk() { return current()->closure->chunk.get(); }

    // --- Scoping & Locals ---
    void begin_scope();
    void end_scope(phos::ast::Source_location loc);
    int resolve_local(const std::string &name);

    // --- Dispatchers ---
    void compile_stmt(ast::Stmt *stmt);
    void compile_expr(ast::Expr *expr);

    // --- Statement Visitors ---
    void visit_print_stmt(const ast::Print_stmt &stmt);
    void visit_expr_stmt(const ast::Expr_stmt &stmt);
    void visit_var_stmt(const ast::Var_stmt &stmt);
    void visit_block_stmt(const ast::Block_stmt &stmt);
    void visit_if_stmt(const ast::If_stmt &stmt);
    void visit_while_stmt(const ast::While_stmt &stmt);
    void visit_for_stmt(const ast::For_stmt &stmt);
    void visit_for_in_stmt(const ast::For_in_stmt &stmt);
    void visit_function_stmt(const ast::Function_stmt &stmt);
    void visit_return_stmt(const ast::Return_stmt &stmt);
    void visit_model_stmt(const ast::Model_stmt &stmt);
    void visit_union_stmt(const ast::Union_stmt &stmt);
    void visit_match_stmt(const ast::Match_stmt &stmt);

    // --- Expression Visitors ---
    void visit_literal_expr(const ast::Literal_expr &expr);
    void visit_binary_expr(const ast::Binary_expr &expr);
    void visit_unary_expr(const ast::Unary_expr &expr);
    void visit_variable_expr(const ast::Variable_expr &expr);
    void visit_assignment_expr(const ast::Assignment_expr &expr);
    void visit_call_expr(const ast::Call_expr &expr);
    void visit_closure_expr(const ast::Closure_expr &expr);
    void visit_model_literal_expr(const ast::Model_literal_expr &expr);
    void visit_field_access_expr(const ast::Field_access_expr &expr);
    void visit_field_assignment_expr(const ast::Field_assignment_expr &expr);
    void visit_method_call_expr(const ast::Method_call_expr &expr);
    void visit_static_path_expr(const ast::Static_path_expr &expr);
    void visit_array_literal_expr(const ast::Array_literal_expr &expr);
    void visit_array_access_expr(const ast::Array_access_expr &expr);
    void visit_array_assignment_expr(const ast::Array_assignment_expr &expr);
    void visit_cast_expr(const ast::Cast_expr &expr);
    void visit_range_expr(const ast::Range_expr &expr);

    // --- Bytecode Emitters ---
    void emit_byte(uint8_t byte, phos::ast::Source_location loc);
    void emit_op(Op_code op, phos::ast::Source_location loc);
    void emit_constant(Value value, phos::ast::Source_location loc);
    uint8_t identifier_constant(const std::string &name, phos::ast::Source_location loc);

    // --- Control Flow Emitters ---
    size_t emit_jump(Op_code instruction, phos::ast::Source_location loc);
    void patch_jump(size_t offset, phos::ast::Source_location loc);
    void emit_loop(size_t loop_start, phos::ast::Source_location loc);

    int resolve_local_in_state(Compiler_state* state, const std::string &name);
    int resolve_upvalue(Compiler_state* state, const std::string &name);
    int add_upvalue(Compiler_state* state, uint8_t index, bool is_local);
};

}  // namespace phos::vm
