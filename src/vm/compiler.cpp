#include "compiler.hpp"
#include <print>
#include "opcodes.hpp"

namespace phos::vm
{

Result<mem::rc_ptr<Closure_value>> Compiler::compile(const std::vector<ast::Stmt *> &statements)
{
    states.clear();

    // The top-level script is just a giant function!
    types::Function_type main_sig;
    main_sig.return_type = types::Primitive_kind::Void;
    auto main_chunk = mem::make_rc<Chunk>();
    auto main_closure = mem::make_rc<Closure_value>("<main>", 0, main_sig, main_chunk);

    states.push_back({main_closure, {}, 0});

    // Stack Slot 0 is reserved for the running function itself
    current()->locals.push_back(Local{"<script>", 0});

    for (auto *stmt : statements)
        if (stmt)
            compile_stmt(stmt);

    // Implicitly return nil at the end of the script
    emit_op(Op_code::Nil, {0, 0});
    emit_op(Op_code::Return, {0, 0});

    auto finished_script = current()->closure;
    states.pop_back();
    return finished_script;
}

// ============================================================================
// Scoping & Locals
// ============================================================================

void Compiler::begin_scope() { current()->scope_depth++; }

void Compiler::end_scope(phos::ast::Source_location loc)
{
    current()->scope_depth--;
    while (!current()->locals.empty() && current()->locals.back().depth > current()->scope_depth)
    {
        emit_op(Op_code::Pop, loc);
        current()->locals.pop_back();
    }
}

int Compiler::resolve_local(const std::string &name)
{
    for (int i = static_cast<int>(current()->locals.size()) - 1; i >= 0; i--)
        if (current()->locals[i].name == name)
            return i;
    return -1;
}

// Dispatchers
void Compiler::compile_stmt(ast::Stmt *stmt)
{
    if (!stmt)
        return;
    if (auto *node = std::get_if<ast::Print_stmt>(&stmt->node))
        visit_print_stmt(*node);
    else if (auto *node = std::get_if<ast::Expr_stmt>(&stmt->node))
        visit_expr_stmt(*node);
    else if (auto *node = std::get_if<ast::Var_stmt>(&stmt->node))
        visit_var_stmt(*node);
    else if (auto *node = std::get_if<ast::Block_stmt>(&stmt->node))
        visit_block_stmt(*node);
    else if (auto *node = std::get_if<ast::If_stmt>(&stmt->node))
        visit_if_stmt(*node);
    else if (auto *node = std::get_if<ast::While_stmt>(&stmt->node))
        visit_while_stmt(*node);
    else if (auto *node = std::get_if<ast::For_stmt>(&stmt->node))
        visit_for_stmt(*node);
    else if (auto *node = std::get_if<ast::Function_stmt>(&stmt->node))
        visit_function_stmt(*node);
    else if (auto *node = std::get_if<ast::Return_stmt>(&stmt->node))
        visit_return_stmt(*node);
    else if (auto *node = std::get_if<ast::Model_stmt>(&stmt->node))
        visit_model_stmt(*node);
    else
        std::println(stderr, "Unimplemented stmt node at index: {}", stmt->node.index());
}

void Compiler::compile_expr(ast::Expr *expr)
{
    if (!expr)
        return;
    if (auto *node = std::get_if<ast::Literal_expr>(&expr->node))
        visit_literal_expr(*node);
    else if (auto *node = std::get_if<ast::Binary_expr>(&expr->node))
        visit_binary_expr(*node);
    else if (auto *node = std::get_if<ast::Unary_expr>(&expr->node))
        visit_unary_expr(*node);
    else if (auto *node = std::get_if<ast::Variable_expr>(&expr->node))
        visit_variable_expr(*node);
    else if (auto *node = std::get_if<ast::Assignment_expr>(&expr->node))
        visit_assignment_expr(*node);
    else if (auto *node = std::get_if<ast::Call_expr>(&expr->node))
        visit_call_expr(*node);
    else if (auto *node = std::get_if<ast::Closure_expr>(&expr->node))
        visit_closure_expr(*node);
    else if (auto *node = std::get_if<ast::Model_literal_expr>(&expr->node))
        visit_model_literal_expr(*node);
    else if (auto *node = std::get_if<ast::Field_access_expr>(&expr->node))
        visit_field_access_expr(*node);
    else if (auto *node = std::get_if<ast::Field_assignment_expr>(&expr->node))
        visit_field_assignment_expr(*node);
    else if (auto *node = std::get_if<ast::Method_call_expr>(&expr->node))
        visit_method_call_expr(*node);
    else
        std::println(stderr, "Unimplemented expr node at index: {}", expr->node.index());
}

// Call Frames & Functions
void Compiler::visit_function_stmt(const ast::Function_stmt &stmt)
{
    // 1. Build the Signature
    types::Function_type sig;
    for (const auto &p : stmt.parameters) sig.parameter_types.push_back(p.type);
    sig.return_type = stmt.return_type;

    // 2. Setup isolated Chunk and State
    auto chunk = mem::make_rc<Chunk>();
    auto closure = mem::make_rc<Closure_value>(stmt.name, stmt.parameters.size(), sig, chunk);
    states.push_back({closure, {}, 0});

    // 3. Slot 0 is always the function object itself
    current()->locals.push_back(Local{stmt.name, 0});

    // 4. Define parameters as standard local variables (Slots 1, 2, 3...)
    for (const auto &p : stmt.parameters) current()->locals.push_back(Local{p.name, 0});

    // 5. Compile the body
    compile_stmt(stmt.body);

    // 6. Ensure every function safely returns
    emit_op(Op_code::Nil, stmt.loc);
    emit_op(Op_code::Return, stmt.loc);

    // 7. Pop the finished compiler state
    auto finished_closure = current()->closure;
    states.pop_back();

    // 8. Emit the new function as a Constant in the OUTER scope
    size_t idx = current_chunk()->add_constant(Value(finished_closure));
    emit_op(Op_code::Constant, stmt.loc);
    emit_byte(idx, stmt.loc);

    // 9. Bind it to the variable name!
    if (current()->scope_depth == 0)
    {
        uint8_t name_idx = identifier_constant(stmt.name, stmt.loc);
        emit_op(Op_code::Define_global, stmt.loc);
        emit_byte(name_idx, stmt.loc);
    }
    else
    {
        current()->locals.push_back(Local{stmt.name, current()->scope_depth});
    }
}

void Compiler::visit_closure_expr(const ast::Closure_expr &expr)
{
    types::Function_type sig;
    for (const auto &p : expr.parameters) sig.parameter_types.push_back(p.type);
    sig.return_type = expr.return_type;

    auto chunk = mem::make_rc<Chunk>();
    auto closure = mem::make_rc<Closure_value>("<closure>", expr.parameters.size(), sig, chunk);
    states.push_back({closure, {}, 0});

    current()->locals.push_back(Local{"<closure>", 0});
    for (const auto &p : expr.parameters) current()->locals.push_back(Local{p.name, 0});

    compile_stmt(expr.body);

    emit_op(Op_code::Nil, expr.loc);
    emit_op(Op_code::Return, expr.loc);

    auto finished_closure = current()->closure;
    states.pop_back();

    size_t idx = current_chunk()->add_constant(Value(finished_closure));
    emit_op(Op_code::Constant, expr.loc);
    emit_byte(idx, expr.loc);
}

void Compiler::visit_call_expr(const ast::Call_expr &expr)
{
    // First, push the function object
    compile_expr(expr.callee);

    // Then, push all the arguments in order
    for (auto *arg : expr.arguments) compile_expr(arg);

    // Emit the magical Call opcode with the arity!
    emit_op(Op_code::Call, expr.loc);
    emit_byte(expr.arguments.size(), expr.loc);
}

void Compiler::visit_return_stmt(const ast::Return_stmt &stmt)
{
    if (stmt.expression)
        compile_expr(stmt.expression);
    else
        emit_op(Op_code::Nil, stmt.loc);  // Return nil if empty

    emit_op(Op_code::Return, stmt.loc);
}

// ============================================================================
// Standard Statement/Expr Visitors (Same as before, routing to current_chunk)
// ============================================================================

void Compiler::visit_print_stmt(const ast::Print_stmt &stmt)
{
    compile_expr(stmt.expression);
    emit_op(Op_code::Print, stmt.loc);
}
void Compiler::visit_expr_stmt(const ast::Expr_stmt &stmt)
{
    compile_expr(stmt.expression);
    emit_op(Op_code::Pop, stmt.loc);
}
void Compiler::visit_literal_expr(const ast::Literal_expr &expr) { emit_constant(expr.value, expr.loc); }

void Compiler::visit_var_stmt(const ast::Var_stmt &stmt)
{
    if (stmt.initializer)
        compile_expr(stmt.initializer);
    else
        emit_op(Op_code::Nil, stmt.loc);

    if (current()->scope_depth == 0)
    {
        uint8_t name_idx = identifier_constant(stmt.name, stmt.loc);
        emit_op(Op_code::Define_global, stmt.loc);
        emit_byte(name_idx, stmt.loc);
    }
    else
    {
        current()->locals.push_back(Local{stmt.name, current()->scope_depth});
    }
}

void Compiler::visit_block_stmt(const ast::Block_stmt &stmt)
{
    begin_scope();
    for (auto *s : stmt.statements) compile_stmt(s);
    end_scope(stmt.loc);
}

void Compiler::visit_if_stmt(const ast::If_stmt &stmt)
{
    compile_expr(stmt.condition);
    size_t then_jump = emit_jump(Op_code::Jump_if_false, stmt.loc);
    emit_op(Op_code::Pop, stmt.loc);
    compile_stmt(stmt.then_branch);
    size_t else_jump = emit_jump(Op_code::Jump, stmt.loc);
    patch_jump(then_jump, stmt.loc);
    emit_op(Op_code::Pop, stmt.loc);
    if (stmt.else_branch)
        compile_stmt(stmt.else_branch);
    patch_jump(else_jump, stmt.loc);
}

void Compiler::visit_while_stmt(const ast::While_stmt &stmt)
{
    size_t loop_start = current_chunk()->code.size();
    compile_expr(stmt.condition);
    size_t exit_jump = emit_jump(Op_code::Jump_if_false, stmt.loc);
    emit_op(Op_code::Pop, stmt.loc);
    compile_stmt(stmt.body);
    emit_loop(loop_start, stmt.loc);
    patch_jump(exit_jump, stmt.loc);
    emit_op(Op_code::Pop, stmt.loc);
}

void Compiler::visit_for_stmt(const ast::For_stmt &stmt)
{
    begin_scope();
    if (stmt.initializer)
        compile_stmt(stmt.initializer);
    size_t loop_start = current_chunk()->code.size();
    size_t exit_jump = -1;
    if (stmt.condition)
    {
        compile_expr(stmt.condition);
        exit_jump = emit_jump(Op_code::Jump_if_false, stmt.loc);
        emit_op(Op_code::Pop, stmt.loc);
    }
    if (stmt.body)
        compile_stmt(stmt.body);
    if (stmt.increment)
    {
        compile_expr(stmt.increment);
        emit_op(Op_code::Pop, stmt.loc);
    }
    emit_loop(loop_start, stmt.loc);
    if (stmt.condition)
    {
        patch_jump(exit_jump, stmt.loc);
        emit_op(Op_code::Pop, stmt.loc);
    }
    end_scope(stmt.loc);
}

void Compiler::visit_model_stmt(const ast::Model_stmt &stmt)
{
    // Compile every method bound to this model
    for (const auto *method : stmt.methods)
    {
        types::Function_type sig;
        for (const auto &p : method->parameters) sig.parameter_types.push_back(p.type);
        sig.return_type = method->return_type;

        auto chunk = mem::make_rc<Chunk>();
        std::string global_name = stmt.name + "::" + method->name;

        // Arity accounts for the hidden 'this' parameter if it's an instance method
        size_t arity = method->parameters.size() + (method->is_static ? 0 : 1);
        auto closure = mem::make_rc<Closure_value>(global_name, arity, sig, chunk);
        states.push_back({closure, {}, 0});

        // Slot 0: The function itself
        current()->locals.push_back(Local{global_name, 0});

        // Slot 1: 'this' (if instance method)
        if (!method->is_static)
            current()->locals.push_back(Local{"this", 0});

        for (const auto &p : method->parameters) current()->locals.push_back(Local{p.name, 0});

        compile_stmt(method->body);

        emit_op(Op_code::Nil, method->loc);
        emit_op(Op_code::Return, method->loc);

        auto finished_closure = current()->closure;
        states.pop_back();

        size_t idx = current_chunk()->add_constant(Value(finished_closure));
        emit_op(Op_code::Constant, method->loc);
        emit_byte(idx, method->loc);

        uint8_t name_idx = identifier_constant(global_name, method->loc);
        emit_op(Op_code::Define_global, method->loc);
        emit_byte(name_idx, method->loc);
    }
}

void Compiler::visit_variable_expr(const ast::Variable_expr &expr)
{
    int arg = resolve_local(expr.name);
    if (arg != -1)
    {
        emit_op(Op_code::Get_local, expr.loc);
        emit_byte(static_cast<uint8_t>(arg), expr.loc);
    }
    else
    {
        uint8_t name_idx = identifier_constant(expr.name, expr.loc);
        emit_op(Op_code::Get_global, expr.loc);
        emit_byte(name_idx, expr.loc);
    }
}

void Compiler::visit_assignment_expr(const ast::Assignment_expr &expr)
{
    compile_expr(expr.value);
    int arg = resolve_local(expr.name);
    if (arg != -1)
    {
        emit_op(Op_code::Set_local, expr.loc);
        emit_byte(static_cast<uint8_t>(arg), expr.loc);
    }
    else
    {
        uint8_t name_idx = identifier_constant(expr.name, expr.loc);
        emit_op(Op_code::Set_global, expr.loc);
        emit_byte(name_idx, expr.loc);
    }
}

void Compiler::visit_binary_expr(const ast::Binary_expr &expr)
{
    if (expr.op == lex::TokenType::LogicalAnd)
    {
        compile_expr(expr.left);
        size_t end_jump = emit_jump(Op_code::Jump_if_false, expr.loc);
        emit_op(Op_code::Pop, expr.loc);
        compile_expr(expr.right);
        patch_jump(end_jump, expr.loc);
        return;
    }
    else if (expr.op == lex::TokenType::LogicalOr)
    {
        compile_expr(expr.left);
        size_t end_jump = emit_jump(Op_code::Jump_if_true, expr.loc);
        emit_op(Op_code::Pop, expr.loc);
        compile_expr(expr.right);
        patch_jump(end_jump, expr.loc);
        return;
    }

    compile_expr(expr.left);
    compile_expr(expr.right);
    switch (expr.op)
    {
        case lex::TokenType::Plus:
            emit_op(Op_code::Add, expr.loc);
            break;
        case lex::TokenType::Minus:
            emit_op(Op_code::Subtract, expr.loc);
            break;
        case lex::TokenType::Star:
            emit_op(Op_code::Multiply, expr.loc);
            break;
        case lex::TokenType::Slash:
            emit_op(Op_code::Divide, expr.loc);
            break;
        case lex::TokenType::Percent:
            emit_op(Op_code::Modulo, expr.loc);
            break;
        case lex::TokenType::Equal:
            emit_op(Op_code::Equal, expr.loc);
            break;
        case lex::TokenType::NotEqual:
            emit_op(Op_code::Not_equal, expr.loc);
            break;
        case lex::TokenType::Less:
            emit_op(Op_code::Less, expr.loc);
            break;
        case lex::TokenType::LessEqual:
            emit_op(Op_code::Less_equal, expr.loc);
            break;
        case lex::TokenType::Greater:
            emit_op(Op_code::Greater, expr.loc);
            break;
        case lex::TokenType::GreaterEqual:
            emit_op(Op_code::Greater_equal, expr.loc);
            break;
        case lex::TokenType::BitAnd:
            emit_op(Op_code::BitAnd, expr.loc);
            break;
        case lex::TokenType::Pipe:
            emit_op(Op_code::BitOr, expr.loc);
            break;
        case lex::TokenType::BitXor:
            emit_op(Op_code::BitXor, expr.loc);
            break;
        case lex::TokenType::BitRshift:
            emit_op(Op_code::BitRShift, expr.loc);
            break;
        case lex::TokenType::BitLShift:
            emit_op(Op_code::BitLShift, expr.loc);
            break;
        default:
            break;
    }
}

void Compiler::visit_unary_expr(const ast::Unary_expr &expr)
{
    compile_expr(expr.right);
    switch (expr.op)
    {
        case lex::TokenType::LogicalNot:
            emit_op(Op_code::Not, expr.loc);
            break;
        case lex::TokenType::BitNot:
            emit_op(Op_code::BitNot, expr.loc);
            break;
        case lex::TokenType::Minus:
            emit_op(Op_code::Negate, expr.loc);
            break;
        default:
            break;
    }
}

void Compiler::visit_model_literal_expr(const ast::Model_literal_expr &expr)
{
    auto type_var = expr.type;
    auto model_type_ptr = std::get<mem::rc_ptr<types::Model_type>>(type_var);

    // Compile fields in the EXACT memory order dictated by the signature
    for (const auto &sig_field : model_type_ptr->fields)
    {
        for (const auto &ast_field : expr.fields)
        {
            if (ast_field.first == sig_field.first)
            {
                compile_expr(ast_field.second);
                break;
            }
        }
    }

    emit_op(Op_code::Construct_model, expr.loc);
    emit_byte(static_cast<uint8_t>(model_type_ptr->fields.size()), expr.loc);
    emit_byte(identifier_constant(model_type_ptr->name, expr.loc), expr.loc);
}

void Compiler::visit_field_access_expr(const ast::Field_access_expr &expr)
{
    compile_expr(expr.object);

    auto type_var = ast::get_type(expr.object->node);
    auto model_type_ptr = std::get<mem::rc_ptr<types::Model_type>>(type_var);

    uint8_t index = 0;
    for (size_t i = 0; i < model_type_ptr->fields.size(); ++i)
    {
        if (model_type_ptr->fields[i].first == expr.field_name)
        {
            index = i;
            break;
        }
    }

    emit_op(Op_code::Get_field, expr.loc);
    emit_byte(index, expr.loc);
}

void Compiler::visit_field_assignment_expr(const ast::Field_assignment_expr &expr)
{
    compile_expr(expr.value);   // Pushes the value
    compile_expr(expr.object);  // Pushes the object

    auto type_var = ast::get_type(expr.object->node);
    auto model_type_ptr = std::get<mem::rc_ptr<types::Model_type>>(type_var);

    uint8_t index = 0;
    for (size_t i = 0; i < model_type_ptr->fields.size(); ++i)
    {
        if (model_type_ptr->fields[i].first == expr.field_name)
        {
            index = i;
            break;
        }
    }

    emit_op(Op_code::Set_field, expr.loc);
    emit_byte(index, expr.loc);
}

void Compiler::visit_method_call_expr(const ast::Method_call_expr &expr)
{
    auto type_var = ast::get_type(expr.object->node);
    auto model_type_ptr = std::get<mem::rc_ptr<types::Model_type>>(type_var);

    std::string global_name = model_type_ptr->name + "::" + expr.method_name;

    // 1. Push Closure to bottom of frame
    uint8_t name_idx = identifier_constant(global_name, expr.loc);
    emit_op(Op_code::Get_global, expr.loc);
    emit_byte(name_idx, expr.loc);

    // 2. Compile object -> becomes the hidden 'this' parameter!
    compile_expr(expr.object);

    // 3. Compile the explicit arguments
    for (auto *arg : expr.arguments) compile_expr(arg);

    // 4. Call (Add 1 to argument count to account for 'this')
    emit_op(Op_code::Call, expr.loc);
    emit_byte(static_cast<uint8_t>(expr.arguments.size() + 1), expr.loc);
}


void Compiler::visit_static_path_expr(const ast::Static_path_expr &expr)
{
    // e.g., User::new -> Compiles down to Get_global "User::new"
    if (auto* base_var = std::get_if<ast::Variable_expr>(&expr.base->node)) {
        std::string global_name = base_var->name + "::" + expr.member.lexeme;
        uint8_t name_idx = identifier_constant(global_name, expr.loc);
        emit_op(Op_code::Get_global, expr.loc);
        emit_byte(name_idx, expr.loc);
    }
}

// ============================================================================
// Emitters
// ============================================================================

void Compiler::emit_op(Op_code op, phos::ast::Source_location loc) { current_chunk()->write(static_cast<uint8_t>(op), loc); }
void Compiler::emit_byte(uint8_t byte, phos::ast::Source_location loc) { current_chunk()->write(byte, loc); }

void Compiler::emit_constant(Value value, phos::ast::Source_location loc)
{
    size_t index = current_chunk()->add_constant(std::move(value));
    emit_op(Op_code::Constant, loc);
    emit_byte(static_cast<uint8_t>(index), loc);
}

uint8_t Compiler::identifier_constant(const std::string &name, phos::ast::Source_location loc)
{
    return static_cast<uint8_t>(current_chunk()->add_constant(Value(name)));
}

size_t Compiler::emit_jump(Op_code instruction, phos::ast::Source_location loc)
{
    emit_op(instruction, loc);
    emit_byte(0xff, loc);
    emit_byte(0xff, loc);
    return current_chunk()->code.size() - 2;
}

void Compiler::patch_jump(size_t offset, phos::ast::Source_location loc)
{
    size_t jump = current_chunk()->code.size() - offset - 2;
    current_chunk()->code[offset] = (jump >> 8) & 0xff;
    current_chunk()->code[offset + 1] = jump & 0xff;
}

void Compiler::emit_loop(size_t loop_start, phos::ast::Source_location loc)
{
    emit_op(Op_code::Loop, loc);
    size_t jump = current_chunk()->code.size() - loop_start + 2;
    emit_byte((jump >> 8) & 0xff, loc);
    emit_byte(jump & 0xff, loc);
}

}  // namespace phos::vm
