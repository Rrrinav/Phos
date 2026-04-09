#include "compiler.hpp"

#include "opcodes.hpp"

#include <algorithm>
#include <print>

namespace phos::vm {

static const types::Function_type *find_model_method_signature(const types::Type &type, const std::string &name)
{
    if (!std::holds_alternative<mem::rc_ptr<types::Model_type>>(type))
        return nullptr;

    auto model_type = std::get<mem::rc_ptr<types::Model_type>>(type);
    for (const auto &[method_name, signature] : model_type->methods)
        if (method_name == name)
            return &signature;
    return nullptr;
}

static bool is_iterator_protocol_type(const types::Type &type)
{
    if (std::holds_alternative<mem::rc_ptr<types::Iterator_type>>(type))
        return true;

    auto next_method = find_model_method_signature(type, "next");
    return next_method && next_method->parameter_types.empty() && types::is_optional(next_method->return_type);
}

static bool has_iter_method(const types::Type &type)
{
    auto iter_method = find_model_method_signature(type, "iter");
    return iter_method && iter_method->parameter_types.empty() && is_iterator_protocol_type(iter_method->return_type);
}

static const Type_checker::Native_sig *find_native_signature(const Compiler &compiler, const std::string &name, int signature_index)
{
    if (!compiler.type_checker || signature_index < 0)
        return nullptr;

    auto it = compiler.type_checker->native_signatures.find(name);
    if (it == compiler.type_checker->native_signatures.end())
        return nullptr;
    if (static_cast<size_t>(signature_index) >= it->second.size())
        return nullptr;
    return &it->second[static_cast<size_t>(signature_index)];
}

Result<mem::rc_ptr<Closure_value>> Compiler::compile(const std::vector<ast::Stmt *> &statements)
{
    states.clear();

    // The top-level script is just a giant function!
    types::Function_type main_sig;
    main_sig.return_type = types::Primitive_kind::Void;
    auto main_chunk = mem::make_rc<Chunk>();
    auto main_closure = mem::make_rc<Closure_value>("<main>", 0, main_sig, main_chunk);

    states.push_back({main_closure, {}, {}, 0});

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

// Scoping & Locals

void Compiler::begin_scope()
{
    current()->scope_depth++;
}

void Compiler::end_scope(phos::ast::Source_location loc)
{
    current()->scope_depth--;
    while (!current()->locals.empty() && current()->locals.back().depth > current()->scope_depth) {
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
    else if (auto *node = std::get_if<ast::For_in_stmt>(&stmt->node))
        visit_for_in_stmt(*node);
    else if (auto *node = std::get_if<ast::Function_stmt>(&stmt->node))
        visit_function_stmt(*node);
    else if (auto *node = std::get_if<ast::Return_stmt>(&stmt->node))
        visit_return_stmt(*node);
    else if (auto *node = std::get_if<ast::Model_stmt>(&stmt->node))
        visit_model_stmt(*node);
    else if (auto *node = std::get_if<ast::Match_stmt>(&stmt->node))
        visit_match_stmt(*node);
    else if (auto *node = std::get_if<ast::Union_stmt>(&stmt->node))
        visit_union_stmt(*node);
    else if (auto *node = std::get_if<ast::Enum_stmt>(&stmt->node))
        visit_enum_stmt(*node);
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
    else if (auto *node = std::get_if<ast::Static_path_expr>(&expr->node))
        visit_static_path_expr(*node);
    else if (auto *node = std::get_if<ast::Enum_member_expr>(&expr->node))
        visit_enum_member_expr(*node);
    else if (auto *node = std::get_if<ast::Array_literal_expr>(&expr->node))
        visit_array_literal_expr(*node);
    else if (auto *node = std::get_if<ast::Array_access_expr>(&expr->node))
        visit_array_access_expr(*node);
    else if (auto *node = std::get_if<ast::Array_assignment_expr>(&expr->node))
        visit_array_assignment_expr(*node);
    else if (auto *node = std::get_if<ast::Cast_expr>(&expr->node))
        visit_cast_expr(*node);
    else if (auto *node = std::get_if<ast::Range_expr>(&expr->node))
        visit_range_expr(*node);
    else if (auto *node = std::get_if<ast::Fstring_expr>(&expr->node))
        visit_fstring_expr(*node);
    else if (auto *node = std::get_if<ast::Anon_model_literal_expr>(&expr->node))
        visit_anon_model_literal_expr(*node);
    else
        std::println(stderr, "Unimplemented expr node at index: {}", expr->node.index());

    for (uint8_t i = 0; i < expr->auto_wrap_depth; ++i) {
        emit_op(Op_code::Wrap_optional, ast::get_loc(expr->node));
    }
}

// Call Frames & Functions
void Compiler::visit_function_stmt(const ast::Function_stmt &stmt)
{
    // 1. Build the Signature
    types::Function_type sig;
    for (const auto &p : stmt.parameters)
        sig.parameter_types.push_back(p.type);
    sig.return_type = stmt.return_type;

    // 2. Setup isolated Chunk and State
    auto chunk = mem::make_rc<Chunk>();
    auto closure = mem::make_rc<Closure_value>(stmt.name, stmt.parameters.size(), sig, chunk);

    states.push_back({closure, {}, {}, 0});

    // 3. Slot 0 is always the function object itself
    current()->locals.push_back(Local{stmt.name, 0});

    // 4. Define parameters as standard local variables (Slots 1, 2, 3...)
    for (const auto &p : stmt.parameters)
        current()->locals.push_back(Local{p.name, 0});

    // 5. Compile the body
    compile_stmt(stmt.body);

    // 6. Ensure every function safely returns
    emit_op(Op_code::Nil, stmt.loc);
    emit_op(Op_code::Return, stmt.loc);

    // 7. Pop the finished compiler state AND grab upvalues
    auto finished_closure = current()->closure;
    auto upvalues = current()->upvalues; // <--- Grab before pop!
    states.pop_back();

    // 8. Emit the new function as a Closure
    size_t idx = current_chunk()->add_constant(Value(finished_closure));
    emit_op(Op_code::Make_closure, stmt.loc);
    emit_byte(static_cast<uint8_t>(idx), stmt.loc);

    // Loop through upvalues and emit their routing instructions
    for (const auto &uv : upvalues) {
        emit_byte(uv.is_local ? 1 : 0, stmt.loc);
        emit_byte(uv.index, stmt.loc);
    }

    // 9. Bind it to the variable name!
    if (current()->scope_depth == 0) {
        uint8_t name_idx = identifier_constant(stmt.name, stmt.loc);
        emit_op(Op_code::Define_global, stmt.loc);
        emit_byte(name_idx, stmt.loc);
    } else {
        current()->locals.push_back(Local{stmt.name, current()->scope_depth});
    }
}

void Compiler::visit_closure_expr(const ast::Closure_expr &expr)
{
    types::Function_type sig;
    for (const auto &p : expr.parameters)
        sig.parameter_types.push_back(p.type);
    sig.return_type = expr.return_type;

    auto chunk = mem::make_rc<Chunk>();
    auto closure = mem::make_rc<Closure_value>("<closure>", expr.parameters.size(), sig, chunk);

    states.push_back({closure, {}, {}, 0});

    current()->locals.push_back(Local{"<closure>", 0});
    for (const auto &p : expr.parameters)
        current()->locals.push_back(Local{p.name, 0});

    compile_stmt(expr.body);

    emit_op(Op_code::Nil, expr.loc);
    emit_op(Op_code::Return, expr.loc);

    auto finished_closure = current()->closure;
    auto upvalues = current()->upvalues; // <--- Grab before pop!
    states.pop_back();

    // Emit Make_closure and routing bytes
    size_t idx = current_chunk()->add_constant(Value(finished_closure));
    emit_op(Op_code::Make_closure, expr.loc);
    emit_byte(static_cast<uint8_t>(idx), expr.loc);

    for (const auto &uv : upvalues) {
        emit_byte(uv.is_local ? 1 : 0, expr.loc);
        emit_byte(uv.index, expr.loc);
    }
}

void Compiler::visit_call_expr(const ast::Call_expr &expr)
{
    if (auto *var_callee = std::get_if<ast::Variable_expr>(&expr.callee->node)) {
        if (var_callee->name == "iter" && expr.arguments.size() == 1) {
            auto arg_type = ast::get_type(expr.arguments[0].value->node);

            if (is_iterator_protocol_type(arg_type)) {
                compile_expr(expr.arguments[0].value);
                return;
            }

            if (has_iter_method(arg_type)) {
                ast::Method_call_expr method_expr{
                    .object = expr.arguments[0].value,
                    .method_name = "iter",
                    .arguments = {},
                    .type = expr.type,
                    .loc = expr.loc,
                };
                visit_method_call_expr(method_expr);
                return;
            }
        }
    }

    // First, push the function object
    compile_expr(expr.callee);

    if (auto *var_callee = std::get_if<ast::Variable_expr>(&expr.callee->node)) {
        if (auto *native_sig = find_native_signature(*this, var_callee->name, expr.native_signature_index)) {
            for (size_t i = 0; i < expr.arguments.size(); ++i) {
                if (expr.arguments[i].value)
                    compile_expr(expr.arguments[i].value);
                else
                    emit_constant(*native_sig->params[i].default_value, expr.loc);
            }

            emit_op(Op_code::Call, expr.loc);
            emit_byte(expr.arguments.size(), expr.loc);
            return;
        }
    }

    // Then, push all the arguments in order
    for (const auto &arg : expr.arguments)
        compile_expr(arg.value);

    // Emit the magical Call opcode with the arity!
    emit_op(Op_code::Call, expr.loc);
    emit_byte(expr.arguments.size(), expr.loc);
}

void Compiler::visit_return_stmt(const ast::Return_stmt &stmt)
{
    if (stmt.expression)
        compile_expr(stmt.expression);
    else
        emit_op(Op_code::Nil, stmt.loc); // Return nil if empty

    emit_op(Op_code::Return, stmt.loc);
}

// ============================================================================
// Standard Statement/Expr Visitors (Same as before, routing to current_chunk)
// ============================================================================

void Compiler::visit_print_stmt(const ast::Print_stmt &stmt)
{
    for (auto *expr : stmt.expressions)
        compile_expr(expr);

    emit_constant(Value(stmt.sep), stmt.loc);
    emit_constant(Value(stmt.end), stmt.loc);

    if (stmt.stream == ast::Print_stream::STDERR)
        emit_op(Op_code::Print_err, stmt.loc);
    else
        emit_op(Op_code::Print, stmt.loc);

    emit_byte(static_cast<uint8_t>(stmt.expressions.size()), stmt.loc);
}

void Compiler::visit_expr_stmt(const ast::Expr_stmt &stmt)
{
    compile_expr(stmt.expression);
    emit_op(Op_code::Pop, stmt.loc);
}
void Compiler::visit_literal_expr(const ast::Literal_expr &expr)
{
    emit_constant(expr.value, expr.loc);
}

void Compiler::visit_var_stmt(const ast::Var_stmt &stmt)
{
    if (stmt.initializer)
        compile_expr(stmt.initializer);
    else
        emit_op(Op_code::Nil, stmt.loc);

    if (current()->scope_depth == 0) {
        uint8_t name_idx = identifier_constant(stmt.name, stmt.loc);
        emit_op(Op_code::Define_global, stmt.loc);
        emit_byte(name_idx, stmt.loc);
    } else {
        current()->locals.push_back(Local{stmt.name, current()->scope_depth});
    }
}

void Compiler::visit_block_stmt(const ast::Block_stmt &stmt)
{
    begin_scope();
    for (auto *s : stmt.statements)
        compile_stmt(s);
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
    if (stmt.condition) {
        compile_expr(stmt.condition);
        exit_jump = emit_jump(Op_code::Jump_if_false, stmt.loc);
        emit_op(Op_code::Pop, stmt.loc);
    }
    if (stmt.body)
        compile_stmt(stmt.body);
    if (stmt.increment) {
        compile_expr(stmt.increment);
        emit_op(Op_code::Pop, stmt.loc);
    }
    emit_loop(loop_start, stmt.loc);
    if (stmt.condition) {
        patch_jump(exit_jump, stmt.loc);
        emit_op(Op_code::Pop, stmt.loc);
    }
    end_scope(stmt.loc);
}

void Compiler::visit_for_in_stmt(const ast::For_in_stmt &stmt)
{
    begin_scope(); // Hidden scope for '.iter'

    // 1. SETUP: .iter = iter(iterable)
    uint8_t make_idx = identifier_constant("iter", stmt.loc);
    emit_op(Op_code::Get_global, stmt.loc);
    emit_byte(make_idx, stmt.loc);

    if (stmt.iterable)
        compile_expr(stmt.iterable);

    emit_op(Op_code::Call, stmt.loc);
    emit_byte(1, stmt.loc);

    current()->locals.push_back(Local{".iter", current()->scope_depth});
    size_t iter_slot = current()->locals.size() - 1;

    // --- LOOP START ---
    size_t loop_start = current_chunk()->code.size();

    // 2. FETCH: val = Iter::next(.iter, 1)
    // [Stack: <Func Iter::next>]
    uint8_t next_idx = identifier_constant("Iter::next", stmt.loc);
    emit_op(Op_code::Get_global, stmt.loc);
    emit_byte(next_idx, stmt.loc);

    emit_op(Op_code::Get_local, stmt.loc);
    emit_byte(static_cast<uint8_t>(iter_slot), stmt.loc);

    // Push the default step argument: 1
    emit_constant(Value(int64_t(1)), stmt.loc);

    // Call Iter::next(.iter, 1) -> Returns {item} (or nil if exhausted)
    // [Stack: {item}]
    emit_op(Op_code::Call, stmt.loc);
    emit_byte(2, stmt.loc);

    // 3. CHECK: if val == nil break
    // Jump_if_nil specifically checks if depth == 0 AND payload == nullptr.
    // If it's {nil} (depth 1), it WILL NOT jump!
    size_t exit_jump = emit_jump(Op_code::Jump_if_nil, stmt.loc);

    // 4. UNWRAP: The VM-Native Way
    // This simply does `val.option_depth -= 1` on the top of the stack.
    // {item} becomes item. {nil} becomes nil. Zero overhead.
    emit_op(Op_code::Unwrap, stmt.loc);

    // --- BODY ---
    begin_scope();
    current()->locals.push_back(Local{stmt.var_name, current()->scope_depth});
    if (stmt.body)
        compile_stmt(stmt.body);
    end_scope(stmt.loc); // Pops 'item'

    emit_loop(loop_start, stmt.loc);

    // --- EXIT ---
    patch_jump(exit_jump, stmt.loc);

    // If we jumped here, the exhausted base 'nil' is sitting on the stack
    emit_op(Op_code::Pop, stmt.loc); 

    end_scope(stmt.loc); // Pops '.iter'
}

void Compiler::visit_model_stmt(const ast::Model_stmt &stmt)
{
    // Compile every method bound to this model
    for (const auto *method : stmt.methods) {
        types::Function_type sig;
        for (const auto &p : method->parameters)
            sig.parameter_types.push_back(p.type);
        sig.return_type = method->return_type;

        auto chunk = mem::make_rc<Chunk>();
        std::string global_name = stmt.name + "::" + method->name;

        // Arity accounts for the hidden 'this' parameter if it's an instance method
        size_t arity = method->parameters.size() + (method->is_static ? 0 : 1);
        auto closure = mem::make_rc<Closure_value>(global_name, arity, sig, chunk);

        states.push_back({closure, {}, {}, 0});

        // Slot 0: The function itself
        current()->locals.push_back(Local{global_name, 0});

        // Slot 1: 'this' (if instance method)
        if (!method->is_static)
            current()->locals.push_back(Local{"this", 0});

        for (const auto &p : method->parameters)
            current()->locals.push_back(Local{p.name, 0});

        compile_stmt(method->body);

        emit_op(Op_code::Nil, method->loc);
        emit_op(Op_code::Return, method->loc);

        auto finished_closure = current()->closure;
        auto upvalues = current()->upvalues; // <--- Grab before pop!
        states.pop_back();

        // Emit Make_closure and routing bytes
        size_t idx = current_chunk()->add_constant(Value(finished_closure));
        emit_op(Op_code::Make_closure, method->loc);
        emit_byte(static_cast<uint8_t>(idx), method->loc);

        for (const auto &uv : upvalues) {
            emit_byte(uv.is_local ? 1 : 0, method->loc);
            emit_byte(uv.index, method->loc);
        }

        uint8_t name_idx = identifier_constant(global_name, method->loc);
        emit_op(Op_code::Define_global, method->loc);
        emit_byte(name_idx, method->loc);
    }
}

void Compiler::visit_union_stmt(const ast::Union_stmt &stmt)
{
    // Unions are purely static type definitions.
    // The Type Checker has already validated everything.
    // We emit absolutely ZERO bytecode for the declaration itself!
    return;
}

void Compiler::visit_enum_stmt(const ast::Enum_stmt &stmt)
{
    // Enums are purely static type definitions.
    // We emit absolutely ZERO bytecode for the declaration itself!
    return;
}

void Compiler::visit_match_stmt(const ast::Match_stmt &stmt)
{
    if (stmt.subject)
        compile_expr(stmt.subject);

    // 1. Fetch the static type to differentiate Unions from Enums/Static values
    types::Type subject_type = ast::get_type(stmt.subject->node);
    bool is_union_subject = std::holds_alternative<mem::rc_ptr<types::Union_type>>(subject_type);

    current()->locals.push_back(Local{"<match_subject>", current()->scope_depth});
    std::vector<size_t> end_jumps;

    for (const auto &arm : stmt.arms) {
        if (arm.is_wildcard) {
            emit_op(Op_code::True, stmt.loc);
        } else if (auto *literal = std::get_if<ast::Literal_expr>(&arm.pattern->node)) {
            emit_op(Op_code::Get_local, stmt.loc);
            emit_byte(static_cast<uint8_t>(current()->locals.size() - 1), stmt.loc);
            compile_expr(arm.pattern);
            emit_op(Op_code::Equal, stmt.loc);
        } else if (auto *range = std::get_if<ast::Range_expr>(&arm.pattern->node)) {
            uint8_t contains_idx = identifier_constant("Iter::contains", stmt.loc);
            emit_op(Op_code::Get_global, stmt.loc);
            emit_byte(contains_idx, stmt.loc);

            const std::string helper_name = range->inclusive ? "__range_inclusive" : "__range_exclusive";
            uint8_t helper_idx = identifier_constant(helper_name, stmt.loc);
            emit_op(Op_code::Get_global, stmt.loc);
            emit_byte(helper_idx, stmt.loc);
            compile_expr(range->start);
            compile_expr(range->end);
            emit_op(Op_code::Call, stmt.loc);
            emit_byte(2, stmt.loc);

            emit_op(Op_code::Get_local, stmt.loc);
            emit_byte(static_cast<uint8_t>(current()->locals.size() - 1), stmt.loc);

            emit_op(Op_code::Call, stmt.loc);
            emit_byte(2, stmt.loc);
        } else if (auto *static_path = std::get_if<ast::Static_path_expr>(&arm.pattern->node)) {
            // 2. Safely emit Match_variant ONLY if the subject is actually a Union!
            if (is_union_subject) {
                emit_op(Op_code::Match_variant, stmt.loc);
                emit_byte(identifier_constant(static_path->member.lexeme, stmt.loc), stmt.loc);
            } else {
                // Otherwise, treat it as a standard Enum value check
                emit_op(Op_code::Get_local, stmt.loc);
                emit_byte(static_cast<uint8_t>(current()->locals.size() - 1), stmt.loc);
                compile_expr(arm.pattern);
                emit_op(Op_code::Equal, stmt.loc);
            }
        } else if (auto *member = std::get_if<ast::Enum_member_expr>(&arm.pattern->node)) {
            if (is_union_subject) {
                emit_op(Op_code::Match_variant, stmt.loc);
                emit_byte(identifier_constant(member->member_name, stmt.loc), stmt.loc);
            } else {
                emit_op(Op_code::Get_local, stmt.loc);
                emit_byte(static_cast<uint8_t>(current()->locals.size() - 1), stmt.loc);
                compile_expr(arm.pattern);
                emit_op(Op_code::Equal, stmt.loc);
            }
        } else {
            // --- NEW: THE CUSTOM PROTOCOL EXECUTION ---
            types::Type pattern_type = ast::get_type(arm.pattern->node);
            bool uses_custom_protocol = false;

            if (auto *model_t = std::get_if<mem::rc_ptr<types::Model_type>>(&pattern_type)) {
                auto &methods = (*model_t)->methods;
                if (auto it = std::find_if(
                        methods.begin(),
                        methods.end(),
                        [](std::pair<std::string, types::Function_type> &x) { return x.first == "__match__"; });
                    it != methods.end())
                    uses_custom_protocol = true;
            }

            if (uses_custom_protocol) {
                auto model_type = types::get_model_type(pattern_type);
                std::string global_method_name = model_type->name + "::__match__";

                // 1. Fetch the __match__ function pointer from globals
                emit_op(Op_code::Get_global, stmt.loc);
                emit_byte(identifier_constant(global_method_name, stmt.loc), stmt.loc);

                // 2. Push the pattern object (This acts as the implicit 'this' context)
                compile_expr(arm.pattern);

                // 3. Push the subject (This acts as the argument)
                emit_op(Op_code::Get_local, stmt.loc);
                emit_byte(static_cast<uint8_t>(current()->locals.size() - 1), stmt.loc);

                // 4. Call the method! (Arity 2: 'this' + 'subject') -> leaves a bool on stack
                emit_op(Op_code::Call, stmt.loc);
                emit_byte(2, stmt.loc);
            } else if (
                std::holds_alternative<ast::Anon_model_literal_expr>(arm.pattern->node)
                || std::holds_alternative<ast::Model_literal_expr>(arm.pattern->node)) {
                // Structural field-by-field match
                // The subject is already on the stack as a local — fetch its model type
                auto *model_t = std::get_if<mem::rc_ptr<types::Model_type>>(&pattern_type);
                if (!model_t) {
                    emit_op(Op_code::False, stmt.loc);
                } else {
                    auto &fields = (*model_t)->fields;
                    // Collect the pattern's field values
                    // We'll emit: (subj.f0 == pat.f0) && (subj.f1 == pat.f1) && ...
                    // Build it as a sequence of comparisons AND-ed together

                    // Helper: get the pattern fields in signature order
                    std::vector<ast::Expr *> pattern_field_exprs(fields.size(), nullptr);

                    if (auto *anon = std::get_if<ast::Anon_model_literal_expr>(&arm.pattern->node)) {
                        for (size_t i = 0; i < fields.size(); ++i)
                            for (auto &[name, val_expr] : anon->fields)
                                if (name == fields[i].first) {
                                    pattern_field_exprs[i] = val_expr;
                                    break;
                                }
                    } else if (auto *named = std::get_if<ast::Model_literal_expr>(&arm.pattern->node)) {
                        for (size_t i = 0; i < fields.size(); ++i)
                            for (auto &[name, val_expr] : named->fields)
                                if (name == fields[i].first) {
                                    pattern_field_exprs[i] = val_expr;
                                    break;
                                }
                    }

                    uint8_t subject_slot = static_cast<uint8_t>(current()->locals.size() - 1);

                    // Emit the first comparison to bootstrap the AND chain
                    emit_op(Op_code::Get_local, stmt.loc);
                    emit_byte(subject_slot, stmt.loc);
                    emit_op(Op_code::Get_field, stmt.loc);
                    emit_byte(static_cast<uint8_t>(0), stmt.loc);
                    compile_expr(pattern_field_exprs[0]);
                    emit_op(Op_code::Equal, stmt.loc);

                    // AND in each subsequent field
                    for (size_t i = 1; i < fields.size(); ++i) {
                        size_t short_circuit = emit_jump(Op_code::Jump_if_false, stmt.loc);
                        emit_op(Op_code::Pop, stmt.loc);

                        emit_op(Op_code::Get_local, stmt.loc);
                        emit_byte(subject_slot, stmt.loc);
                        emit_op(Op_code::Get_field, stmt.loc);
                        emit_byte(static_cast<uint8_t>(i), stmt.loc);
                        compile_expr(pattern_field_exprs[i]);
                        emit_op(Op_code::Equal, stmt.loc);

                        patch_jump(short_circuit, stmt.loc);
                    }
                }
            }
        }

        size_t next_arm_jump = emit_jump(Op_code::Jump_if_false, stmt.loc);
        emit_op(Op_code::Pop, stmt.loc);

        begin_scope();

        bool is_union_match = !arm.is_wildcard && is_union_subject
            && (std::holds_alternative<ast::Static_path_expr>(arm.pattern->node)
                || std::holds_alternative<ast::Enum_member_expr>(arm.pattern->node));

        if (is_union_match) {
            if (!arm.bind_name.empty())
                current()->locals.push_back(Local{arm.bind_name, current()->scope_depth});
            else
                emit_op(Op_code::Pop, stmt.loc);
        }

        if (arm.body)
            compile_stmt(arm.body);

        end_scope(stmt.loc);

        end_jumps.push_back(emit_jump(Op_code::Jump, stmt.loc));

        patch_jump(next_arm_jump, stmt.loc);
        emit_op(Op_code::Pop, stmt.loc);

        if (is_union_match)
            emit_op(Op_code::Pop, stmt.loc);
    }

    // Patch the jumps to land exactly ON the Pop instruction!
    // This ensures successful branches clean up the match subject before continuing.
    for (size_t j : end_jumps)
        patch_jump(j, stmt.loc);

    emit_op(Op_code::Pop, stmt.loc);
    current()->locals.pop_back();
}

void Compiler::visit_variable_expr(const ast::Variable_expr &expr)
{
    int arg = resolve_local(expr.name);
    if (arg != -1) {
        emit_op(Op_code::Get_local, expr.loc);
        emit_byte(static_cast<uint8_t>(arg), expr.loc);
    } else if (int upval = resolve_upvalue(current(), expr.name); upval != -1) {
        emit_op(Op_code::Get_upvalue, expr.loc);
        emit_byte(static_cast<uint8_t>(upval), expr.loc);
    } else {
        uint8_t name_idx = identifier_constant(expr.name, expr.loc);
        emit_op(Op_code::Get_global, expr.loc);
        emit_byte(name_idx, expr.loc);
    }
}

void Compiler::visit_assignment_expr(const ast::Assignment_expr &expr)
{
    compile_expr(expr.value);
    int arg = resolve_local(expr.name);
    if (arg != -1) {
        emit_op(Op_code::Set_local, expr.loc);
        emit_byte(static_cast<uint8_t>(arg), expr.loc);
    } else if (int upval = resolve_upvalue(current(), expr.name); upval != -1) {
        emit_op(Op_code::Set_upvalue, expr.loc);
        emit_byte(static_cast<uint8_t>(upval), expr.loc);
    } else {
        uint8_t name_idx = identifier_constant(expr.name, expr.loc);
        emit_op(Op_code::Set_global, expr.loc);
        emit_byte(name_idx, expr.loc);
    }
}

void Compiler::visit_binary_expr(const ast::Binary_expr &expr)
{
    if (expr.op == lex::TokenType::LogicalAnd) {
        compile_expr(expr.left);
        size_t end_jump = emit_jump(Op_code::Jump_if_false, expr.loc);
        emit_op(Op_code::Pop, expr.loc);
        compile_expr(expr.right);
        patch_jump(end_jump, expr.loc);
        return;
    } else if (expr.op == lex::TokenType::LogicalOr) {
        compile_expr(expr.left);
        size_t end_jump = emit_jump(Op_code::Jump_if_true, expr.loc);
        emit_op(Op_code::Pop, expr.loc);
        compile_expr(expr.right);
        patch_jump(end_jump, expr.loc);
        return;
    }

    compile_expr(expr.left);
    compile_expr(expr.right);
    switch (expr.op) {
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
    switch (expr.op) {
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

    if (std::holds_alternative<mem::rc_ptr<types::Union_type>>(type_var)) {
        auto union_type_ptr = std::get<mem::rc_ptr<types::Union_type>>(type_var);

        // A union literal only ever has 1 field, as enforced by the Type Checker
        std::string variant_name = expr.fields[0].first;
        ast::Expr *payload = expr.fields[0].second;

        // 1. Push the payload onto the stack
        if (payload)
            compile_expr(payload);
        else
            emit_op(Op_code::Nil, expr.loc);

        // 2. Wrap it in the Union memory object!
        emit_op(Op_code::Construct_union, expr.loc);
        emit_byte(identifier_constant(union_type_ptr->name, expr.loc), expr.loc);
        emit_byte(identifier_constant(variant_name, expr.loc), expr.loc);
        return;
    }

    auto model_type_ptr = std::get<mem::rc_ptr<types::Model_type>>(type_var);

    // Compile fields in the EXACT memory order dictated by the signature
    for (const auto &sig_field : model_type_ptr->fields) {
        for (const auto &ast_field : expr.fields) {
            if (ast_field.first == sig_field.first) {
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
    for (size_t i = 0; i < model_type_ptr->fields.size(); ++i) {
        if (model_type_ptr->fields[i].first == expr.field_name) {
            index = i;
            break;
        }
    }

    emit_op(Op_code::Get_field, expr.loc);
    emit_byte(index, expr.loc);
}

void Compiler::visit_field_assignment_expr(const ast::Field_assignment_expr &expr)
{
    compile_expr(expr.value);  // Pushes the value
    compile_expr(expr.object); // Pushes the object

    auto type_var = ast::get_type(expr.object->node);
    auto model_type_ptr = std::get<mem::rc_ptr<types::Model_type>>(type_var);

    uint8_t index = 0;
    for (size_t i = 0; i < model_type_ptr->fields.size(); ++i) {
        if (model_type_ptr->fields[i].first == expr.field_name) {
            index = i;
            break;
        }
    }

    emit_op(Op_code::Set_field, expr.loc);
    emit_byte(index, expr.loc);
}

void Compiler::visit_method_call_expr(const ast::Method_call_expr &expr)
{
    if (expr.is_closure_field) {
        compile_expr(expr.object); // Pushes the Model

        emit_op(Op_code::Get_field, expr.loc);
        emit_byte(expr.field_index, expr.loc); // Replaces Model with the Closure on the stack!

        for (const auto &arg : expr.arguments)
            compile_expr(arg.value); // Push args

        emit_op(Op_code::Call, expr.loc); // Execute!
        emit_byte(static_cast<uint8_t>(expr.arguments.size()), expr.loc);
        return;
    }

    auto type_var = ast::get_type(expr.object->node);
    std::string global_name;

    if (std::holds_alternative<mem::rc_ptr<types::Optional_type>>(type_var) && expr.method_name == "or_else") {
        compile_expr(expr.object);
        size_t nil_jump = emit_jump(Op_code::Jump_if_nil, expr.loc);
        size_t done_jump = emit_jump(Op_code::Jump, expr.loc);

        patch_jump(nil_jump, expr.loc);
        emit_op(Op_code::Pop, expr.loc);

        compile_expr(expr.arguments[0].value);
        emit_op(Op_code::Call, expr.loc);
        emit_byte(0, expr.loc);

        patch_jump(done_jump, expr.loc);
        return;
    }

    // If it's a Model, route to ModelName::method
    if (auto *model_t = std::get_if<mem::rc_ptr<types::Model_type>>(&type_var))
        global_name = (*model_t)->name + "::" + expr.method_name;
    // If it's an Array, route to Array::method
    else if (std::holds_alternative<mem::rc_ptr<types::Array_type>>(type_var))
        global_name = "Array::" + expr.method_name;
    else if (std::holds_alternative<mem::rc_ptr<types::Optional_type>>(type_var))
        global_name = "Optional::" + expr.method_name;
    else if (std::holds_alternative<mem::rc_ptr<types::Iterator_type>>(type_var))
        global_name = "Iter::" + expr.method_name;
    // If it's a string, route to string::method
    else if (auto *prim = std::get_if<types::Primitive_kind>(&type_var); prim && *prim == types::Primitive_kind::String)
        global_name = "string::" + expr.method_name;

    uint8_t name_idx = identifier_constant(global_name, expr.loc);
    emit_op(Op_code::Get_global, expr.loc);
    emit_byte(name_idx, expr.loc);

    compile_expr(expr.object); // Push the hidden 'this'

    if (auto *native_sig = find_native_signature(*this, global_name, expr.native_signature_index)) {
        for (size_t i = 0; i < expr.arguments.size(); ++i) {
            if (expr.arguments[i].value)
                compile_expr(expr.arguments[i].value);
            else
                emit_constant(*native_sig->params[i + 1].default_value, expr.loc);
        }

        emit_op(Op_code::Call, expr.loc);
        emit_byte(static_cast<uint8_t>(expr.arguments.size() + 1), expr.loc); // +1 for 'this'
        return;
    }

    for (const auto &arg : expr.arguments)
        compile_expr(arg.value);

    emit_op(Op_code::Call, expr.loc);
    emit_byte(static_cast<uint8_t>(expr.arguments.size() + 1), expr.loc); // +1 for 'this'
}

void Compiler::visit_static_path_expr(const ast::Static_path_expr &expr)
{
    auto type_var = ast::get_type(expr.base->node);
    if (std::holds_alternative<mem::rc_ptr<types::Union_type>>(type_var)) {
        if (!std::holds_alternative<mem::rc_ptr<types::Union_type>>(expr.type)) {
            emit_op(Op_code::Nil, expr.loc);
            return;
        }

        // It's a Union Variant with NO payload!
        auto union_name = std::get<mem::rc_ptr<types::Union_type>>(type_var)->name;

        emit_op(Op_code::Nil, expr.loc);
        emit_op(Op_code::Construct_union, expr.loc);
        emit_byte(identifier_constant(union_name, expr.loc), expr.loc);
        emit_byte(identifier_constant(expr.member.lexeme, expr.loc), expr.loc);
        return;
    }
    if (std::holds_alternative<mem::rc_ptr<types::Enum_type>>(type_var)) {
        auto enum_type = std::get<mem::rc_ptr<types::Enum_type>>(type_var);
        Value val = enum_type->variants->map.at(expr.member.lexeme);
        emit_constant(val, expr.loc);
        return;
    }
    // e.g., User::new -> Compiles down to Get_global "User::new"
    if (auto *base_var = std::get_if<ast::Variable_expr>(&expr.base->node)) {
        std::string global_name = base_var->name + "::" + expr.member.lexeme;
        uint8_t name_idx = identifier_constant(global_name, expr.loc);
        emit_op(Op_code::Get_global, expr.loc);
        emit_byte(name_idx, expr.loc);
    }
}

void Compiler::visit_enum_member_expr(const ast::Enum_member_expr &expr)
{
    types::Type base_type = expr.type;
    if (auto *opt_t = std::get_if<mem::rc_ptr<types::Optional_type>>(&base_type)) {
        base_type = (*opt_t)->base_type;
    }

    if (auto *enum_type_ptr = std::get_if<mem::rc_ptr<types::Enum_type>>(&base_type)) {
        Value val = (*enum_type_ptr)->variants->map.at(expr.member_name);
        emit_constant(val, expr.loc);
    } else {
        emit_op(Op_code::Nil, expr.loc);
    }
}

void Compiler::visit_array_literal_expr(const ast::Array_literal_expr &expr)
{
    // Push all elements to the stack from left to right
    for (auto *elem : expr.elements)
        compile_expr(elem);

    // Tell the VM to pop N elements and wrap them in an Array_value
    emit_op(Op_code::Create_array, expr.loc);
    emit_byte(static_cast<uint8_t>(expr.elements.size()), expr.loc);
}

void Compiler::visit_array_access_expr(const ast::Array_access_expr &expr)
{
    compile_expr(expr.array); // Pushes the array
    compile_expr(expr.index); // Pushes the index
    emit_op(Op_code::Get_index, expr.loc);
}

void Compiler::visit_cast_expr(const ast::Cast_expr &expr)
{
    if (auto *target_array = std::get_if<mem::rc_ptr<types::Array_type>>(&expr.target_type)) {
        if ((*target_array)->element_type == types::Type(types::Primitive_kind::U8)) {
            uint8_t bytes_idx = identifier_constant("bytes", expr.loc);
            emit_op(Op_code::Get_global, expr.loc);
            emit_byte(bytes_idx, expr.loc);
            compile_expr(expr.expression);
            emit_op(Op_code::Call, expr.loc);
            emit_byte(1, expr.loc);
            return;
        }
    }

    compile_expr(expr.expression);

    if (std::holds_alternative<types::Primitive_kind>(expr.target_type)) {
        auto prim = std::get<types::Primitive_kind>(expr.target_type);

        // We only need runtime casts for actual memory-changing conversions
        // You can expand this if-statement as you add i32, u8, etc.
        if (types::is_numeric_primitive(prim)) {
            emit_op(Op_code::Cast, expr.loc);
            emit_byte(static_cast<uint8_t>(prim), expr.loc); // The operand is the target type!
        }
    }
}

void Compiler::visit_array_assignment_expr(const ast::Array_assignment_expr &expr)
{
    compile_expr(expr.value); // Pushes the value FIRST
    compile_expr(expr.array); // Pushes the array
    compile_expr(expr.index); // Pushes the index
    emit_op(Op_code::Set_index, expr.loc);
}

void Compiler::visit_range_expr(const ast::Range_expr &expr)
{
    const std::string helper_name = expr.inclusive ? "__range_inclusive" : "__range_exclusive";
    uint8_t helper_idx = identifier_constant(helper_name, expr.loc);
    emit_op(Op_code::Get_global, expr.loc);
    emit_byte(helper_idx, expr.loc);
    compile_expr(expr.start);
    compile_expr(expr.end);
    emit_op(Op_code::Call, expr.loc);
    emit_byte(2, expr.loc);
}

void Compiler::visit_fstring_expr(const ast::Fstring_expr &expr)
{
    bool has_component = false;
    size_t interpolation_index = 0;
    std::string literal_buf;

    auto append_literal = [&](const std::string &text) {
        if (text.empty())
            return;
        emit_constant(Value(text), expr.loc);
        if (has_component)
            emit_op(Op_code::Add, expr.loc);
        else
            has_component = true;
    };

    auto append_interpolation = [&](ast::Expr *interpolation) {
        uint8_t to_str_idx = identifier_constant("to_str", expr.loc);
        emit_op(Op_code::Get_global, expr.loc);
        emit_byte(to_str_idx, expr.loc);
        compile_expr(interpolation);
        emit_op(Op_code::Call, expr.loc);
        emit_byte(1, expr.loc);

        if (has_component)
            emit_op(Op_code::Add, expr.loc);
        else
            has_component = true;
    };

    size_t i = 0;
    while (i < expr.raw_template.size()) {
        if (expr.raw_template[i] == '{') {
            // RESOLVE ESCAPED OPEN BRACE
            if (i + 1 < expr.raw_template.size() && expr.raw_template[i + 1] == '{') {
                literal_buf += '{';
                i += 2;
                continue;
            }

            append_literal(literal_buf);
            literal_buf.clear();

            size_t start = ++i;
            int depth = 1;
            while (i < expr.raw_template.size() && depth > 0) {
                if (expr.raw_template[i] == '{')
                    depth++;
                else if (expr.raw_template[i] == '}')
                    depth--;
                if (depth > 0)
                    i++;
            }
            (void)start;
            i++;

            if (interpolation_index < expr.interpolations.size())
                append_interpolation(expr.interpolations[interpolation_index++]);
        } else if (expr.raw_template[i] == '}') {
            // RESOLVE ESCAPED CLOSE BRACE
            if (i + 1 < expr.raw_template.size() && expr.raw_template[i + 1] == '}') {
                literal_buf += '}';
                i += 2;
                continue;
            }
            i++; // Fallback in case of isolated '}' (caught by parser anyway)
        } else {
            literal_buf += expr.raw_template[i++];
        }
    }

    append_literal(literal_buf);

    if (!has_component)
        emit_constant(Value(std::string("")), expr.loc);
}

void Compiler::visit_anon_model_literal_expr(const ast::Anon_model_literal_expr &expr)
{
    // Unwrap the Optional context if it exists
    types::Type base_type = expr.type;
    if (auto *opt_t = std::get_if<mem::rc_ptr<types::Optional_type>>(&base_type))
        base_type = (*opt_t)->base_type;

    // --- 1. UNION ANONYMOUS LITERAL ---
    if (std::holds_alternative<mem::rc_ptr<types::Union_type>>(base_type)) {
        auto union_type_ptr = std::get<mem::rc_ptr<types::Union_type>>(base_type);

        // The Type Checker guarantees unions have exactly 1 field here
        std::string variant_name = expr.fields[0].first;

        // Push the payload onto the stack
        if (expr.fields[0].second)
            compile_expr(expr.fields[0].second);
        else
            emit_op(Op_code::Nil, expr.loc);

        // Tell the VM to wrap it in a Union_value
        emit_op(Op_code::Construct_union, expr.loc);
        emit_byte(identifier_constant(union_type_ptr->name, expr.loc), expr.loc);
        emit_byte(identifier_constant(variant_name, expr.loc), expr.loc);
        return;
    }

    // --- 2. MODEL ANONYMOUS LITERAL ---
    if (std::holds_alternative<mem::rc_ptr<types::Model_type>>(base_type)) {
        // Compile each field's value in structural order
        for (const auto &field : expr.fields)
            compile_expr(field.second);

        auto model_type_ptr = std::get<mem::rc_ptr<types::Model_type>>(base_type);

        emit_op(Op_code::Construct_model, expr.loc);
        emit_byte(static_cast<uint8_t>(model_type_ptr->fields.size()), expr.loc);
        emit_byte(identifier_constant(model_type_ptr->name, expr.loc), expr.loc);
        return;
    }
}

// ============================================================================
// Emitters
// ============================================================================

void Compiler::emit_op(Op_code op, phos::ast::Source_location loc)
{
    current_chunk()->write(static_cast<uint8_t>(op), loc);
}
void Compiler::emit_byte(uint8_t byte, phos::ast::Source_location loc)
{
    current_chunk()->write(byte, loc);
}

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

int Compiler::resolve_local_in_state(Compiler_state *state, const std::string &name)
{
    for (int i = static_cast<int>(state->locals.size()) - 1; i >= 0; i--)
        if (state->locals[i].name == name)
            return i;
    return -1;
}

int Compiler::add_upvalue(Compiler_state *state, uint8_t index, bool is_local)
{
    for (size_t i = 0; i < state->upvalues.size(); i++)
        if (state->upvalues[i].index == index && state->upvalues[i].is_local == is_local)
            return static_cast<int>(i);
    state->upvalues.push_back({index, is_local});
    state->closure->upvalue_count = state->upvalues.size();
    return static_cast<int>(state->upvalues.size() - 1);
}

int Compiler::resolve_upvalue(Compiler_state *state, const std::string &name)
{
    if (state == &states.front())
        return -1; // The global script scope has no upvalues

    Compiler_state *parent = state - 1;

    // 1. Is it a local variable in the immediate parent?
    int local = resolve_local_in_state(parent, name);
    if (local != -1)
        return add_upvalue(state, static_cast<uint8_t>(local), true);

    // 2. Is it an upvalue in the parent? (Recursive searching upwards!)
    int upvalue = resolve_upvalue(parent, name);
    if (upvalue != -1)
        return add_upvalue(state, static_cast<uint8_t>(upvalue), false);

    return -1;
}
} // namespace phos::vm
