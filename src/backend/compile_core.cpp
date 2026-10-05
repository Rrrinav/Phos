#include "compiler.hpp"

#include "frontend/ast_utils.hpp"
#include "frontend/environment/symbol.hpp"

#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <optional>
#include <print>
#include <utility>

namespace phos::vm {

namespace {
// Single tables for opcode selection. Previously each of the five
// *_opcode_for functions below carried its own copy of the same
// kind/operator mapping as a switch; the tables keep them consistent.
using Cast_row = std::pair<types::Primitive_kind, vm::Opcode>;

constexpr Cast_row kCastOpcodes[] = {
    {types::Primitive_kind::I8, vm::Opcode::Cast_i8},
    {types::Primitive_kind::I16, vm::Opcode::Cast_i16},
    {types::Primitive_kind::I32, vm::Opcode::Cast_i32},
    {types::Primitive_kind::I64, vm::Opcode::Cast_i64},
    {types::Primitive_kind::U8, vm::Opcode::Cast_u8},
    {types::Primitive_kind::U16, vm::Opcode::Cast_u16},
    {types::Primitive_kind::U32, vm::Opcode::Cast_u32},
    {types::Primitive_kind::U64, vm::Opcode::Cast_u64},
    {types::Primitive_kind::F16, vm::Opcode::Cast_f16},
    {types::Primitive_kind::F32, vm::Opcode::Cast_f32},
    {types::Primitive_kind::F64, vm::Opcode::Cast_f64},
};

constexpr Cast_row kSatCastOpcodes[] = {
    {types::Primitive_kind::I8, vm::Opcode::Sat_cast_i8},
    {types::Primitive_kind::I16, vm::Opcode::Sat_cast_i16},
    {types::Primitive_kind::I32, vm::Opcode::Sat_cast_i32},
    {types::Primitive_kind::I64, vm::Opcode::Sat_cast_i64},
    {types::Primitive_kind::U8, vm::Opcode::Sat_cast_u8},
    {types::Primitive_kind::U16, vm::Opcode::Sat_cast_u16},
    {types::Primitive_kind::U32, vm::Opcode::Sat_cast_u32},
    {types::Primitive_kind::U64, vm::Opcode::Sat_cast_u64},
};

struct Family_row
{
    lex::TokenType op;
    vm::Opcode i64;
    vm::Opcode u64;
    vm::Opcode f64;
};

constexpr Family_row kArithmeticOpcodes[] = {
    {lex::TokenType::Plus, vm::Opcode::Add_i64, vm::Opcode::Add_u64, vm::Opcode::Add_f64},
    {lex::TokenType::Minus, vm::Opcode::Sub_i64, vm::Opcode::Sub_u64, vm::Opcode::Sub_f64},
    {lex::TokenType::Star, vm::Opcode::Mul_i64, vm::Opcode::Mul_u64, vm::Opcode::Mul_f64},
    {lex::TokenType::Slash, vm::Opcode::Div_i64, vm::Opcode::Div_u64, vm::Opcode::Div_f64},
    {lex::TokenType::Percent, vm::Opcode::Mod_i64, vm::Opcode::Mod_u64, vm::Opcode::Mod_f64},
};

constexpr Family_row kComparisonOpcodes[] = {
    {lex::TokenType::Equal, vm::Opcode::Eq_i64, vm::Opcode::Eq_u64, vm::Opcode::Eq_f64},
    {lex::TokenType::NotEqual, vm::Opcode::Neq_i64, vm::Opcode::Neq_u64, vm::Opcode::Neq_f64},
    {lex::TokenType::Less, vm::Opcode::Lt_i64, vm::Opcode::Lt_u64, vm::Opcode::Lt_f64},
    {lex::TokenType::LessEqual, vm::Opcode::Lte_i64, vm::Opcode::Lte_u64, vm::Opcode::Lte_f64},
    {lex::TokenType::Greater, vm::Opcode::Gt_i64, vm::Opcode::Gt_u64, vm::Opcode::Gt_f64},
    {lex::TokenType::GreaterEqual, vm::Opcode::Gte_i64, vm::Opcode::Gte_u64, vm::Opcode::Gte_f64},
};

struct Int_family_row
{
    lex::TokenType op;
    vm::Opcode i64;
    vm::Opcode u64;
};

constexpr Int_family_row kBitwiseOpcodes[] = {
    {lex::TokenType::BitAnd, vm::Opcode::BitAnd_i64, vm::Opcode::BitAnd_u64},
    {lex::TokenType::Pipe, vm::Opcode::BitOr_i64, vm::Opcode::BitOr_u64},
    {lex::TokenType::BitXor, vm::Opcode::BitXor_i64, vm::Opcode::BitXor_u64},
    {lex::TokenType::BitLShift, vm::Opcode::Shl_i64, vm::Opcode::Shl_u64},
    {lex::TokenType::BitRshift, vm::Opcode::Shr_i64, vm::Opcode::Shr_u64},
};

template <typename Row, size_t N>
std::optional<vm::Opcode> find_family_opcode(const Row (&table)[N], lex::TokenType op, bool is_float, bool is_unsigned)
{
    for (const auto &row : table) {
        if (row.op == op) {
            if constexpr (requires { row.f64; }) {
                if (is_float) {
                    return row.f64;
                }
            }
            return is_unsigned ? row.u64 : row.i64;
        }
    }
    return std::nullopt;
}

template <size_t N>
std::optional<vm::Opcode> find_cast_opcode(const Cast_row (&table)[N], types::Primitive_kind kind)
{
    for (const auto &row : table) {
        if (row.first == kind) {
            return row.second;
        }
    }
    return std::nullopt;
}
} // namespace

Compiler::Compiler(phos::Compiler_context &ctx) : ctx(ctx)
{}

// CONTEXT & LEXICAL SCOPING ENGINE
void Compiler::push_context(Function_context *fctx)
{
    fctx->enclosing = current_ctx_;
    current_ctx_ = fctx;
    if (root_ctx_ == nullptr) {
        root_ctx_ = fctx;
    }
}

void Compiler::pop_context()
{
    current_ctx_ = current_ctx_->enclosing;
    if (current_ctx_ == nullptr) {
        root_ctx_ = nullptr;
    }
}

std::string Compiler::canonical_function_name(const ast::Function_stmt &stmt) const
{
    if (stmt.name.find("::") != std::string::npos) {
        return stmt.name;
    }

    if (current_module_ns_.empty() || current_module_ns_ == "main") {
        return stmt.name;
    }

    return current_module_ns_ + "::" + stmt.name;
}

void Compiler::set_closure_name(Closure_data &closure, std::string_view name)
{
    size_t name_size = sizeof(String_data) + name.length() + 1;
    closure.name = static_cast<String_data *>(ctx.arena.allocate_bytes(name_size));
    closure.name->length = name.length();
    std::copy(name.begin(), name.end(), closure.name->chars);
    closure.name->chars[name.length()] = '\0';
}

int Compiler::resolve_local(Function_context *fctx, const std::string &name)
{
    for (int i = static_cast<int>(fctx->locals.size()) - 1; i >= 0; i--) {
        if (fctx->locals[i].name == name) {
            return fctx->locals[i].reg;
        }
    }
    return -1;
}

int Compiler::add_upvalue(Function_context *fctx, const std::string &name, uint8_t index, bool is_local)
{
    for (size_t i = 0; i < fctx->upvalues.size(); i++) {
        if (fctx->upvalues[i].index == index && fctx->upvalues[i].is_local == is_local) {
            return static_cast<int>(i);
        }
    }
    fctx->upvalues.push_back({name, index, is_local});
    return static_cast<int>(fctx->upvalues.size() - 1);
}

void Compiler::push_scope()
{
    current_ctx_->scopes.push_back({current_ctx_->locals.size(), {}});
}

void Compiler::pop_scope()
{
    // Copy to safely prevent vector invalidation if a defer contains a nested block!
    auto defers = current_ctx_->scopes.back().defers;
    for (auto it = defers.rbegin(); it != defers.rend(); ++it) {
        compile_stmt(*it);
    }
    current_ctx_->locals.resize(current_ctx_->scopes.back().locals_count);
    current_ctx_->scopes.pop_back();
}

void Compiler::emit_defers(size_t target_depth)
{
    // Evaluate from inner-most scope outwards until the target loop/function boundary
    for (int i = static_cast<int>(current_ctx_->scopes.size()) - 1; i >= static_cast<int>(target_depth); --i) {
        auto defers = current_ctx_->scopes[i].defers;
        for (auto it = defers.rbegin(); it != defers.rend(); ++it) {
            compile_stmt(*it);
        }
    }
}

int Compiler::resolve_upvalue(Function_context *fctx, const std::string &name)
{
    if (fctx->enclosing == nullptr) {
        return -1;
    }

    int local_index = resolve_local(fctx->enclosing, name);
    if (local_index != -1) {
        return add_upvalue(fctx, name, static_cast<uint8_t>(local_index), true);
    }

    int upvalue_index = resolve_upvalue(fctx->enclosing, name);
    if (upvalue_index != -1) {
        return add_upvalue(fctx, name, static_cast<uint8_t>(upvalue_index), false);
    }

    return -1;
}

// MAIN COMPILATION FLOW
void Compiler::bind_natives()
{
    for (const auto &[name, sigs] : ctx.type_env.native_signatures) {
        if (sigs.empty() || sigs[0].func == nullptr) {
            continue;
        }

        Closure_data *closure = ctx.arena.allocate<Closure_data>();
        new (closure) Closure_data();

        set_closure_name(*closure, name);

        closure->arity = sigs[0].is_variadic() ? sigs[0].fixed_arity() : sigs[0].params.size();
        closure->min_arity = sigs[0].fixed_arity();
        closure->is_variadic = sigs[0].is_variadic();
        closure->native_func = sigs[0].func;
        closure->code_count = 0;
        closure->code = nullptr;
        closure->constant_count = 0;
        closure->constants = nullptr;

        if (auto sym_id = ctx.registry.lookup_global(name)) {
            function_locations_[*sym_id] = closure;
        } else {
            std::println(std::cerr, "Compiler Bug: Native function '{}' missing from global registry.", name);
            std::exit(EXIT_FAILURE);
        }
    }
}

Closure_data Compiler::compile_workspace([[maybe_unused]] Module_id main_mod)
{
    Function_context global_ctx;
    global_ctx.enclosing = nullptr;
    global_ctx.current_register = 0;
    current_ctx_ = nullptr;
    root_ctx_ = nullptr;
    current_module_ns_.clear();
    function_locations_.clear();
    function_global_slots_.clear();
    push_context(&global_ctx);

    bind_natives();

    for (const auto &module : ctx.workspace.modules) {
        hoist_module_functions(module);
    }

    std::vector<Module_id> build_order = ctx.workspace.get_topological_order();
    for (Module_id mod_id : build_order) {
        const auto &module = ctx.workspace.get_module(mod_id);
        current_module_ns_ = module.logical_namespace;

        for (auto stmt_id : module.ast_roots) {
            compile_stmt(stmt_id);
        }
    }

    emit(vm::Instruction::make_rrr(vm::Opcode::Return, 0, 0, 0));

    // Patch any top-level unresolved gotos (though unlikely outside a function)
    for (const auto &[label_name, jump_idx] : current_ctx_->unresolved_gotos) {
        if (current_ctx_->labels.contains(label_name)) {
            current_ctx_->block.instructions[jump_idx].i.imm = current_ctx_->labels.at(label_name);
        } else {
            std::println(std::cerr, "Compiler Bug: Unresolved goto label '{}' bypassed semantic checker.", label_name);
            std::exit(EXIT_FAILURE);
        }
    }

    Closure_data data{};
    auto &final_block = current_block();

    data.code_count = final_block.instructions.size();
    if (data.code_count > 0) {
        data.code = ctx.arena.allocate<vm::Instruction>(data.code_count);
        std::copy(final_block.instructions.begin(), final_block.instructions.end(), data.code);
    }

    data.constant_count = final_block.constants.size();
    if (data.constant_count > 0) {
        data.constants = ctx.arena.allocate<Value>(data.constant_count);
        std::copy(final_block.constants.begin(), final_block.constants.end(), data.constants);
    }

    // Registers are densely allocated from 0: the high-water mark is the exact live window.
    data.register_footprint = static_cast<uint16_t>(global_ctx.current_register);

    pop_context();
    return data;
}

// Incremental compile for REPL sessions: binds natives on the first entry,
// hoists only the given module's functions, and compiles only its roots.
// All compiler state (function_locations_, function_global_slots_) persists
// across calls so closures and function slots survive between entries.
Closure_data Compiler::compile_module_only(Module_id mod_id)
{
    if (function_locations_.empty()) {
        bind_natives();
    }

    const auto &module = ctx.workspace.get_module(mod_id);

    Function_context global_ctx;
    global_ctx.enclosing = nullptr;
    global_ctx.current_register = 0;
    current_ctx_ = nullptr;
    root_ctx_ = nullptr;
    current_module_ns_ = module.logical_namespace;
    push_context(&global_ctx);

    hoist_module_functions(module);

    for (auto stmt_id : module.ast_roots) {
        compile_stmt(stmt_id);
    }

    emit(vm::Instruction::make_rrr(vm::Opcode::Return, 0, 0, 0));

    // Patch any top-level unresolved gotos (though unlikely outside a function)
    for (const auto &[label_name, jump_idx] : current_ctx_->unresolved_gotos) {
        if (current_ctx_->labels.contains(label_name)) {
            current_ctx_->block.instructions[jump_idx].i.imm = current_ctx_->labels.at(label_name);
        } else {
            std::println(std::cerr, "Compiler Bug: Unresolved goto label '{}' bypassed semantic checker.", label_name);
            std::exit(EXIT_FAILURE);
        }
    }

    Closure_data data{};
    auto &final_block = current_block();

    data.code_count = final_block.instructions.size();
    if (data.code_count > 0) {
        data.code = ctx.arena.allocate<vm::Instruction>(data.code_count);
        std::copy(final_block.instructions.begin(), final_block.instructions.end(), data.code);
    }

    data.constant_count = final_block.constants.size();
    if (data.constant_count > 0) {
        data.constants = ctx.arena.allocate<Value>(data.constant_count);
        std::copy(final_block.constants.begin(), final_block.constants.end(), data.constants);
    }

    // Registers are densely allocated from 0: the high-water mark is the exact live window.
    data.register_footprint = static_cast<uint16_t>(global_ctx.current_register);

    pop_context();
    return data;
}

Closure_data Compiler::compile(const std::vector<ast::Stmt_id> &statements)
{
    (void)statements;
    Function_context global_ctx;
    global_ctx.enclosing = nullptr;
    global_ctx.current_register = 0;
    current_ctx_ = nullptr;
    root_ctx_ = nullptr;
    current_module_ns_.clear();
    function_locations_.clear();
    function_global_slots_.clear();
    push_context(&global_ctx);

    for (const auto &[name, sigs] : ctx.type_env.native_signatures) {
        if (sigs.empty() || sigs[0].func == nullptr) {
            continue;
        }

        Closure_data *closure = ctx.arena.allocate<Closure_data>();
        new (closure) Closure_data();

        set_closure_name(*closure, name);

        closure->arity = sigs[0].params.size();
        closure->native_func = sigs[0].func;
        closure->code_count = 0;
        closure->code = nullptr;
        closure->constant_count = 0;
        closure->constants = nullptr;

        if (auto sym_id = ctx.registry.lookup_global(name)) {
            function_locations_[*sym_id] = closure;
        } else {
            std::println(std::cerr, "Compiler Bug: Native function '{}' missing from global registry.", name);
            std::exit(EXIT_FAILURE);
        }
    }

    for (const auto &module : ctx.workspace.modules) {
        hoist_module_functions(module);
    }

    for (const auto &module : ctx.workspace.modules) {
        current_module_ns_ = module.logical_namespace;
        for (auto stmt_id : module.ast_roots) {
            compile_stmt(stmt_id);
        }
    }

    emit(vm::Instruction::make_rrr(vm::Opcode::Return, 0, 0, 0));

    Closure_data data{};
    auto &final_block = current_block();

    data.code_count = final_block.instructions.size();
    if (data.code_count > 0) {
        data.code = ctx.arena.allocate<vm::Instruction>(data.code_count);
        std::copy(final_block.instructions.begin(), final_block.instructions.end(), data.code);
    }

    data.constant_count = final_block.constants.size();
    if (data.constant_count > 0) {
        data.constants = ctx.arena.allocate<Value>(data.constant_count);
        std::copy(final_block.constants.begin(), final_block.constants.end(), data.constants);
    }

    // Registers are densely allocated from 0: the high-water mark is the exact live window.
    data.register_footprint = static_cast<uint16_t>(global_ctx.current_register);

    pop_context();
    return data;
}

// BYTECODE & REGISTER HELPERS
uint8_t Compiler::allocate_register()
{
    if (current_ctx_->current_register >= 255) {
        std::println(std::cerr, "Expression too complex! Exceeded 256 temporary registers.");
        std::exit(EXIT_FAILURE);
    }
    return current_ctx_->current_register++;
}

void Compiler::reset_registers()
{
    current_ctx_->current_register = 0;
}

size_t Compiler::emit(vm::Instruction inst)
{
    return current_block().emit(inst);
}

uint16_t Compiler::add_constant(Value val)
{
    return current_block().add_constant(val);
}

void Compiler::emit_numeric_normalize(uint8_t reg, types::Type_id type)
{
    if (!ctx.tt.is_numeric_primitive(type)) {
        return;
    }

    auto kind = ctx.tt.get_primitive(type);
    if (kind == types::Primitive_kind::I64 || kind == types::Primitive_kind::U64 || kind == types::Primitive_kind::F64) {
        return;
    }

    emit(vm::Instruction::make_rrr(cast_opcode_for(type), reg, 0, 0));
}

vm::Opcode Compiler::cast_opcode_for(types::Type_id type) const
{
    if (auto op = find_cast_opcode(kCastOpcodes, ctx.tt.get_primitive(type))) {
        return *op;
    }
    std::println(std::cerr, "Compiler Bug: No cast opcode for type '{}'.", ctx.tt.to_string(type));
    std::exit(EXIT_FAILURE);
}

vm::Opcode Compiler::saturating_cast_opcode_for(types::Type_id type) const
{
    if (auto op = find_cast_opcode(kSatCastOpcodes, ctx.tt.get_primitive(type))) {
        return *op;
    }
    std::println(std::cerr, "Compiler Bug: No saturating cast opcode for type '{}'.", ctx.tt.to_string(type));
    std::exit(EXIT_FAILURE);
}

vm::Opcode Compiler::arithmetic_opcode_for(lex::TokenType op, types::Type_id type) const
{
    bool is_float = ctx.tt.is_float_primitive(type);
    bool is_unsigned = ctx.tt.is_unsigned_integer_primitive(type);

    if (auto found = find_family_opcode(kArithmeticOpcodes, op, is_float, is_unsigned)) {
        return *found;
    }
    std::println(std::cerr, "Compiler Bug: Unsupported arithmetic operator.");
    std::exit(EXIT_FAILURE);
}

vm::Opcode Compiler::comparison_opcode_for(lex::TokenType op, types::Type_id left_type, types::Type_id right_type) const
{
    if (ctx.tt.is_string(left_type) && ctx.tt.is_string(right_type)) {
        if (op == lex::TokenType::Equal) {
            return vm::Opcode::Eq_str;
        }
        if (op == lex::TokenType::NotEqual) {
            return vm::Opcode::Neq_str;
        }
    }
    bool is_float = ctx.tt.is_float_primitive(left_type) || ctx.tt.is_float_primitive(right_type);
    bool is_unsigned = ctx.tt.is_unsigned_integer_primitive(left_type) && ctx.tt.is_unsigned_integer_primitive(right_type);

    if (auto found = find_family_opcode(kComparisonOpcodes, op, is_float, is_unsigned)) {
        return *found;
    }
    std::println(std::cerr, "Compiler Bug: Unsupported comparison operator.");
    std::exit(EXIT_FAILURE);
}

vm::Opcode Compiler::bitwise_opcode_for(lex::TokenType op, types::Type_id type) const
{
    bool is_unsigned = ctx.tt.is_unsigned_integer_primitive(type);
    if (auto found = find_family_opcode(kBitwiseOpcodes, op, false, is_unsigned)) {
        return *found;
    }
    std::println(std::cerr, "Compiler Bug: Unsupported bitwise operator.");
    std::exit(EXIT_FAILURE);
}

// STATEMENTS
std::optional<uint32_t> Compiler::function_global_slot(Symbol_id sym_id) const
{
    auto it = function_global_slots_.find(sym_id);
    if (it == function_global_slots_.end()) {
        return std::nullopt;
    }
    return it->second;
}

uint8_t Compiler::load_function_instance(Symbol_id sym_id, Closure_data *closure)
{
    if (auto slot = function_global_slot(sym_id)) {
        const bool decl_compiled = function_upvalues_.contains(sym_id);
        const bool inside_function = (current_ctx_ != nullptr && current_ctx_->enclosing != nullptr);

        const bool need_live_instance = inside_function || (decl_compiled && closure->upvalue_count > 0);

        if (need_live_instance) {
            uint8_t target_reg = allocate_register();
            emit(vm::Instruction::make_ri(vm::Opcode::Load_global, target_reg, *slot));
            return target_reg;
        }
    }

    // Fallback: no slot (e.g. natives) or a top-level forward reference that was
    // authored before the function declaration compiled. In that case we load a
    // prototype constant, keeping the original top-level ordering semantics.
    // Functions with upvalues rebuild a live instance at the reference site;
    // upvalue-free ones just load the prototype.
    if (closure->upvalue_count > 0) {
        return compile_closure_value(sym_id, closure);
    }
    uint8_t target_reg = allocate_register();
    uint16_t const_idx = add_constant(Value::make_closure(closure));
    emit(vm::Instruction::make_ri(vm::Opcode::Load_const, target_reg, const_idx));
    return target_reg;
}

uint8_t Compiler::compile_closure_value(Symbol_id sym_id, Closure_data *closure)
{
    // Build a live closure instance at the reference site. Upvalue routes are
    // re-resolved from the names captured at declaration, so the instance picks
    // up the correct cells no matter which scope it is created in.
    uint16_t const_idx = add_constant(Value::make_closure(closure));
    uint8_t dst_reg = allocate_register();
    emit(vm::Instruction::make_ri(vm::Opcode::Make_closure, dst_reg, const_idx));

    auto it = function_upvalues_.find(sym_id);
    if (it == function_upvalues_.end() || it->second.size() != closure->upvalue_count) {
        std::println(std::cerr, "Compiler Bug: Missing upvalue routing for closure '{}'.", closure->name ? closure->name->chars : "");
        std::exit(EXIT_FAILURE);
    }

    for (const auto &uv : it->second) {
        vm::Instruction route;
        route.rrr.op = vm::Opcode::None;

        int local_idx = resolve_local(current_ctx_, uv.name);
        if (local_idx != -1) {
            route.rrr.src_a = 1;
            route.rrr.src_b = static_cast<uint8_t>(local_idx);
        } else {
            int upval_idx = resolve_upvalue(current_ctx_, uv.name);
            if (upval_idx == -1) {
                std::println(
                    std::cerr,
                    "Compiler Bug: Could not re-resolve upvalue '{}' while creating closure '{}'.",
                    uv.name,
                    closure->name ? closure->name->chars : "");
                std::exit(EXIT_FAILURE);
            }
            route.rrr.src_a = 0;
            route.rrr.src_b = static_cast<uint8_t>(upval_idx);
        }

        route.rrr.dst = 0;
        emit(route);
    }

    return dst_reg;
}

} // namespace phos::vm
