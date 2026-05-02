#include "resolver.hpp"

#include <algorithm>
#include <string_view>
#include <type_traits>
#include <variant>

namespace phos {

namespace {

Module_id find_owner_module(const Workspace &workspace, ast::Stmt_id stmt_id)
{
    for (const auto &module : workspace.modules) {
        if (std::find(module.ast_roots.begin(), module.ast_roots.end(), stmt_id) != module.ast_roots.end()) {
            return module.id;
        }
    }

    return Module_id::null();
}

std::string exported_name(std::string_view canonical_name)
{
    const auto pos = canonical_name.rfind("::");
    if (pos == std::string_view::npos) {
        return std::string(canonical_name);
    }
    return std::string(canonical_name.substr(pos + 2));
}

void publish_symbol(Compiler_context &ctx, Module_id owner, const std::string &canonical_name, Symbol_kind kind, types::Type_id type, ast::Stmt_id decl)
{
    if (owner.is_null()) {
        return;
    }

    if (auto existing = ctx.registry.lookup_global(canonical_name)) {
        auto &sym = ctx.registry.get_symbol(*existing);
        sym.kind = kind;
        sym.type = type;
        sym.declaration = decl;
        sym.owner_module = owner;
        ctx.workspace.get_module(owner).add_public_symbol(exported_name(canonical_name), *existing);
        return;
    }

    Symbol sym{
        .id = Symbol_id{0},
        .name = canonical_name,
        .kind = kind,
        .type = type,
        .owner_module = owner,
        .is_public = true,
        .const_value = std::nullopt,
        .stack_offset = std::nullopt,
        .ffi_index = std::nullopt,
        .declaration = decl,
    };

    const Symbol_id sym_id = ctx.registry.create_symbol(std::move(sym));
    ctx.workspace.get_module(owner).add_public_symbol(exported_name(canonical_name), sym_id);
}

} // namespace

Resolver::Resolver(Compiler_context &ctx) : ctx_(ctx)
{}

void Resolver::resolver_error(const ast::Source_location &loc, const std::string &message)
{
    diagnostics_.error(loc.l, loc.c, loc.file, "{}", message);
}

void Resolver::resolver_warning(const ast::Source_location &loc, const std::string &message)
{
    diagnostics_.warning(loc.l, loc.c, loc.file, "{}", message);
}

err::Engine Resolver::resolve(const std::vector<ast::Stmt_id> &statements)
{
    (void)statements;

    // 1: Add all top-level names to the environment
    for (const auto &module : ctx_.workspace.modules) {
        declare_globals(module.ast_roots);
    }

    // 2: Overwrite all Unresolved_types with concrete Type_ids in the AST
    for (const auto &module : ctx_.workspace.modules) {
        resolve_placeholders(module.ast_roots);
    }

    // 3: Extract the fully resolved structural data into the Type_table
    for (const auto &module : ctx_.workspace.modules) {
        populate_signatures(module.ast_roots);
    }

    return diagnostics_;
}

// Pass 1: Declare Globals
void Resolver::declare_globals(const std::vector<ast::Stmt_id> &statements)
{
    for (auto stmt_id : statements) {
        if (stmt_id.is_null()) {
            continue;
        }
        const auto &stmt_node = ctx_.tree.get(stmt_id).node;
        const Module_id owner_module = find_owner_module(ctx_.workspace, stmt_id);

        if (const auto *func_stmt = std::get_if<ast::Function_stmt>(&stmt_node)) {
            if (ctx_.type_env.is_function_defined(func_stmt->name)) {
                resolver_error(func_stmt->loc, "Function '" + func_stmt->name + "' is already defined.");
            } else {
                ctx_.type_env.functions[func_stmt->name] = {stmt_id};
                publish_symbol(ctx_, owner_module, func_stmt->name, Symbol_kind::Phos_func, ctx_.tt.get_unknown(), stmt_id);
            }
        } else if (const auto *model_stmt = std::get_if<ast::Model_stmt>(&stmt_node)) {
            if (ctx_.type_env.is_type_defined(model_stmt->name)) {
                resolver_error(model_stmt->loc, "Type name model '" + model_stmt->name + "' is already defined.");
            } else {
                auto model_type = ctx_.type_env.tt.model(model_stmt->name);
                ctx_.type_env.global_types[model_stmt->name] = model_type;
                ctx_.type_env.model_data[model_stmt->name] = {};
                publish_symbol(ctx_, owner_module, model_stmt->name, Symbol_kind::Model_def, model_type, stmt_id);
            }
        } else if (const auto *union_stmt = std::get_if<ast::Union_stmt>(&stmt_node)) {
            if (ctx_.type_env.is_type_defined(union_stmt->name)) {
                resolver_error(union_stmt->loc, "Type name union '" + union_stmt->name + "' is already defined.");
            } else {
                auto union_type = ctx_.type_env.tt.union_(union_stmt->name, {});
                ctx_.type_env.global_types[union_stmt->name] = union_type;
                ctx_.type_env.union_data[union_stmt->name] = {};
                publish_symbol(ctx_, owner_module, union_stmt->name, Symbol_kind::Union_def, union_type, stmt_id);
            }
        } else if (const auto *enum_stmt = std::get_if<ast::Enum_stmt>(&stmt_node)) {
            if (ctx_.type_env.is_type_defined(enum_stmt->name)) {
                resolver_error(enum_stmt->loc, "Type name enum '" + enum_stmt->name + "' is already defined.");
            } else {
                auto enum_type = ctx_.type_env.tt.enum_(enum_stmt->name, enum_stmt->base_type);
                ctx_.type_env.global_types[enum_stmt->name] = enum_type;
                ctx_.type_env.enum_data[enum_stmt->name] = {};
                publish_symbol(ctx_, owner_module, enum_stmt->name, Symbol_kind::Enum_def, enum_type, stmt_id);
            }
        }
    }
}

// Pass 2: Resolve Placeholders
void Resolver::resolve_placeholders(const std::vector<ast::Stmt_id> &statements)
{
    for (auto stmt_id : statements) {
        resolve_stmt(stmt_id);
    }
}

void Resolver::resolve_type(types::Type_id &id, const ast::Source_location &loc)
{
    if (ctx_.type_env.tt.is_unresolved(id)) {
        std::string name = ctx_.type_env.tt.get(id).as<types::Unresolved_type>().name;
        if (ctx_.type_env.is_type_defined(name)) {
            id = *ctx_.type_env.get_type(name);
        } else {
            resolver_error(loc, "Unknown type '" + name + "'");
            id = ctx_.type_env.tt.get_unknown();
        }
    } else if (ctx_.type_env.tt.is_array(id)) {
        types::Type_id elem = ctx_.type_env.tt.get_array_elem(id);
        resolve_type(elem, loc);
        id = ctx_.type_env.tt.array(elem);
    } else if (ctx_.type_env.tt.is_optional(id)) {
        types::Type_id base = ctx_.type_env.tt.get_optional_base(id);
        resolve_type(base, loc);
        id = ctx_.type_env.tt.optional(base);
    } else if (ctx_.type_env.tt.is_function(id)) {
        auto func = ctx_.type_env.tt.get(id).as<types::Function_type>();
        for (auto &p : func.params) {
            resolve_type(p, loc);
        }
        resolve_type(func.ret, loc);
        id = ctx_.type_env.tt.function(func.params, func.ret);
    }
    // if `id` is `Unknown` or any other concrete type, we just exit immediately!
}

void Resolver::resolve_stmt(ast::Stmt_id stmt_id)
{
    if (stmt_id.is_null()) {
        return;
    }
    auto &stmt_var = ctx_.tree.get(stmt_id).node;

    std::visit(
        [this](auto &s) {
            using T = std::decay_t<decltype(s)>;

            if constexpr (std::is_same_v<T, ast::Function_stmt>) {
                resolve_type(s.return_type, s.loc);
                for (auto &param : s.parameters) {
                    resolve_type(param.type, s.loc);
                    resolve_expr(param.default_value);
                }
                resolve_stmt(s.body);

            } else if constexpr (std::is_same_v<T, ast::Model_stmt>) {
                for (auto &field : s.fields) {
                    resolve_type(field.type, field.loc);
                    resolve_expr(field.default_value);
                }
                for (auto method_id : s.methods) {
                    resolve_stmt(method_id);
                }

            } else if constexpr (std::is_same_v<T, ast::Union_stmt>) {
                for (auto &variant : s.variants) {
                    resolve_type(variant.type, variant.loc);
                    resolve_expr(variant.default_value);
                }

            } else if constexpr (std::is_same_v<T, ast::Enum_stmt>) {
                resolve_type(s.base_type, s.loc);

            } else if constexpr (std::is_same_v<T, ast::Block_stmt>) {
                for (auto child_stmt : s.statements) {
                    resolve_stmt(child_stmt);
                }

            } else if constexpr (std::is_same_v<T, ast::Var_stmt>) {
                resolve_type(s.type, s.loc);
                resolve_expr(s.initializer);

            } else if constexpr (std::is_same_v<T, ast::Print_stmt>) {
                for (auto ex : s.expressions) {
                    resolve_expr(ex);
                }

            } else if constexpr (std::is_same_v<T, ast::Expr_stmt>) {
                resolve_expr(s.expression);

            } else if constexpr (std::is_same_v<T, ast::If_stmt>) {
                resolve_expr(s.condition);
                resolve_stmt(s.then_branch);
                resolve_stmt(s.else_branch);

            } else if constexpr (std::is_same_v<T, ast::While_stmt>) {
                resolve_expr(s.condition);
                resolve_stmt(s.body);

            } else if constexpr (std::is_same_v<T, ast::For_stmt>) {
                resolve_stmt(s.initializer);
                resolve_expr(s.condition);
                resolve_expr(s.increment);
                resolve_stmt(s.body);

            } else if constexpr (std::is_same_v<T, ast::For_in_stmt>) {
                resolve_expr(s.iterable);
                resolve_stmt(s.body);

            } else if constexpr (std::is_same_v<T, ast::Match_stmt>) {
                resolve_expr(s.subject);
                for (auto &arm : s.arms) {
                    resolve_expr(arm.pattern);
                    resolve_stmt(arm.body);
                }

            } else if constexpr (std::is_same_v<T, ast::Return_stmt>) {
                resolve_expr(s.expression);
            }
        },
        stmt_var);
}

void Resolver::resolve_expr(ast::Expr_id expr_id)
{
    if (expr_id.is_null()) {
        return;
    }
    auto &expr_var = ctx_.tree.get(expr_id).node;

    std::visit(
        [this](auto &e) {
            using T = std::decay_t<decltype(e)>;

            if constexpr (std::is_same_v<T, ast::Cast_expr>) {
                resolve_expr(e.expression);
                resolve_type(e.target_type, e.loc);
            } else if constexpr (std::is_same_v<T, ast::Closure_expr>) {
                resolve_type(e.return_type, e.loc);
                std::vector<types::Type_id> param_types;
                for (auto &param : e.parameters) {
                    resolve_type(param.type, e.loc);
                    resolve_expr(param.default_value);
                    param_types.push_back(param.type);
                }
                resolve_stmt(e.body);
                // Rebuild the closure's functional type wrapper with the resolved internal types!
                e.type = ctx_.type_env.tt.function(param_types, e.return_type);
            } else if constexpr (std::is_same_v<T, ast::Variable_expr>) {
                // Note: Variable expression types are Unknown here! The Checker resolves them.
            } else if constexpr (std::is_same_v<T, ast::Binary_expr>) {
                resolve_expr(e.left);
                resolve_expr(e.right);
            } else if constexpr (std::is_same_v<T, ast::Unary_expr>) {
                resolve_expr(e.right);
            } else if constexpr (std::is_same_v<T, ast::Call_expr>) {
                resolve_expr(e.callee);
                for (auto &arg : e.arguments) {
                    resolve_expr(arg.value);
                }
            } else if constexpr (std::is_same_v<T, ast::Method_call_expr>) {
                resolve_expr(e.object);
                for (auto &arg : e.arguments) {
                    resolve_expr(arg.value);
                }
            } else if constexpr (std::is_same_v<T, ast::Assignment_expr>) {
                resolve_expr(e.value);
            } else if constexpr (std::is_same_v<T, ast::Field_assignment_expr>) {
                resolve_expr(e.object);
                resolve_expr(e.value);
            } else if constexpr (std::is_same_v<T, ast::Array_assignment_expr>) {
                resolve_expr(e.array);
                resolve_expr(e.index);
                resolve_expr(e.value);
            } else if constexpr (std::is_same_v<T, ast::Field_access_expr>) {
                resolve_expr(e.object);
            } else if constexpr (std::is_same_v<T, ast::Model_literal_expr>) {
                for (auto &field : e.fields) {
                    resolve_expr(field.second);
                }
            } else if constexpr (std::is_same_v<T, ast::Anon_model_literal_expr>) {
                for (auto &field : e.fields) {
                    resolve_expr(field.second);
                }
            } else if constexpr (std::is_same_v<T, ast::Array_literal_expr>) {
                for (auto elem : e.elements) {
                    resolve_expr(elem);
                }
            } else if constexpr (std::is_same_v<T, ast::Array_access_expr>) {
                resolve_expr(e.array);
                resolve_expr(e.index);
            } else if constexpr (std::is_same_v<T, ast::Static_path_expr>) {
                resolve_expr(e.base);
            } else if constexpr (std::is_same_v<T, ast::Range_expr>) {
                resolve_expr(e.start);
                resolve_expr(e.end);
            } else if constexpr (std::is_same_v<T, ast::Spawn_expr>) {
                resolve_expr(e.call);
            } else if constexpr (std::is_same_v<T, ast::Await_expr>) {
                resolve_expr(e.thread);
            } else if constexpr (std::is_same_v<T, ast::Yield_expr>) {
                resolve_expr(e.value);
            }
        },
        expr_var);
}

// Pass 3: Populate Signatures
void Resolver::populate_signatures(const std::vector<ast::Stmt_id> &statements)
{
    for (auto stmt_id : statements) {
        if (stmt_id.is_null()) {
            continue;
        }
        const auto &stmt_node = ctx_.tree.get(stmt_id).node;

        if (const auto *model_stmt = std::get_if<ast::Model_stmt>(&stmt_node)) {
            auto type_opt = ctx_.type_env.get_type(model_stmt->name);
            if (!type_opt) {
                continue;
            }
            types::Type_id model_id = *type_opt;
            publish_symbol(ctx_, find_owner_module(ctx_.workspace, stmt_id), model_stmt->name, Symbol_kind::Model_def, model_id, stmt_id);

            // Modify the internal Type_table variant directly
            auto &model_t = std::get<types::Model_type>(ctx_.type_env.tt.types_[model_id.value].data);

            for (const auto &field : model_stmt->fields) {
                if (field.is_static) {
                    model_t.static_fields.push_back({field.name, field.type});
                    model_t.static_field_indices[field.name] = static_cast<uint32_t>(model_t.static_fields.size() - 1);
                    if (!field.default_value.is_null()) {
                        ctx_.type_env.model_data[model_stmt->name].static_fields[field.name] = field.default_value;
                    }
                    ctx_.type_env.model_data[model_stmt->name].static_field_types[field.name] = field.type;
                } else {
                    model_t.fields.push_back({field.name, field.type});
                    model_t.field_indices[field.name] = static_cast<uint32_t>(model_t.fields.size() - 1);
                    if (!field.default_value.is_null()) {
                        ctx_.type_env.model_data[model_stmt->name].field_defaults[field.name] = field.default_value;
                    }
                }
            }

            for (auto method_id : model_stmt->methods) {
                const auto *method_ast = std::get_if<ast::Function_stmt>(&ctx_.tree.get(method_id).node);
                if (!method_ast) {
                    continue;
                }

                std::vector<types::Type_id> param_types;
                for (const auto &param : method_ast->parameters) {
                    param_types.push_back(param.type);
                }
                types::Function_type method_type{param_types, method_ast->return_type};

                if (method_ast->is_static) {
                    ctx_.type_env.model_data[model_stmt->name].static_methods[method_ast->name] = {method_id};
                    model_t.static_methods.push_back({method_ast->name, method_type});
                    model_t.static_method_indices[method_ast->name] = static_cast<uint32_t>(model_t.static_methods.size() - 1);
                } else {
                    ctx_.type_env.model_data[model_stmt->name].methods[method_ast->name] = {method_id};
                    model_t.methods.push_back({method_ast->name, method_type});
                    model_t.method_indices[method_ast->name] = static_cast<uint32_t>(model_t.methods.size() - 1);
                }
            }
        } else if (const auto *union_stmt = std::get_if<ast::Union_stmt>(&stmt_node)) {
            auto type_opt = ctx_.type_env.get_type(union_stmt->name);
            if (!type_opt) {
                continue;
            }
            types::Type_id union_id = *type_opt;
            publish_symbol(ctx_, find_owner_module(ctx_.workspace, stmt_id), union_stmt->name, Symbol_kind::Union_def, union_id, stmt_id);
            auto &union_t = std::get<types::Union_type>(ctx_.type_env.tt.types_[union_id.value].data);

            for (const auto &variant : union_stmt->variants) {
                auto it = std::find_if(union_t.variants.begin(), union_t.variants.end(), [&](const auto &v) { return v.first == variant.name; });

                if (it != union_t.variants.end()) {
                    resolver_error(union_stmt->loc, "Duplicate variant '" + variant.name + "' in union.");
                } else {
                    union_t.variants.push_back({variant.name, variant.type});
                    if (!variant.default_value.is_null()) {
                        ctx_.type_env.union_data[union_stmt->name].variant_defaults[variant.name] = variant.default_value;
                    }
                }
            }
        } else if (const auto *enum_stmt = std::get_if<ast::Enum_stmt>(&stmt_node)) {
            if (!ctx_.type_env.get_type(enum_stmt->name)) {
                continue;
            }
            publish_symbol(ctx_, find_owner_module(ctx_.workspace, stmt_id), enum_stmt->name, Symbol_kind::Enum_def, *ctx_.type_env.get_type(enum_stmt->name), stmt_id);

            std::int64_t current_signed_value = 0;
            std::uint64_t current_unsigned_value = 0;

            for (const auto &variant : enum_stmt->variants) {
                Value val = Value(nullptr);

                if (variant.second.has_value()) {
                    val = variant.second.value();
                    if (val.is_s_integer()) {
                        current_signed_value = val.as_int() + 1;
                    } else if (val.is_u_integer()) {
                        current_unsigned_value = val.is_u_integer() + 1;
                    }
                } else {
                    if (ctx_.type_env.tt.is_primitive(enum_stmt->base_type) && ctx_.type_env.tt.is_integer_primitive(enum_stmt->base_type)) {

                        auto base_kind = ctx_.type_env.tt.get_primitive(enum_stmt->base_type);
                        auto next_value = types::is_signed_integer_primitive(base_kind) ? Value(current_signed_value++)
                                                                                        : Value(current_unsigned_value++);

                        auto coerced = coerce_numeric_literal(next_value, base_kind);
                        if (!coerced) {
                            resolver_error(enum_stmt->loc, "Enum auto-increment overflowed base type.");
                        } else {
                            val = coerced.value();
                        }
                    } else {
                        val = Value::make_string(ctx_.arena, variant.first);
                    }
                }

                auto &enum_var_map = ctx_.type_env.enum_data[enum_stmt->name].variants;
                if (enum_var_map.contains(variant.first)) {
                    resolver_error(enum_stmt->loc, "Duplicate variant '" + variant.first + "' in enum.");
                }

                for (const auto &[existing_name, existing_val] : enum_var_map) {
                    if (existing_val == val) {
                        resolver_error(
                            enum_stmt->loc,
                            "Enum variant value " + std::string(val.to_string()) + " is already used by variant '" + existing_name + "'.");
                    }
                }
                enum_var_map[variant.first] = val;
            }
        } else if (const auto *func_stmt = std::get_if<ast::Function_stmt>(&stmt_node)) {
            std::vector<types::Type_id> param_types;
            param_types.reserve(func_stmt->parameters.size());
            for (const auto &param : func_stmt->parameters) {
                param_types.push_back(param.type);
            }

            publish_symbol(
                ctx_,
                find_owner_module(ctx_.workspace, stmt_id),
                func_stmt->name,
                Symbol_kind::Phos_func,
                ctx_.type_env.tt.function(param_types, func_stmt->return_type),
                stmt_id);
        }
    }
}

} // namespace phos
