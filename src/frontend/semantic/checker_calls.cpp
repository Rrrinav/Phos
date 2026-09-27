#include "semantic_checker.hpp"

#include "frontend/ast_utils.hpp"

#include <algorithm>
#include <cctype>
#include <format>

namespace phos {

types::Type_id Semantic_checker::check_call_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto loc = get_node<ast::Call_expr>(ctx.tree, expr_id).loc;
    auto callee_id = get_node<ast::Call_expr>(ctx.tree, expr_id).callee;
    auto arguments_copy = get_node<ast::Call_expr>(ctx.tree, expr_id).arguments;

    auto bind_intrinsic = [&](const std::vector<std::string> &param_names, const std::string &name) -> bool {
        std::vector<ast::Call_argument> ordered(param_names.size());
        std::vector<bool> filled(param_names.size(), false);
        bool seen_named = false;
        size_t next_pos = 0;

        for (const auto &arg : arguments_copy) {
            size_t target_idx = param_names.size();
            if (!arg.name.empty()) {
                seen_named = true;
                auto it = std::find(param_names.begin(), param_names.end(), arg.name);
                if (it == param_names.end()) {
                    type_error(arg.loc, std::format("Intrinsic '{}' has no parameter named '{}'.", name, arg.name));
                    return false;
                }
                target_idx = static_cast<size_t>(std::distance(param_names.begin(), it));
                if (filled[target_idx]) {
                    type_error(arg.loc, std::format("Parameter '{}' was provided more than once.", arg.name));
                    return false;
                }
            } else {
                if (seen_named) {
                    type_error(arg.loc, "Positional arguments cannot appear after named arguments.");
                    return false;
                }
                target_idx = next_pos++;
                if (target_idx >= param_names.size()) {
                    type_error(loc, std::format("Too many arguments for '{}'. Expected {}.", name, param_names.size()));
                    return false;
                }
            }
            ordered[target_idx] = arg;
            filled[target_idx] = true;
        }

        for (size_t i = 0; i < param_names.size(); ++i) {
            if (!filled[i]) {
                type_error(loc, std::format("Missing required argument '{}' for '{}'.", param_names[i], name));
                return false;
            }
        }
        arguments_copy = std::move(ordered);
        return true;
    };

    // 1. Intercept hardcoded language intrinsics
    if (auto *var_callee = std::get_if<ast::Variable_expr>(&ctx.tree.get(callee_id).node)) {
        if (var_callee->name == "len") {
            if (!bind_intrinsic({"collection"}, "len")) {
                return get_node<ast::Call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
            }
            auto arg_type = check_expr(arguments_copy[0].value);
            if (!ctx.tt.is_string(arg_type) && !ctx.tt.is_array(arg_type)) {
                type_error(loc, "len() expects a string or an array.");
            }
            get_node<ast::Call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
            return get_node<ast::Call_expr>(ctx.tree, expr_id).type = ctx.tt.get_u64();
        }

        if (var_callee->name == "iter") {
            if (!bind_intrinsic({"iterable"}, "iter")) {
                return get_node<ast::Call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
            }
            auto iter_type = to_iterator_type(check_expr(arguments_copy[0].value));
            if (!is_iterator_protocol_type(iter_type)) {
                type_error(loc, "Value passed to iter() is not iterable.");
                return get_node<ast::Call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
            }
            get_node<ast::Call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
            return get_node<ast::Call_expr>(ctx.tree, expr_id).type = iter_type;
        }

        if (var_callee->name == "clone") {
            if (!bind_intrinsic({"value"}, "clone")) {
                return get_node<ast::Call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
            }
            auto ret = check_expr(arguments_copy[0].value);
            get_node<ast::Call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
            return get_node<ast::Call_expr>(ctx.tree, expr_id).type = ret;
        }
    }

    // 2. Fully evaluate and type-check the callee node (Resolves the FFI bug!)
    auto callee_type = check_expr(callee_id);
    if (ctx.tt.is_unknown(callee_type)) {
        return get_node<ast::Call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
    }

    // 3. Check if the resolved AST node points to a Native FFI function
    std::string ffi_callee_name;
    const auto &callee_node = ctx.tree.get(callee_id).node;

    if (const auto *var = std::get_if<ast::Variable_expr>(&callee_node)) {
        if (var->resolved_symbol) {
            auto &sym = ctx.registry.get_symbol(*var->resolved_symbol);
            if (sym.kind == Symbol_kind::Native_func) {
                ffi_callee_name = std::string(ctx.registry.resolve(sym.name));
            }
        }
    } else if (const auto *sp = std::get_if<ast::Static_path_expr>(&callee_node)) {
        if (sp->resolved_symbol) {
            auto &sym = ctx.registry.get_symbol(*sp->resolved_symbol);
            if (sym.kind == Symbol_kind::Native_func) {
                ffi_callee_name = std::string(ctx.registry.resolve(sym.name));
            }
        }
    }

    // 4. Handle Native FFI Binding
    if (!ffi_callee_name.empty() && ctx.type_env.is_native_defined(ffi_callee_name)) {
        const auto &signatures = *ctx.type_env.get_native_signatures(ffi_callee_name);

        for (size_t sig_index = 0; sig_index < signatures.size(); ++sig_index) {
            auto bound = try_bind_native_arguments(signatures[sig_index].params, arguments_copy);
            if (bound.ok) {
                get_node<ast::Call_expr>(ctx.tree, expr_id).arguments = std::move(bound.ordered_arguments);
                get_node<ast::Call_expr>(ctx.tree, expr_id).native_signature_index = static_cast<int>(sig_index);

                auto ret = parse_type_string(signatures[sig_index].ret_type_str, bound.generics);
                return get_node<ast::Call_expr>(ctx.tree, expr_id).type = ret;
            }
        }

        auto diagnostic = err::msg::error(
            diagnostics_.phase(),
            loc.l,
            loc.c,
            loc.file,
            "Arguments do not match any native signature for function '{}'.",
            ffi_callee_name);

        std::string actual_str = "";
        for (size_t i = 0; i < arguments_copy.size(); ++i) {
            actual_str += ctx.tt.to_string(check_expr(arguments_copy[i].value));
            if (i != arguments_copy.size() - 1) {
                actual_str += ", ";
            }
        }
        if (actual_str.empty()) {
            actual_str = "void";
        }

        std::string expected_str = "";
        if (signatures.size() == 1) {
            for (size_t i = 0; i < signatures[0].params.size(); ++i) {
                expected_str += signatures[0].params[i].display_type();
                if (i != signatures[0].params.size() - 1) {
                    expected_str += ", ";
                }
            }
            if (expected_str.empty()) {
                expected_str = "void";
            }
        } else {
            for (size_t s = 0; s < signatures.size(); ++s) {
                expected_str += "(";
                for (size_t i = 0; i < signatures[s].params.size(); ++i) {
                    expected_str += signatures[s].params[i].display_type();
                    if (i != signatures[s].params.size() - 1) {
                        expected_str += ", ";
                    }
                }
                expected_str += ")";
                if (s != signatures.size() - 1) {
                    expected_str += " OR ";
                }
            }
        }

        diagnostic.expected_got(expected_str, actual_str);
        diagnostics_.push(std::move(diagnostic));

        return get_node<ast::Call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
    }

    // 5. Handle Standard Phos Function Binding
    if (!ctx.tt.is_function(callee_type)) {
        type_error(ast::get_loc(callee_node), "This expression cannot be called.");
        return get_node<ast::Call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
    }

    auto sig = ctx.tt.get(callee_type).as<types::Function_type>();
    const ast::Function_stmt *declaration = nullptr;

    if (const auto *var = std::get_if<ast::Variable_expr>(&callee_node)) {
        if (ctx.type_env.is_function_defined(var->name)) {
            declaration = std::get_if<ast::Function_stmt>(&ctx.tree.get(ctx.type_env.get_function(var->name)->declaration).node);
        }
    }

    if (declaration) {
        auto bound = bind_call_arguments(declaration->parameters, arguments_copy, loc, "function", declaration->name);
        get_node<ast::Call_expr>(ctx.tree, expr_id).arguments = std::move(bound.ordered_arguments);
    } else {
        bool has_named = std::any_of(arguments_copy.begin(), arguments_copy.end(), [](const auto &arg) { return !arg.name.empty(); });
        if (has_named) {
            type_error(loc, "Named arguments only supported for direct static calls.");
            return get_node<ast::Call_expr>(ctx.tree, expr_id).type = effective_return_type(sig, ctx.tt);
        }
        if (arguments_copy.size() != sig.params.size()) {
            type_error(loc, "Incorrect number of arguments.");
            return get_node<ast::Call_expr>(ctx.tree, expr_id).type = effective_return_type(sig, ctx.tt);
        }
        for (size_t i = 0; i < arguments_copy.size(); ++i) {
            auto arg_type = check_expr(arguments_copy[i].value, sig.params[i]);
            if (!is_compatible(sig.params[i], arg_type)) {
                auto aloc = ast::get_loc(ctx.tree.get(arguments_copy[i].value).node);
                auto diagnostic = err::msg::error(diagnostics_.phase(), aloc.l, aloc.c, aloc.file, "Argument type mismatch.");
                diagnostic.expected_got(ctx.tt.to_string(sig.params[i]), ctx.tt.to_string(arg_type));
                diagnostics_.push(std::move(diagnostic));
            }
        }
    }

    return get_node<ast::Call_expr>(ctx.tree, expr_id).type = effective_return_type(sig, ctx.tt);
}

types::Type_id Semantic_checker::check_method_call_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;

    auto obj_id = get_node<ast::Method_call_expr>(ctx.tree, expr_id).object;
    auto method_name = get_node<ast::Method_call_expr>(ctx.tree, expr_id).method_name;
    auto loc = get_node<ast::Method_call_expr>(ctx.tree, expr_id).loc;
    auto arguments_copy = get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments;

    auto bind_intrinsic = [&](const std::vector<std::string> &param_names, const std::string &name) -> bool {
        std::vector<ast::Call_argument> ordered(param_names.size());
        std::vector<bool> filled(param_names.size(), false);
        bool seen_named = false;
        size_t next_pos = 0;

        for (const auto &arg : arguments_copy) {
            size_t target_idx = param_names.size();
            if (!arg.name.empty()) {
                seen_named = true;
                auto it = std::find(param_names.begin(), param_names.end(), arg.name);
                if (it == param_names.end()) {
                    type_error(arg.loc, std::format("Intrinsic '{}' has no parameter named '{}'.", name, arg.name));
                    return false;
                }
                target_idx = static_cast<size_t>(std::distance(param_names.begin(), it));
                if (filled[target_idx]) {
                    type_error(arg.loc, std::format("Parameter '{}' was provided more than once.", arg.name));
                    return false;
                }
            } else {
                if (seen_named) {
                    type_error(arg.loc, "Positional arguments cannot appear after named arguments.");
                    return false;
                }
                target_idx = next_pos++;
                if (target_idx >= param_names.size()) {
                    type_error(loc, std::format("Too many arguments for '{}'. Expected {}.", name, param_names.size()));
                    return false;
                }
            }
            ordered[target_idx] = arg;
            filled[target_idx] = true;
        }

        for (size_t i = 0; i < param_names.size(); ++i) {
            if (!filled[i]) {
                type_error(loc, std::format("Missing required argument '{}' for '{}'.", param_names[i], name));
                return false;
            }
        }
        arguments_copy = std::move(ordered);
        return true;
    };

    if (std::holds_alternative<ast::Variable_expr>(ctx.tree.get(obj_id).node)) {
        auto var_callee_name = get_node<ast::Variable_expr>(ctx.tree, obj_id).name;

        if (var_callee_name == "len") {
            if (!bind_intrinsic({"collection"}, "len")) {
                return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
            }
            auto arg_type = check_expr(arguments_copy[0].value);
            if (!ctx.tt.is_string(arg_type) && !ctx.tt.is_array(arg_type)) {
                type_error(loc, "len() expects a string or an array.");
            }
            get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
            return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_u64();
        }

        if (var_callee_name == "iter") {
            if (!bind_intrinsic({"iterable"}, "iter")) {
                return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
            }
            auto arg_type_res = check_expr(arguments_copy[0].value);
            auto iter_type = to_iterator_type(arg_type_res);
            if (!is_iterator_protocol_type(iter_type)) {
                type_error(loc, "Value passed to iter() is not iterable.");
                return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
            }
            get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
            return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = iter_type;
        }

        if (var_callee_name == "clone") {
            if (!bind_intrinsic({"value"}, "clone")) {
                return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
            }
            auto ret = check_expr(arguments_copy[0].value);
            get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
            return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ret;
        }
    }

    auto obj_type = check_expr(obj_id);

    if (ctx.tt.is_any(obj_type)) {
        for (auto &arg : arguments_copy) {
            if (!arg.value.is_null()) {
                std::ignore = check_expr(arg.value);
            }
        }
        get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
        return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_any();
    }

    if (ctx.tt.is_union(obj_type)) {
        auto &union_t = std::get<types::Union_type>(ctx.tt.get(obj_type).data);

        if (method_name == "has" || method_name == "get") {
            if (!bind_intrinsic({"variant"}, method_name)) {
                get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
                return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
            }

            std::string variant_name;
            auto arg_expr_node = &ctx.tree.get(arguments_copy[0].value).node;

            if (auto *static_path = std::get_if<ast::Static_path_expr>(arg_expr_node)) {
                variant_name = static_path->member.lexeme;
                if (auto *var_expr = std::get_if<ast::Variable_expr>(&ctx.tree.get(static_path->base).node)) {
                    if (var_expr->name != union_t.name) {
                        type_error(
                            ast::get_loc(*arg_expr_node),
                            std::format("Argument to .{}() must be a variant of the same union type.", method_name));
                    }
                }
            } else if (auto *em = std::get_if<ast::Enum_member_expr>(arg_expr_node)) {
                variant_name = em->member_name;
            } else {
                type_error(ast::get_loc(*arg_expr_node), "Argument must be a static-like variant access.");
            }

            if (!variant_name.empty() && !contains_key(union_t.variants, variant_name)) {
                type_error(ast::get_loc(*arg_expr_node), std::format("Union has no variant named '{}'", variant_name));
            }

            get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);

            if (method_name == "has") {
                return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_bool();
            } else { // get
                auto it = find_by_key(union_t.variants, variant_name);
                if (it != union_t.variants.end()) {
                    return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = it->second;
                }
                return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
            }
        } else {
            type_error(loc, "Union types only support the '.has()' and '.get()' methods.");
            return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }
    }

    if (method_name == "iter") {
        if (!bind_intrinsic({}, "iter")) {
            get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
            return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }

        auto iter_type = to_iterator_type(obj_type);
        if (!is_iterator_protocol_type(iter_type)) {
            type_error(ast::get_loc(ctx.tree.get(obj_id).node), "This type is not iterable.");
            get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
            return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }
        get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
        return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = iter_type;
    }

    if (ctx.tt.is_iterator(obj_type)) {
        if (method_name == "next" || method_name == "prev") {

            if (arguments_copy.empty()) {
                ast::Literal_expr default_step{Value(static_cast<int64_t>(1)), {0}, loc};
                ast::Expr_id step_id = ctx.tree.add_expr(ast::Expr(default_step));
                ast::get_type(ctx.tree.get(step_id).node) = ctx.tt.get_i32();
                arguments_copy.push_back({"", step_id, loc});
            }

            if (!bind_intrinsic({"step"}, method_name)) {
                get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
                return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
            }

            auto step_type = check_expr(arguments_copy[0].value);
            if (!ctx.tt.is_integer_primitive(step_type)) {
                type_error(ast::get_loc(ctx.tree.get(arguments_copy[0].value).node), "Step amount must be an integer.");
            }

            get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
            auto ret = ctx.tt.optional(ctx.tt.get_iter_elem(obj_type));
            return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ret;
        }

        type_error(loc, std::format("Native Iterators have no method '{}'.", method_name));
        return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
    }

    std::string ffi_name;
    if (ctx.tt.is_array(obj_type)) {
        ffi_name = "Array::" + method_name;
    }
    if (ctx.tt.is_string(obj_type)) {
        ffi_name = "String::" + method_name;
    }
    if (ctx.tt.is_optional(obj_type)) {
        ffi_name = "Optional::" + method_name;
    }

    if (ctx.tt.is_optional(obj_type) && method_name == "get") {
        auto base_type = ctx.tt.get_optional_base(obj_type);

        if (arguments_copy.empty()) {
            return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = base_type;
        } else {
            if (!bind_intrinsic({"fallback"}, "get")) {
                get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
                return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
            }
            auto arg_type = check_expr(arguments_copy[0].value);

            if (ctx.tt.is_string(arg_type)) {
                get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
                return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = base_type;
            } else if (ctx.tt.is_function(arg_type)) {
                auto c_type = ctx.tt.get(arg_type).as<types::Function_type>();
                if (!c_type.params.empty()) {
                    type_error(ast::get_loc(ctx.tree.get(arguments_copy[0].value).node), "Closure passed to 'get' must take no arguments.");
                }
                if (!is_compatible(base_type, effective_return_type(c_type, ctx.tt))) {
                    type_error(ast::get_loc(ctx.tree.get(arguments_copy[0].value).node), "Closure return type mismatch.");
                }
                get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
                return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = base_type;
            } else {
                type_error(loc, "Argument to 'get' must be a string or a closure.");
                get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
                return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
            }
        }
    }
    if (ctx.tt.is_optional(obj_type) && method_name == "value_or") {
        auto base_type = ctx.tt.get_optional_base(obj_type);

        if (!bind_intrinsic({"fallback"}, "value_or")) {
            get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
            return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }

        auto fallback_type = check_expr(arguments_copy[0].value, base_type);

        if (!is_compatible(base_type, fallback_type)) {
            type_error(
                ast::get_loc(ctx.tree.get(arguments_copy[0].value).node),
                std::format(
                    "Fallback type must match the optional's base type. Expected '{}', got '{}'.",
                    ctx.tt.to_string(base_type),
                    ctx.tt.to_string(fallback_type)));
        }

        get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
        return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = base_type;
    }

    if (ctx.tt.is_optional(obj_type) && method_name == "or_else") {
        if (!bind_intrinsic({"fallback"}, "or_else")) {
            get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
            return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }
        auto closure_type = check_expr(arguments_copy[0].value);
        if (!ctx.tt.is_function(closure_type)) {
            type_error(ast::get_loc(ctx.tree.get(arguments_copy[0].value).node), "Argument to 'or_else' must be a closure.");
            get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
            return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }
        auto c_type = ctx.tt.get(closure_type).as<types::Function_type>();
        auto base_type = ctx.tt.get_optional_base(obj_type);
        if (!c_type.params.empty()) {
            type_error(ast::get_loc(ctx.tree.get(arguments_copy[0].value).node), "Closure passed to 'or_else' must take no arguments.");
        }
        if (!is_compatible(base_type, effective_return_type(c_type, ctx.tt))) {
            type_error(ast::get_loc(ctx.tree.get(arguments_copy[0].value).node), "Closure return type mismatch.");
        }
        get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
        return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = base_type;
    }

    if (ctx.tt.is_optional(obj_type) && (method_name == "is_val" || method_name == "has_val" || method_name == "is_nil")) {
        if (!bind_intrinsic({}, method_name)) {
            get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
            return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }
        get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
        return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_bool();
    }

    if (!ffi_name.empty() && ctx.type_env.is_native_defined(ffi_name)) {
        const auto &signatures = *ctx.type_env.get_native_signatures(ffi_name);
        for (size_t sig_index = 0; sig_index < signatures.size(); ++sig_index) {
            auto bound = try_bind_native_arguments(signatures[sig_index].params, arguments_copy, obj_type);
            if (bound.ok) {
                get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(bound.ordered_arguments);
                get_node<ast::Method_call_expr>(ctx.tree, expr_id).native_signature_index = static_cast<int>(sig_index);
                auto ret = parse_type_string(signatures[sig_index].ret_type_str, bound.generics);
                return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ret;
            }
        }
        type_error(loc, std::format("Arguments do not match any native signature for method '{}'.", method_name));
        get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
        return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
    }

    if (method_name == "map") {
        if (!bind_intrinsic({"func"}, "map")) {
            get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
            return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }
        auto closure_type = check_expr(arguments_copy[0].value);
        if (!ctx.tt.is_function(closure_type)) {
            type_error(ast::get_loc(ctx.tree.get(arguments_copy[0].value).node), "Argument to 'map' must be a closure.");
            get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
            return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }
        auto c_type = ctx.tt.get(closure_type).as<types::Function_type>();

        if (ctx.tt.is_array(obj_type)) {
            auto element_type = ctx.tt.get_array_elem(obj_type);
            if (c_type.params.size() != 1 || !is_compatible(c_type.params[0], element_type)) {
                type_error(ast::get_loc(ctx.tree.get(arguments_copy[0].value).node), "Closure parameter mismatch.");
            }
            get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
            auto ret = ctx.tt.array(effective_return_type(c_type, ctx.tt));
            return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ret;
        }
        if (ctx.tt.is_optional(obj_type)) {
            auto base_type = ctx.tt.get_optional_base(obj_type);
            if (c_type.params.size() != 1 || !is_compatible(c_type.params[0], base_type)) {
                type_error(ast::get_loc(ctx.tree.get(arguments_copy[0].value).node), "Closure parameter mismatch.");
            }
            get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
            auto ret = ctx.tt.optional(effective_return_type(c_type, ctx.tt));
            return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ret;
        }
        type_error(loc, ".map() can only be called on arrays and optionals.");
        get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
        return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
    }

    if (ctx.tt.is_optional(obj_type)) {
        type_error(loc, "Cannot call method on an optional type. Unwrap first.");
        get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
        return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
    }

    if (ctx.tt.is_model(obj_type)) {
        auto &model_name = std::get<types::Model_type>(ctx.tt.get(obj_type).data).name;
        auto model_data_ptr = ctx.type_env.get_model(model_name);

        if (model_data_ptr && model_data_ptr->methods.contains(method_name)) {
            const auto &method_decl_id = model_data_ptr->methods.at(method_name).declaration;
            auto decl = std::get_if<ast::Function_stmt>(&ctx.tree.get(method_decl_id).node);

            std::vector<ast::Call_argument> ufcs_args;

            // NEW: Idempotent re-check guard. Since AST IDs are strictly unique per node in the Arena,
            // if arguments_copy[0] exactly matches obj_id, it was already safely prepended!
            bool already_bound = !arguments_copy.empty() && arguments_copy[0].value == obj_id;

            if (already_bound) {
                ufcs_args = arguments_copy;
            } else {
                ufcs_args.push_back({"", obj_id, loc});
                for (auto &arg : arguments_copy) {
                    ufcs_args.push_back(arg);
                }
            }

            auto bound = bind_call_arguments(decl->parameters, ufcs_args, loc, "method", method_name);
            get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(bound.ordered_arguments);

            if (!bound.ok) {
                return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type =
                           effective_return_type(declared_return_types(decl->returns, ctx.tt), ctx.tt);
            }

            return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type =
                       effective_return_type(declared_return_types(decl->returns, ctx.tt), ctx.tt);
        }

        std::string native_model_method_name = model_name + "::" + method_name;
        if (!model_name.empty() && ctx.type_env.is_native_defined(native_model_method_name)) {
            const auto &signatures = *ctx.type_env.get_native_signatures(native_model_method_name);
            for (size_t sig_index = 0; sig_index < signatures.size(); ++sig_index) {
                auto bound = try_bind_native_arguments(signatures[sig_index].params, arguments_copy, obj_type);
                if (bound.ok) {
                    get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(bound.ordered_arguments);
                    get_node<ast::Method_call_expr>(ctx.tree, expr_id).native_signature_index = static_cast<int>(sig_index);
                    auto ret = parse_type_string(signatures[sig_index].ret_type_str, bound.generics);
                    return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ret;
                }
            }

            type_error(loc, std::format("Arguments do not match any native signature for method '{}'.", method_name));
            get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
            return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }

        auto &model_t = std::get<types::Model_type>(ctx.tt.get(obj_type).data);
        for (size_t i = 0; i < model_t.fields.size(); ++i) {
            if (model_t.fields[i].first == method_name) {
                auto field_type = model_t.fields[i].second;
                if (ctx.tt.is_function(field_type)) {
                    get_node<ast::Method_call_expr>(ctx.tree, expr_id).is_closure_field = true;
                    get_node<ast::Method_call_expr>(ctx.tree, expr_id).field_index = static_cast<uint8_t>(i);
                    auto func_type = ctx.tt.get(field_type).as<types::Function_type>();

                    if (arguments_copy.size() != func_type.params.size()) {
                        type_error(loc, "Incorrect number of arguments for closure field.");
                    } else {
                        for (size_t j = 0; j < arguments_copy.size(); ++j) {
                            auto arg_res = check_expr(arguments_copy[j].value, func_type.params[j]);
                            if (!is_compatible(func_type.params[j], arg_res)) {
                                type_error(ast::get_loc(ctx.tree.get(arguments_copy[j].value).node), "Argument type mismatch.");
                            }
                        }
                    }
                    get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
                    return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = effective_return_type(func_type, ctx.tt);
                }
            }
        }
    }

    type_error(loc, std::format("No method or closure field named '{}' in {}", method_name, ctx.tt.to_string(obj_type)));
    get_node<ast::Method_call_expr>(ctx.tree, expr_id).arguments = std::move(arguments_copy);
    return get_node<ast::Method_call_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
}

} // namespace phos
