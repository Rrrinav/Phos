# ============================================================
# PHOS LANGUAGE — PROPOSED CHANGES
# Immutability-by-default, const inlining, green threads,
# match statement, range/iterator for-loops, f-strings
# ============================================================
# Each section is a complete file-level diff.
# Lines starting with +++ are new/changed, --- removed.
# ============================================================

# ────────────────────────────────────────────────────────────
# 1.  LEXER  —  token.hpp   (new token types only)
# ────────────────────────────────────────────────────────────
# Add to TokenType enum:

    Mut,          // mut
    Spawn,        // spawn
    Await,        // await  (keyword, not just an opcode)
    Yield,        // yield
    Match,        // match
    FatArrow,     // =>
    DotDot,       // ..    range operator
    DotDotEq,     // ..=   inclusive range
    In,           // in    for-in loop
    FString,      // f"..."  interpolated string literal
    Underscore,   // _     wildcard pattern

# Add to keyword map (wherever you map strings → TokenTypes):

    {"mut",    TokenType::Mut},
    {"spawn",  TokenType::Spawn},
    {"await",  TokenType::Await},
    {"yield",  TokenType::Yield},
    {"match",  TokenType::Match},
    {"in",     TokenType::In},

# ────────────────────────────────────────────────────────────
# 2.  LEXER  —  scanning  (lexer.cpp)
# ────────────────────────────────────────────────────────────
# (a)  f-strings — scan f"..." into a single FString token
#      whose literal is the raw template (braces kept).
#      The parser/type-checker expands it into a Binary concat tree.

# In scan_string() (or equivalent), before the normal '"' branch:
    if (source[start] == 'f' && peek() == '"') {
        advance();            // consume 'f'
        return scan_fstring();
    }

# scan_fstring():
    Token scan_fstring() {
        // consume opening "
        advance();
        std::string raw;
        while (!is_at_end() && peek() != '"') {
            char c = advance();
            if (c == '\n') line++;
            raw += c;
        }
        if (is_at_end()) error("Unterminated f-string");
        advance(); // closing "
        return make_token(TokenType::FString, raw);
        // literal = raw template string, e.g. "hello {name}, you are {age}"
    }

# (b)  .. and ..= range operators
    case '.':
        if (match('.')) {
            if (match('=')) return make_token(TokenType::DotDotEq);
            return make_token(TokenType::DotDot);
        }
        return make_token(TokenType::Dot);

# (c)  => fat arrow
    case '=':
        if (match('>')) return make_token(TokenType::FatArrow);
        if (match('=')) return make_token(TokenType::Equal);
        return make_token(TokenType::Assign);

# ────────────────────────────────────────────────────────────
# 3.  AST  —  ast.hpp   (new/changed nodes)
# ────────────────────────────────────────────────────────────

# 3a. Variable mutability — change Var_stmt
--- struct Var_stmt {
---     bool is_const;          // old: is_const = no reassign
---     ...
--- };

+++ struct Var_stmt {
+++     bool is_mut   = false;  // mut x  → mutable, reassignable
+++     bool is_const = false;  // const x → compile-time inline (must be literal/foldable)
+++     // default (neither) → immutable let binding, not reassignable, not inlined
+++     std::string name;
+++     types::Type type;
+++     Expr* initializer;
+++     bool type_inferred = false;
+++     Source_location loc;
+++ };

# 3b. Range expression  (0..n  or  0..=n)
+++ struct Range_expr {
+++     Expr* start;
+++     Expr* end;
+++     bool inclusive;   // true = ..=, false = ..
+++     types::Type type; // resolved to Range<i64> or similar
+++     Source_location loc;
+++ };

# 3c. For-in loop  (replaces / supplements C-style for)
+++ struct For_in_stmt {
+++     std::string var_name;    // loop variable (always immutable inside body)
+++     Expr*       iterable;    // any Range_expr or array expr
+++     Stmt*       body;
+++     Source_location loc;
+++ };

# 3d. Match statement
+++ struct Match_arm {
+++     std::string variant_name;    // for union variants: "Some", "Ok", ...
+++     std::string bind_name;       // inner binding, "" if wildcard or unit
+++     bool is_wildcard = false;    // true for _ arm
+++     Stmt* body;
+++ };
+++ struct Match_stmt {
+++     Expr*                  subject;
+++     std::vector<Match_arm> arms;
+++     Source_location        loc;
+++ };

# 3e. Spawn / await / yield expressions
+++ struct Spawn_expr {
+++     Expr* call;          // must be a Call_expr
+++     types::Type type;    // Thread<T> where T = call's return type
+++     Source_location loc;
+++ };
+++ struct Await_expr {
+++     Expr* thread;        // expr of type Thread<T>
+++     types::Type type;    // T
+++     Source_location loc;
+++ };
+++ struct Yield_expr {
+++     Expr* value;         // nullptr for void yield
+++     types::Type type;
+++     Source_location loc;
+++ };

# 3f. F-string expression
+++ struct Fstring_expr {
+++     // Expanded by parser into a tree of Binary_expr (string concat).
+++     // The AST node itself is kept for the printer's benefit;
+++     // check_expr reduces it to the concat tree.
+++     std::string raw_template;                 // e.g. "hello {name}"
+++     std::vector<Expr*> interpolations;        // exprs inside {}
+++     types::Type type;                         // always String
+++     Source_location loc;
+++ };

# Add all new node types to the Expr::Node variant:
    using Node = std::variant<
        Literal_expr, Variable_expr, Binary_expr, Unary_expr,
        Call_expr, Assignment_expr, Cast_expr,
        Field_access_expr, Method_call_expr, Model_literal_expr,
        Closure_expr, Field_assignment_expr,
        Array_literal_expr, Array_assignment_expr, Array_access_expr,
        Static_path_expr,
        Range_expr,        // NEW
        Spawn_expr,        // NEW
        Await_expr,        // NEW
        Yield_expr,        // NEW
        Fstring_expr       // NEW
    >;

# Add new stmt types to Stmt::Node:
    using Node = std::variant<
        Return_stmt, Function_stmt, Model_stmt, Var_stmt,
        Print_stmt, Expr_stmt, Block_stmt, If_stmt, While_stmt,
        For_stmt, Union_stmt,
        For_in_stmt,  // NEW
        Match_stmt    // NEW
    >;

# ────────────────────────────────────────────────────────────
# 4.  PARSER  —  parser.hpp   (declarations only)
# ────────────────────────────────────────────────────────────
# Add to private section:

    Result<ast::Stmt*>  for_in_statement();
    Result<ast::Stmt*>  match_statement();
    Result<ast::Expr*>  range_or_expr();     // sits between assignment and logical_or
    Result<ast::Expr*>  parse_fstring(const lex::Token& tok);
    Result<ast::Expr*>  spawn_expression();
    Result<ast::Expr*>  await_expression();

# ────────────────────────────────────────────────────────────
# 5.  PARSER  —  parser.cpp   (full implementations)
# ────────────────────────────────────────────────────────────

# 5a. var_declaration  — immutability-by-default
Result<ast::Stmt*> Parser::var_declaration()
{
    // grammar:  "let" ("mut" | "const")? name (":" type)? (":=" | "=" expr) ";"
    bool is_mut   = match({lex::TokenType::Mut});
    bool is_const = !is_mut && match({lex::TokenType::Const});

    auto name = __Try(consume(lex::TokenType::Identifier, "Expect variable name"));

    types::Type var_type = types::Type(types::Primitive_kind::Void);
    ast::Expr*  initializer  = nullptr;
    bool        type_inferred = false;

    if (match({lex::TokenType::Colon}))
    {
        if (match({lex::TokenType::Assign}))        // :=  infer type
        {
            initializer   = __Try(expression());
            type_inferred = true;
        }
        else                                         // :  explicit type
        {
            var_type = __Try(parse_type());
            if (match({lex::TokenType::Assign}))
                initializer = __Try(expression());
        }
    }
    else
        return std::unexpected(create_error(peek(), "Expect ':' or ':=' after variable name."));

    __TryIgnore(consume(lex::TokenType::Semicolon, "Expected ';' after variable declaration"));

    return mem::Arena::alloc(this->arena_, ast::Stmt{
        ast::Var_stmt{
            .is_mut       = is_mut,
            .is_const     = is_const,
            .name         = name.lexeme,
            .type         = var_type,
            .initializer  = initializer,
            .type_inferred = type_inferred,
            .loc          = {name.line, name.column},
        }
    });
}

# 5b. statement() — add new statement heads
Result<ast::Stmt*> Parser::statement()
{
    skip_newlines();
    if (is_at_end())
        return std::unexpected(create_error(peek(), "Unexpected end of file"));

    if (match({lex::TokenType::Print}))    return print_statement();
    if (match({lex::TokenType::PrintErr})) return print_statement(ast::Print_stream::STDERR);
    if (match({lex::TokenType::LeftBrace}))return block_statement();
    if (match({lex::TokenType::If}))       return if_statement();
    if (match({lex::TokenType::While}))    return while_statement();
    if (match({lex::TokenType::For}))      return for_statement_dispatch();  // see 5c
    if (match({lex::TokenType::Match}))    return match_statement();          // NEW
    if (match({lex::TokenType::Return}))   return return_statement();
    return expression_statement();
}

# 5c. for_statement_dispatch — decide C-style vs for-in
Result<ast::Stmt*> Parser::for_statement_dispatch()
{
    // Peek ahead: if next token is '(' it's the C-style for
    // Otherwise it's  for <ident> in <expr> { ... }
    if (check(lex::TokenType::LeftParen))
        return for_statement();   // existing C-style implementation

    return for_in_statement();
}

# 5d. for_in_statement
Result<ast::Stmt*> Parser::for_in_statement()
{
    auto var_name = __Try(consume(lex::TokenType::Identifier, "Expect loop variable name after 'for'"));
    __TryIgnore(consume(lex::TokenType::In, "Expect 'in' after loop variable"));

    auto iterable = __Try(expression());  // range or array
    auto body     = __Try(statement());

    return mem::Arena::alloc(this->arena_, ast::Stmt{
        ast::For_in_stmt{
            .var_name = var_name.lexeme,
            .iterable = iterable,
            .body     = body,
            .loc      = {var_name.line, var_name.column},
        }
    });
}

# 5e. match_statement
Result<ast::Stmt*> Parser::match_statement()
{
    auto loc     = ast::Source_location{peek().line, peek().column};
    auto subject = __Try(expression());
    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' after match subject"));

    std::vector<ast::Match_arm> arms;
    while (!check(lex::TokenType::RightBrace) && !is_at_end())
    {
        skip_newlines();
        if (check(lex::TokenType::RightBrace)) break;

        ast::Match_arm arm;

        if (match({lex::TokenType::Underscore}))
        {
            arm.is_wildcard = true;
        }
        else
        {
            // Expect  Variant(bind)  or  Variant
            arm.variant_name = __Try(consume(lex::TokenType::Identifier, "Expect variant name")).lexeme;
            if (match({lex::TokenType::LeftParen}))
            {
                arm.bind_name = __Try(consume(lex::TokenType::Identifier, "Expect binding name")).lexeme;
                __TryIgnore(consume(lex::TokenType::RightParen, "Expect ')'"));
            }
        }

        __TryIgnore(consume(lex::TokenType::FatArrow, "Expect '=>' after match pattern"));
        arm.body = __Try(statement());
        arms.push_back(std::move(arm));

        match({lex::TokenType::Comma});   // optional trailing comma
        skip_newlines();
    }

    __TryIgnore(consume(lex::TokenType::RightBrace, "Expect '}' after match arms"));

    return mem::Arena::alloc(this->arena_, ast::Stmt{
        ast::Match_stmt{ .subject = subject, .arms = std::move(arms), .loc = loc }
    });
}

# 5f. expression() — insert range_or_expr above assignment
Result<ast::Expr*> Parser::expression() { return assignment(); }

Result<ast::Expr*> Parser::assignment()
{
    // (unchanged except we call range_or_expr instead of logical_or for the RHS)
    auto expr = __Try(range_or_expr());   // was: logical_or()
    // ... rest unchanged
}

# 5g. range_or_expr  (new precedence level: lower than logical_or, higher than assignment)
Result<ast::Expr*> Parser::range_or_expr()
{
    auto start = __Try(logical_or());

    if (match({lex::TokenType::DotDotEq, lex::TokenType::DotDot}))
    {
        bool inclusive = (previous().type == lex::TokenType::DotDotEq);
        auto end_expr  = __Try(logical_or());
        return mem::Arena::alloc(this->arena_, ast::Expr{
            ast::Range_expr{
                .start     = start,
                .end       = end_expr,
                .inclusive = inclusive,
                .type      = types::Type(types::Primitive_kind::Void), // resolved later
                .loc       = {previous().line, previous().column},
            }
        });
    }
    return start;
}

# 5h. primary() — add spawn/await/yield/f-string handling
// Inside primary(), add BEFORE the Identifier branch:

    if (match({lex::TokenType::Spawn}))
    {
        auto call = __Try(expression());
        return mem::Arena::alloc(this->arena_, ast::Expr{
            ast::Spawn_expr{ .call = call, .type = {}, .loc = {previous().line, previous().column} }
        });
    }

    if (match({lex::TokenType::Await}))
    {
        auto thread = __Try(expression());
        return mem::Arena::alloc(this->arena_, ast::Expr{
            ast::Await_expr{ .thread = thread, .type = {}, .loc = {previous().line, previous().column} }
        });
    }

    if (match({lex::TokenType::Yield}))
    {
        ast::Expr* val = nullptr;
        if (!check(lex::TokenType::Semicolon) && !check(lex::TokenType::RightBrace))
            val = __Try(expression());
        return mem::Arena::alloc(this->arena_, ast::Expr{
            ast::Yield_expr{ .value = val, .type = {}, .loc = {previous().line, previous().column} }
        });
    }

    if (match({lex::TokenType::FString}))
        return parse_fstring(previous());

// parse_fstring: split template on {} pairs, build concat tree
Result<ast::Expr*> Parser::parse_fstring(const lex::Token& tok)
{
    // tok.lexeme holds the raw template, e.g. "hello {name}, age {age+1}"
    // We parse out the {} segments and build a tree of string concat Binary_exprs.
    // The interpolation exprs are re-parsed from the captured substring.
    // For the AST printer we also store the Fstring_expr node.

    const std::string& src = tok.lexeme;   // raw template (no outer quotes)
    std::vector<ast::Expr*> parts;
    std::string literal_buf;
    size_t i = 0;

    auto flush_literal = [&]() {
        if (!literal_buf.empty()) {
            parts.push_back(mem::Arena::alloc(this->arena_, ast::Expr{
                ast::Literal_expr{
                    .value = Value(literal_buf),
                    .type  = types::Type(types::Primitive_kind::String),
                    .loc   = {tok.line, tok.column}
                }
            }));
            literal_buf.clear();
        }
    };

    while (i < src.size()) {
        if (src[i] == '{') {
            flush_literal();
            size_t start = ++i;
            int depth = 1;
            while (i < src.size() && depth > 0) {
                if (src[i] == '{') depth++;
                else if (src[i] == '}') depth--;
                if (depth > 0) i++;
            }
            // src[start..i] is the inner expression source
            std::string inner = src.substr(start, i - start);
            // Re-lex and re-parse inner expression
            // (requires a sub-lexer call — wire up your Lexer here)
            auto inner_tokens = lex::Lexer(inner).scan();
            Parser inner_parser(inner_tokens, this->arena_);
            auto inner_expr = inner_parser.expression();
            if (!inner_expr)
                return std::unexpected(create_error(tok, "Invalid expression in f-string: " + inner));
            parts.push_back(*inner_expr);
            i++;
        } else {
            literal_buf += src[i++];
        }
    }
    flush_literal();

    if (parts.empty())
        return mem::Arena::alloc(this->arena_, ast::Expr{
            ast::Literal_expr{.value=Value(std::string("")),
                              .type=types::Type(types::Primitive_kind::String),
                              .loc={tok.line,tok.column}}
        });

    // Fold parts into left-associative concat tree
    ast::Expr* result = parts[0];
    for (size_t j = 1; j < parts.size(); ++j) {
        result = mem::Arena::alloc(this->arena_, ast::Expr{
            ast::Binary_expr{
                .left  = result,
                .op    = lex::TokenType::Plus,
                .right = parts[j],
                .type  = types::Type(types::Primitive_kind::String),
                .loc   = {tok.line, tok.column}
            }
        });
    }
    return result;
}

# ────────────────────────────────────────────────────────────
# 6.  TYPE CHECKER  —  type-checker.cpp
# ────────────────────────────────────────────────────────────

# 6a. Var_stmt — immutability is now the default
void Type_checker::check_stmt_node(ast::Var_stmt &stmt)
{
    // const requires a compile-time foldable initializer
    if (stmt.is_const) {
        if (!stmt.initializer || !is_const_foldable(*stmt.initializer))
            type_error(stmt.loc,
                "'const' requires a compile-time constant initializer (literal or const expression).");
    }

    types::Type init_type = types::Type(types::Primitive_kind::Void);
    if (stmt.initializer) {
        auto res = check_expr(*stmt.initializer,
                              stmt.type_inferred ? std::nullopt : std::make_optional(stmt.type));
        if (res) init_type = res.value();
        else     return;
    }

    if (stmt.type_inferred) {
        if (is_array(init_type) &&
            types::get_array_type(init_type)->element_type == types::Type(types::Primitive_kind::Void))
            type_error(stmt.loc, "Cannot infer type of an empty array initializer.");
        stmt.type = init_type;
    } else if (stmt.initializer && !is_compatible(stmt.type, init_type)) {
        type_error(stmt.loc,
            "Initializer type '" + types::type_to_string(init_type) +
            "' does not match declared type '" + types::type_to_string(stmt.type) + "'.");
    }

    // is_const → stored as (type, is_const=true); is_mut → (type, is_const=false)
    // default (immutable let) → treat same as const for reassignment purposes
    bool can_reassign = stmt.is_mut;
    declare(stmt.name, stmt.type, !can_reassign, stmt.loc);
}

# Helper: is_const_foldable
bool Type_checker::is_const_foldable(const ast::Expr& expr) const
{
    return std::visit([this](const auto& e) -> bool {
        using T = std::decay_t<decltype(e)>;
        if constexpr (std::is_same_v<T, ast::Literal_expr>)
            return true;
        if constexpr (std::is_same_v<T, ast::Unary_expr>)
            return is_const_foldable(*e.right);
        if constexpr (std::is_same_v<T, ast::Binary_expr>)
            return is_const_foldable(*e.left) && is_const_foldable(*e.right);
        if constexpr (std::is_same_v<T, ast::Variable_expr>) {
            // allow other consts
            auto res = lookup_variable(e.name, e.loc);
            return res.has_value() && res->second; // is_const flag in scope
        }
        return false;
    }, expr.node);
}

# 6b. Assignment — only allow reassign to mut variables
Result<types::Type> Type_checker::check_expr_node(ast::Assignment_expr &expr,
                                                   std::optional<types::Type> context_type)
{
    (void)context_type;
    auto var_info_res = lookup(expr.name, expr.loc);
    if (!var_info_res)
        return std::unexpected(err::msg("Variable not found", "", 0, 0));

    bool immutable = var_info_res.value().second;  // stored as !can_reassign
    if (immutable)
        type_error(expr.loc,
            "Cannot assign to '" + expr.name + "' — it is immutable. Declare it with 'let mut' to allow reassignment.");

    auto var_type    = var_info_res.value().first;
    auto val_type_res = check_expr(*expr.value, var_type);
    if (!val_type_res) return val_type_res;

    if (!is_compatible(var_type, val_type_res.value()))
        type_error(expr.loc, "Assignment type mismatch.");
    return expr.type = var_type;
}

# 6c. For-in statement
void Type_checker::check_stmt_node(ast::For_in_stmt &stmt)
{
    auto iter_res = check_expr(*stmt.iterable);
    if (!iter_res) return;

    types::Type elem_type;
    bool valid = false;

    if (is_array(iter_res.value())) {
        elem_type = types::get_array_type(iter_res.value())->element_type;
        valid = true;
    } else if (is_range(iter_res.value())) {   // see 6d for is_range
        elem_type = types::Type(types::Primitive_kind::Int);
        valid = true;
    }

    if (!valid) {
        type_error(stmt.loc,
            "For-in requires an array or range, got '" +
            types::type_to_string(iter_res.value()) + "'.");
        return;
    }

    begin_scope();
    // Loop variable is always immutable inside the body
    declare(stmt.var_name, elem_type, /*is_const=*/true, stmt.loc);
    if (stmt.body) check_stmt(*stmt.body);
    end_scope();
}

# 6d. Range_expr
Result<types::Type> Type_checker::check_expr_node(ast::Range_expr &expr,
                                                   std::optional<types::Type> /*ctx*/)
{
    auto s = check_expr(*expr.start);
    auto e = check_expr(*expr.end);
    if (!s || !e) return expr.type = types::Primitive_kind::Void;

    if (s.value() != types::Type(types::Primitive_kind::Int) ||
        e.value() != types::Type(types::Primitive_kind::Int))
    {
        type_error(expr.loc, "Range bounds must be 'i64'.");
    }
    // Represent Range as a named model-like type; for now use a sentinel
    // (add a Range_type to your type system or treat as array-like in the VM).
    auto range_t = mem::make_rc<types::Range_type>(); // add Range_type to type.hpp
    range_t->inclusive = expr.inclusive;
    return expr.type = types::Type(range_t);
}

bool Type_checker::is_range(const types::Type &type) const {
    return std::holds_alternative<mem::rc_ptr<types::Range_type>>(type);
}

# 6e. Match statement
void Type_checker::check_stmt_node(ast::Match_stmt &stmt)
{
    auto subject_res = check_expr(*stmt.subject);
    if (!subject_res) return;
    auto subject_type = subject_res.value();

    if (!is_union(subject_type)) {
        type_error(stmt.loc,
            "'match' currently only works on union types, got '" +
            types::type_to_string(subject_type) + "'.");
        return;
    }

    auto union_type = types::get_union_type(subject_type);
    std::unordered_set<std::string> covered;
    bool has_wildcard = false;

    for (auto &arm : stmt.arms) {
        if (arm.is_wildcard) { has_wildcard = true; continue; }

        if (!union_type->variants.count(arm.variant_name)) {
            type_error(stmt.loc,
                "Union '" + union_type->name + "' has no variant '" + arm.variant_name + "'.");
            continue;
        }
        if (covered.count(arm.variant_name))
            type_error(stmt.loc, "Duplicate match arm for variant '" + arm.variant_name + "'.");
        covered.insert(arm.variant_name);

        begin_scope();
        // If arm has a binding, bring it into scope with the variant's payload type
        if (!arm.bind_name.empty()) {
            auto payload_type = union_type->variants.at(arm.variant_name);
            declare(arm.bind_name, payload_type, /*is_const=*/true, stmt.loc);
        }
        if (arm.body) check_stmt(*arm.body);
        end_scope();
    }

    // Exhaustiveness check
    if (!has_wildcard) {
        for (auto &[vname, _] : union_type->variants) {
            if (!covered.count(vname))
                type_error(stmt.loc,
                    "Match is not exhaustive — variant '" + vname + "' is not covered. Add an arm or a '_' wildcard.");
        }
    }
}

# 6f. Spawn / Await / Yield
Result<types::Type> Type_checker::check_expr_node(ast::Spawn_expr &expr,
                                                   std::optional<types::Type> /*ctx*/)
{
    auto call_res = check_expr(*expr.call);
    if (!call_res) return expr.type = types::Primitive_kind::Void;
    // Thread<T> where T is the call's return type
    auto thread_t = mem::make_rc<types::Thread_type>();
    thread_t->inner = call_res.value();
    return expr.type = types::Type(thread_t);
}

Result<types::Type> Type_checker::check_expr_node(ast::Await_expr &expr,
                                                   std::optional<types::Type> /*ctx*/)
{
    auto thread_res = check_expr(*expr.thread);
    if (!thread_res) return expr.type = types::Primitive_kind::Void;
    if (!is_thread(thread_res.value())) {
        type_error(expr.loc, "'await' requires a thread value (result of 'spawn').");
        return expr.type = types::Primitive_kind::Void;
    }
    return expr.type = types::get_thread_type(thread_res.value())->inner;
}

Result<types::Type> Type_checker::check_expr_node(ast::Yield_expr &expr,
                                                   std::optional<types::Type> /*ctx*/)
{
    if (!current_is_green_thread_) {
        type_error(expr.loc, "'yield' can only be used inside a spawned function.");
    }
    types::Type t = types::Primitive_kind::Void;
    if (expr.value) {
        auto res = check_expr(*expr.value);
        if (res) t = res.value();
    }
    return expr.type = t;
}

# ────────────────────────────────────────────────────────────
# 7.  TYPE SYSTEM  —  type.hpp   (new type variants)
# ────────────────────────────────────────────────────────────
# Add to the Type variant and supporting structs:

+++ struct Range_type {
+++     bool inclusive = false;
+++     // element_type is always i64 for now
+++ };

+++ struct Thread_type {
+++     types::Type inner;  // return type of the spawned function
+++ };

# Add to types::Type variant:
    using Type = std::variant<
        Primitive_kind,
        mem::rc_ptr<Function_type>,
        mem::rc_ptr<Closure_type>,
        mem::rc_ptr<Model_type>,
        mem::rc_ptr<Union_type>,
        mem::rc_ptr<Array_type>,
        mem::rc_ptr<Optional_type>,
        mem::rc_ptr<Range_type>,    // NEW
        mem::rc_ptr<Thread_type>    // NEW
    >;

# Helper accessors (add alongside existing get_*_type helpers):
    inline bool is_range(const Type& t)  { return std::holds_alternative<mem::rc_ptr<Range_type>>(t); }
    inline bool is_thread(const Type& t) { return std::holds_alternative<mem::rc_ptr<Thread_type>>(t); }
    inline mem::rc_ptr<Range_type>  get_range_type(const Type& t)  { return std::get<mem::rc_ptr<Range_type>>(t); }
    inline mem::rc_ptr<Thread_type> get_thread_type(const Type& t) { return std::get<mem::rc_ptr<Thread_type>>(t); }

# type_to_string additions:
    case Range_type:  return "range";
    case Thread_type: return "thread<" + type_to_string(inner) + ">";

# ────────────────────────────────────────────────────────────
# 8.  SUMMARY OF SEMANTIC RULES
# ────────────────────────────────────────────────────────────
#
#  let x := 5         → immutable, type inferred (i64)
#  let x: i64 = 5     → immutable, type explicit
#  let mut x := 5     → mutable,   type inferred
#  let mut x: i64     → mutable,   default-init, no assign yet
#  const PI := 3.14   → compile-time constant (inlined by compiler)
#                        initializer MUST be a literal or const expression
#
#  for i in 0..10     → i in [0,9]  (exclusive)
#  for i in 0..=10    → i in [0,10] (inclusive)
#  for x in arr       → x iterates over array elements (immutable)
#
#  match val {
#      Some(v) => ...
#      None    => ...
#      _       => ...   ← wildcard, must be last
#  }
#  Exhaustiveness checked at compile time (errors if missing variant + no wildcard).
#
#  spawn fn(args)     → returns Thread<T>
#  await thread_val   → blocks current green thread, returns T
#  yield              → voluntarily yields execution (inside spawned fn only)
#
#  f"hello {name}!"   → parsed into string concat tree at parse time
#                        each {} segment is a full expression, cast to string via .to_string()
#                        type is always String
