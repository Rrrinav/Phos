# Compiler context

Name of this struct maybe misleading, it is used for both frontend and code-gen.

It contains 6 members:

``` cpp
struct Compiler_context
{
    mem::Arena &arena;
    ast::Ast_tree tree;
    types::Type_table tt;
    Workspace workspace;
    Symbol_registry registry;
    Type_environment type_env;
};
```

## 1. Arena

This is just the main memory pool for the system. Interally implemented as block based bump allocator.

## 2. Tree

This holds information about all the types. Every type is this system is represented by an Id, which is just an index into a global types array.
These types reference to other types using indices to form more complicated types.

Type table is just a container managing those types.

## 3. Workspace

A workspace contains 3 members.

```cpp
class Workspace
{
    std::vector<Module_unit> modules;
    std::unordered_map<std::string, Module_id> path_to_module_;
    std::unordered_map<std::string, Module_id> namespace_to_module_;
};
```

### Module_unit

```cpp
struct Module_unit
{
    Module_id id;
    std::string logical_namespace;
    std::filesystem::path os_path;

    std::vector<ast::Stmt_id> ast_roots;
    std::unordered_map<std::string, Module_id> imports;
    std::unordered_map<std::string, Module_id> exported_modules;
    std::unordered_map<std::string, Symbol_id> exported_symbols;
}
```

- `Module_id`: A module id is just a unique identifier for each module.
- `os_path`: It is just the path to the module from the root.
- `logical_namespace`: The name of logical namespace that that module will follow in finalized program.
- `ast_roots`: All the top level statements in AST belonging to that specific module.
- `imports`: All the imported modules in that module.
- `exported_modules`: All the modules it exports.
- `exported_symbols`: All the symbols it exports.
---

1. `Modules`: Contains list of all the modules.
2. `Path to module`: Contains mapping from OS paths to module.
2. `Namespace to module`: Contains mapping from logical namespace to module.

## 4. Ast_tree

This holds the actual Abstract Syntax Tree. It stores every single parsed expression and statement node from all loaded files in a giant, flat array. Instead of using raw pointers, nodes reference each other using lightweight `Expr_id` and `Stmt_id` indices.

## 5. Type_table

This holds information about all the types in the system. Every type is represented by a `Type_id`, which is just an index into a global types array. These types reference other types using indices to form more complicated structures (like arrays, optionals, or functions). The Type table is the central container managing these structural definitions.

## 6. Symbol_registry

The Symbol Registry is the absolute "master phonebook" of the compiler. It is the single source of truth for any named entity that needs to be referenced across different scopes or files.

```cpp
class Symbol_registry
{
    std::vector<Symbol> symbols;
    std::unordered_map<std::string, Symbol_id> global_index;
};
```

### Symbol

```cpp
struct Symbol
{
    Symbol_id id;
    std::string name; 
    Symbol_kind kind;
    types::Type_id type;

    Module_id owner_module;
    bool is_public;

    std::optional<Value> const_value;
    std::optional<uint16_t> stack_offset;
    std::optional<size_t> ffi_index;

    ast::Stmt_id declaration;
};
```

- `id`: The fast, unique integer identifier used by the compiler and Virtual Machine.
- `name`: The canonical, globally unique name of the symbol (e.g., `"std::math::sin"`).
- `kind`: An enum defining what this symbol actually is (`Local_var`, `Global_var`, `Native_func`, `Phos_func`, `Model_def`, etc.).
- `type`: The `Type_id` representing the signature or structural type of this symbol.
- `owner_module`: The `Module_id` of the file where this symbol was originally declared.
- `is_public`: A boolean indicating if this symbol can be exported and used by other modules.
- `declaration`: A link back to the exact `Stmt_id` in the AST where this symbol was defined.

The Semantic Checker uses the `global_index` to find symbols by their string name during compilation. Once found, it permanently "stamps" the integer `Symbol_id` onto the AST node. By the time the code reaches the Bytecode Compiler, it completely ignores strings and uses the integer ID to instantly look up the `Symbol` in the `symbols` vector.

## 7. Type_environment

Type environment is completely decoupled from files and purely concerns itself with the semantic rules and relationships of the code.

```cpp
class Type_environment
{
    types::Type_table &tt;

    std::unordered_map<std::string, types::Type_id> global_types;
    std::unordered_map<std::string, Function_type_data> functions;
    std::unordered_map<std::string, Model_type_data> model_data;
    std::unordered_map<std::string, Union_type_data> union_data;
    std::unordered_map<std::string, Enum_type_data> enum_data;

    std::unordered_map<std::string, std::vector<Native_sig>> native_signatures;
    std::unordered_map<std::string, Value> native_constants;
};
```

### Purpose and Responsibilities:
- **Structural Tracking:** While the `Type_table` only knows the types of things and their constituents, `Type_environment` keeps track of ast nodes associated with them, default values, methods associated, etc.
- **Method Resolution:** The `Model_type_data` struct holds dictionaries of all the methods, static fields, and default values belonging to a specific model. When a user types `my_obj.do_something()`, the Semantic Checker asks the `Type_environment` if `do_something` actually exists on that model.
- **FFI Boundary:** The `native_signatures` map is the strict bridge between Phos code and C++ code. It registers the exact parameters and return types expected by your core built-in functions, allowing the compiler to validate native calls before they ever reach the VM.
