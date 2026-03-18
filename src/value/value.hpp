#pragma once

#include <string>
#include <vector>
#include <variant>
#include <cstdint>
#include <cstddef>
#include <utility>

#include "../memory/ref_counted.hpp"
#include "type.hpp"

// --- Forward Declarations ---
namespace phos::vm
{
    struct Chunk;
    class Virtual_machine;
    struct Call_frame;
}  // namespace phos::vm

namespace phos
{

// Forward declare all our reference-counted objects
struct Model_value;
struct Closure_value;
struct Array_value;
struct Union_value;
struct Green_thread_value;

// ============================================================================
// The Core Value Variant (Notice Native_function_value is GONE!)
// ============================================================================
using Value = std::variant<long,
                           double,
                           bool,
                           std::string,
                           mem::rc_ptr<Model_value>,
                           mem::rc_ptr<Closure_value>,
                           mem::rc_ptr<Array_value>,
                           mem::rc_ptr<Union_value>,
                           mem::rc_ptr<Green_thread_value>,
                           std::nullptr_t,
                           std::monostate>;

// Raw C++ Function Pointer for the Zero-Overhead FFI
using Native_fn = Value (*)(vm::Virtual_machine *vm, uint8_t arg_count);

// ============================================================================
// Object Definitions
// ============================================================================

struct Closure_value
{
    std::string name;
    size_t arity;
    types::Function_type signature;

    // If it's a Phos script function:
    mem::rc_ptr<vm::Chunk> chunk = nullptr;

    // If it's a Native C++ function:
    Native_fn native_func = nullptr;

    // Constructor for Bytecode Functions
    Closure_value(std::string n, size_t a, types::Function_type sig, mem::rc_ptr<vm::Chunk> c)
        : name(std::move(n)), arity(a), signature(std::move(sig)), chunk(std::move(c)), native_func(nullptr)
    {
    }

    // Constructor for Native C++ Functions
    Closure_value(std::string n, size_t a, types::Function_type sig, Native_fn nf)
        : name(std::move(n)), arity(a), signature(std::move(sig)), chunk(nullptr), native_func(nf)
    {
    }
};

struct Array_value
{
    types::Type type;
    std::vector<Value> elements;

    Array_value(types::Type t, std::vector<Value> elems) : type(std::move(t)), elements(std::move(elems)) {}
};

struct Model_value
{
    types::Model_type signature;
    std::vector<Value> fields;

    Model_value(types::Model_type sig, std::vector<Value> f) : signature(std::move(sig)), fields(std::move(f)) {}
};

struct Union_value
{
    std::string union_name;
    std::string variant_name;
    Value payload;

    Union_value(std::string u_name, std::string v_name, Value p)
        : union_name(std::move(u_name)), variant_name(std::move(v_name)), payload(std::move(p))
    {
    }
};

struct Green_thread_value
{
    // C++17 allows vectors of incomplete types, so we can use vm::Call_frame here safely
    std::vector<vm::Call_frame> frames;
    std::vector<Value> stack;
    bool is_completed = false;
};

// ============================================================================
// Type Checkers & Getters
// ============================================================================

bool is_nil(const Value &val);
bool is_bool(const Value &val);
bool is_int(const Value &val);
bool is_float(const Value &val);
bool is_string(const Value &val);
bool is_array(const Value &val);
bool is_model(const Value &val);
bool is_closure(const Value &val);

bool get_bool(const Value &val);
long get_int(const Value &val);
double get_float(const Value &val);
std::string get_string(const Value &val);
mem::rc_ptr<Array_value> get_array(const Value &val);
mem::rc_ptr<Model_value> get_model(const Value &val);
mem::rc_ptr<Closure_value> get_closure(const Value &val);

// ============================================================================
// Utility
// ============================================================================

std::string value_to_string(const Value &val);

bool operator==(const Value &a, const Value &b);
bool operator!=(const Value &a, const Value &b);

}  // namespace phos
