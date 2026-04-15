#pragma once

#include "type.hpp"
#include "../vm/instruction.hpp"
#include "../vm/frame.hpp"

#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <stdfloat>

// Forward declare your Arena
namespace phos::mem {
class Arena;
}

namespace phos::numeric {
#if defined(__STDCPP_FLOAT16_T__)
using float16_t = std::float16_t;
inline constexpr bool has_native_float16 = true;
#elif defined(__FLT16_MANT_DIG__)
using float16_t = _Float16;
inline constexpr bool has_native_float16 = true;
#else
struct float16_t
{
    float value = 0.0f;
    constexpr float16_t() = default;
    constexpr float16_t(float v) : value(v)
    {}
    constexpr float16_t(double v) : value(static_cast<float>(v))
    {}
    constexpr operator float() const
    {
        return value;
    }
    auto operator<=>(const float16_t &) const = default;
};
inline constexpr bool has_native_float16 = false;
#endif
} // namespace phos::numeric

namespace phos {

struct Value;

// Raw C-style function pointer for native FFI (VM context passed as void* for now)
using Native_fn = Value (*)(void *vm_context, uint8_t arg_count);

// ============================================================================
// 1. THE RAW DATA STRUCTS (Zero OOP, Zero Std Lib, Arena Allocated)
// ============================================================================

struct String_data
{
    uint32_t length;
    char chars[]; // C99 Flexible Array Member
};

struct Array_data
{
    uint32_t capacity;
    uint32_t count;
    struct Value *elements;
};

struct Model_data
{
    types::Model_type signature;
    uint32_t field_count;
    struct Value *fields;
};

struct Union_data
{
    String_data *union_name;
    String_data *variant_name;
    struct Value *payload;
};

struct Upvalue_data
{
    size_t stack_index;
    bool is_closed;
    struct Value *closed_value;
};

struct Closure_data
{
    String_data *name{};
    std::size_t arity{};
    types::Function_type signature{};

    vm::Instruction *code{nullptr};
    std::size_t code_count{0};

    Value *constants{nullptr};
    std::size_t constant_count{0};

    std::optional<Native_fn> native_func{std::nullopt};

    std::size_t upvalue_count{0};
    Upvalue_data **upvalues{nullptr};
};

struct Green_thread_data
{
    // CONTROL FLOW
    // The history of active function calls (The Call Stack)
    vm::Call_frame *call_stack;
    size_t call_stack_capacity;
    size_t call_stack_count;

    // DATA
    // The flat array of raw memory where all variables and math live.
    struct Value *value_stack;
    size_t value_stack_capacity;

    // LIFECYCLE
    // Is this thread completely finished executing?
    bool is_completed;

    // Variables that need to be saved to the heap before a function returns.
    Upvalue_data **open_upvalues;
    size_t open_upvalue_count;
};

struct Iterator_data
{
    enum class State_type : uint8_t { Empty, Singleton, Interval, Array, String };

    types::Type element_type;
    State_type state_type;
    int64_t cursor;

    // Explicit union replaces std::variant
    union {
        struct Value *singleton_val;
        struct
        {
            int64_t start;
            int64_t end;
            bool inclusive;
        } interval;
        Array_data *array;
        String_data *string;
    } state;
};

// ============================================================================
// 2. THE VALUE API (Strictly 16-Bytes, Safe, Modern C++)
// ============================================================================

enum class Value_tag : uint8_t {
    Nil, Bool,
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F16, F32, F64,
    String, Array, Model, Union,
    Closure, Iterator, Green_thread, Upvalue
};

struct Value
{
private:
    // Memory Payload (Exactly 8 bytes)
    union Payload {
        bool boolean;

        // Exact width mappings
        int8_t i8;
        int16_t i16;
        int32_t i32;
        int64_t i64;
        uint8_t u8;
        uint16_t u16;
        uint32_t u32;
        uint64_t u64;
        numeric::float16_t f16;
        float f32;
        double f64;

        // Arena Pointers
        String_data *str;
        Array_data *arr;
        Model_data *model;
        Union_data *un;
        Closure_data *closure;
        Iterator_data *iter;
        Green_thread_data *gt;
        Upvalue_data *upvalue;

        Payload() : i64(0)
        {}
    } as;

    Value_tag tag_ = Value_tag::Nil;
    uint8_t option_depth_ = 0;

    // Private raw constructors (Only used by the Arena Factories)
    Value(String_data *s, uint8_t d) : tag_(Value_tag::String), option_depth_(d)
    {
        as.str = s;
    }
    Value(Array_data *a, uint8_t d) : tag_(Value_tag::Array), option_depth_(d)
    {
        as.arr = a;
    }
    Value(Model_data *m, uint8_t d) : tag_(Value_tag::Model), option_depth_(d)
    {
        as.model = m;
    }
    Value(Union_data *u, uint8_t d) : tag_(Value_tag::Union), option_depth_(d)
    {
        as.un = u;
    }
    Value(Closure_data *c, uint8_t d) : tag_(Value_tag::Closure), option_depth_(d)
    {
        as.closure = c;
    }
    Value(Iterator_data *i, uint8_t d) : tag_(Value_tag::Iterator), option_depth_(d)
    {
        as.iter = i;
    }
    Value(Green_thread_data *g, uint8_t d) : tag_(Value_tag::Green_thread), option_depth_(d)
    {
        as.gt = g;
    }
    Value(Upvalue_data *u, uint8_t d) : tag_(Value_tag::Upvalue), option_depth_(d)
    {
        as.upvalue = u;
    }

public:
    // --- Arena Factories (The only way to create heap objects safely) ---
    static Value make_string(mem::Arena &arena, std::string_view text, uint8_t depth = 0);
    static Value make_array(mem::Arena &arena, uint32_t capacity, uint8_t depth = 0);
    static Value make_model(mem::Arena &arena, types::Model_type sig, uint32_t field_count, uint8_t depth = 0);
    static Value make_union(mem::Arena &arena, String_data *u_name, String_data *v_name, Value payload, uint8_t depth = 0);
    static Value
    make_closure_native(mem::Arena &arena, String_data *name, size_t arity, types::Function_type sig, Native_fn func, uint8_t depth = 0);

    // --- Fast Inline Constructors ---
    Value() = default;
    Value(std::nullptr_t)
    {}
    Value(bool b, uint8_t d = 0) : tag_(Value_tag::Bool), option_depth_(d)
    {
        as.boolean = b;
    }

    // Integers
    Value(int8_t v, uint8_t d = 0) : tag_(Value_tag::I8), option_depth_(d)
    {
        as.i8 = v;
    }
    Value(int16_t v, uint8_t d = 0) : tag_(Value_tag::I16), option_depth_(d)
    {
        as.i16 = v;
    }
    Value(int32_t v, uint8_t d = 0) : tag_(Value_tag::I32), option_depth_(d)
    {
        as.i32 = v;
    }
    Value(int64_t v, uint8_t d = 0) : tag_(Value_tag::I64), option_depth_(d)
    {
        as.i64 = v;
    }
    Value(uint8_t v, uint8_t d = 0) : tag_(Value_tag::U8), option_depth_(d)
    {
        as.u8 = v;
    }
    Value(uint16_t v, uint8_t d = 0) : tag_(Value_tag::U16), option_depth_(d)
    {
        as.u16 = v;
    }
    Value(uint32_t v, uint8_t d = 0) : tag_(Value_tag::U32), option_depth_(d)
    {
        as.u32 = v;
    }
    Value(uint64_t v, uint8_t d = 0) : tag_(Value_tag::U64), option_depth_(d)
    {
        as.u64 = v;
    }

    // Floats
    Value(numeric::float16_t v, uint8_t d = 0) : tag_(Value_tag::F16), option_depth_(d)
    {
        as.f16 = v;
    }
    Value(float v, uint8_t d = 0) : tag_(Value_tag::F32), option_depth_(d)
    {
        as.f32 = v;
    }
    Value(double v, uint8_t d = 0) : tag_(Value_tag::F64), option_depth_(d)
    {
        as.f64 = v;
    }

    // --- Type Queries ---
    Value_tag tag() const
    {
        return tag_;
    }
    uint8_t depth() const
    {
        return option_depth_;
    }

    bool is_nil() const
    {
        return tag_ == Value_tag::Nil || (option_depth_ > 0 && tag_ == Value_tag::Nil);
    }
    bool is_bool() const
    {
        return tag_ == Value_tag::Bool;
    }

    bool is_integer() const
    {
        return tag_ >= Value_tag::I8 && tag_ <= Value_tag::U64;
    }
    bool is_float() const
    {
        return tag_ >= Value_tag::F16 && tag_ <= Value_tag::F64;
    }
    bool is_number() const
    {
        return tag_ >= Value_tag::I8 && tag_ <= Value_tag::F64;
    }

    bool is_string() const
    {
        return tag_ == Value_tag::String;
    }
    bool is_array() const
    {
        return tag_ == Value_tag::Array;
    }
    bool is_model() const
    {
        return tag_ == Value_tag::Model;
    }
    bool is_union() const
    {
        return tag_ == Value_tag::Union;
    }
    bool is_closure() const
    {
        return tag_ == Value_tag::Closure;
    }
    bool is_iterator() const
    {
        return tag_ == Value_tag::Iterator;
    }
    bool is_green_thread() const
    {
        return tag_ == Value_tag::Green_thread;
    }

    // --- Elegant Safe Accessors ---
    std::string_view as_string() const
    {
        return std::string_view(as.str->chars, as.str->length);
    }

    std::span<Value> as_array_elements() const
    {
        return std::span<Value>(as.arr->elements, as.arr->count);
    }
    std::span<Value> as_model_fields() const
    {
        return std::span<Value>(as.model->fields, as.model->field_count);
    }

    void push_array_element(Value val)
    {
        as.arr->elements[as.arr->count++] = val;
    }

    bool as_bool() const
    {
        return as.boolean;
    }

    // Automatic Upcasting (e.g. i8 safely upcasts to i64 here)
    int64_t as_int() const;
    uint64_t as_uint() const;
    double as_float() const;

    // --- Safe Conversions ---
    std::optional<std::int64_t> try_as_i64() const;
    std::optional<std::uint64_t> try_as_u64() const;

    types::Primitive_kind numeric_type() const;
    std::optional<Value> cast_numeric(types::Primitive_kind target_type) const;
    std::optional<Value> coerce_literal(types::Primitive_kind target_type) const;

    // --- Utility ---
    std::string to_string() const;
    std::string to_debug_string() const;

    // --- Operators ---
    bool operator==(const Value &other) const;
    bool operator!=(const Value &other) const
    {
        return !(*this == other);
    }
};

static_assert(sizeof(Value) == 16, "Value struct must be exactly 16 bytes for L1 Cache alignment.");

} // namespace phos
