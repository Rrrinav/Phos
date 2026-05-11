#include "./value.hpp"

#include "core/arena.hpp"
#include "virtual_machine/vm_context.hpp"

#include <charconv>
#include <cstdlib>
#include <cstring>
#include <format>
#include <limits>
#include <memory>
#include <type_traits>

namespace phos {

namespace {

template <typename T, typename Element>
Element *trailing_storage(T *base)
{
    auto *raw = reinterpret_cast<char *>(base) + sizeof(T);
    return reinterpret_cast<Element *>(raw);
}

} // namespace

template <typename Allocator>
Value Value::make_string(Allocator &alloc, std::string_view text, uint8_t depth)
{
    size_t total_size = sizeof(String_data) + text.length() + 1;
    String_data *str = nullptr;
    bool is_gc = std::is_same_v<std::decay_t<Allocator>, vm::Vm_context>;

    if constexpr (std::is_same_v<std::decay_t<Allocator>, vm::Vm_context>) {
        str = alloc.template alloc<String_data>(total_size, static_cast<uint8_t>(Value_tag::String));
        new (str) String_data();
    } else {
        void *raw_mem = alloc.allocate_bytes(total_size, alignof(String_data));
        str = new (raw_mem) String_data();
    }

    str->length = static_cast<uint32_t>(text.length());
    std::memcpy(str->chars, text.data(), text.length());
    str->chars[text.length()] = '\0';

    return Value(str, depth, is_gc);
}

template <typename Allocator>
Value Value::make_array(Allocator &alloc, uint32_t capacity, uint8_t depth)
{
    Array_data *arr = nullptr;
    bool is_gc = std::is_same_v<std::decay_t<Allocator>, vm::Vm_context>;
    const size_t total_size = sizeof(Array_data) + (static_cast<size_t>(capacity) * sizeof(Value));

    if constexpr (std::is_same_v<std::decay_t<Allocator>, vm::Vm_context>) {
        arr = alloc.template alloc<Array_data>(total_size, static_cast<uint8_t>(Value_tag::Array));
        new (arr) Array_data();
        arr->capacity = capacity;
        arr->count = 0;
        arr->elements_on_heap = false; // Trailing natively!

        if (capacity > 0) {
            arr->elements = trailing_storage<Array_data, Value>(arr);
            std::uninitialized_fill_n(arr->elements, capacity, Value());
        } else {
            arr->elements = nullptr;
        }
    } else {
        // Fallback for Arena (can't easily do trailing without manual memory blocks)
        arr = alloc.template allocate<Array_data>();
        new (arr) Array_data();
        arr->capacity = capacity;
        arr->count = 0;
        arr->elements_on_heap = true;

        if (capacity > 0) {
            arr->elements = alloc.template allocate<Value>(capacity);
            std::uninitialized_fill_n(arr->elements, capacity, Value());
        } else {
            arr->elements = nullptr;
        }
    }

    return Value(arr, depth, is_gc);
}

template <typename Allocator>
Value Value::make_model(Allocator &alloc, types::Model_type sig, uint32_t field_count, uint8_t depth)
{
    Model_data *model = nullptr;
    bool is_gc = std::is_same_v<std::decay_t<Allocator>, vm::Vm_context>;
    const size_t total_size = sizeof(Model_data) + (static_cast<size_t>(field_count) * sizeof(Value));

    if constexpr (std::is_same_v<std::decay_t<Allocator>, vm::Vm_context>) {
        model = alloc.template alloc<Model_data>(total_size, static_cast<uint8_t>(Value_tag::Model));
        new (model) Model_data();
        model->signature = std::move(sig);
        model->field_count = field_count;
        model->fields_on_heap = false; // Trailing!

        if (field_count > 0) {
            model->fields = trailing_storage<Model_data, Value>(model);
            std::uninitialized_fill_n(model->fields, field_count, Value());
        } else {
            model->fields = nullptr;
        }
    } else {
        model = alloc.template allocate<Model_data>();
        new (model) Model_data();
        model->signature = std::move(sig);
        model->field_count = field_count;
        model->fields_on_heap = true;

        if (field_count > 0) {
            model->fields = alloc.template allocate<Value>(field_count);
            std::uninitialized_fill_n(model->fields, field_count, Value());
        } else {
            model->fields = nullptr;
        }
    }

    return Value(model, depth, is_gc);
}

template <typename Allocator>
Value Value::make_union(Allocator &alloc, String_data *u_name, String_data *v_name, Value payload, uint8_t depth)
{
    Union_data *un = nullptr;
    bool is_gc = std::is_same_v<std::decay_t<Allocator>, vm::Vm_context>;

    if constexpr (std::is_same_v<std::decay_t<Allocator>, vm::Vm_context>) {
        const size_t total_size = sizeof(Union_data) + sizeof(Value);
        un = alloc.template alloc<Union_data>(total_size, static_cast<uint8_t>(Value_tag::Union));
        new (un) Union_data();
        un->union_name = u_name;
        un->variant_name = v_name;

        un->payload = trailing_storage<Union_data, Value>(un);
        *(un->payload) = payload;
    } else {
        un = alloc.template allocate<Union_data>();
        new (un) Union_data();
        un->union_name = u_name;
        un->variant_name = v_name;

        un->payload = alloc.template allocate<Value>();
        *(un->payload) = payload;
    }

    return Value(un, depth, is_gc);
}

template <typename Allocator>
Value Value::make_closure_native(Allocator &alloc, String_data *name, size_t arity, types::Function_type sig, Native_fn func, uint8_t depth)
{
    Closure_data *closure = nullptr;
    bool is_gc = std::is_same_v<std::decay_t<Allocator>, vm::Vm_context>;

    if constexpr (std::is_same_v<std::decay_t<Allocator>, vm::Vm_context>) {
        closure = alloc.template alloc<Closure_data>(sizeof(Closure_data), static_cast<uint8_t>(Value_tag::Closure));
        new (closure) Closure_data();
        closure->is_prototype = false;
    } else {
        closure = alloc.template allocate<Closure_data>();
        new (closure) Closure_data();
        closure->is_prototype = true;
    }

    closure->name = name;
    closure->arity = arity;
    closure->signature = std::move(sig);
    closure->native_func = func;
    closure->upvalue_count = 0;
    closure->upvalues = nullptr;

    return Value(closure, depth, is_gc);
}

template <typename Allocator>
Value Value::make_iterator(Allocator &alloc, uint8_t depth)
{
    Iterator_data *iter = nullptr;
    bool is_gc = std::is_same_v<std::decay_t<Allocator>, vm::Vm_context>;

    if constexpr (std::is_same_v<std::decay_t<Allocator>, vm::Vm_context>) {
        iter = alloc.template alloc<Iterator_data>(sizeof(Iterator_data), static_cast<uint8_t>(Value_tag::Iterator));
        new (iter) Iterator_data();
    } else {
        iter = alloc.template allocate<Iterator_data>();
        new (iter) Iterator_data();
    }

    return Value(iter, depth, is_gc);
}

// -----------------------------------------------------------------------------
// EXPLICIT TEMPLATE INSTANTIATIONS
// -----------------------------------------------------------------------------

// Arena Instantiations (Compile-time)
template Value Value::make_string(mem::Arena &, std::string_view, uint8_t);
template Value Value::make_array(mem::Arena &, uint32_t, uint8_t);
template Value Value::make_model(mem::Arena &, types::Model_type, uint32_t, uint8_t);
template Value Value::make_union(mem::Arena &, String_data *, String_data *, Value, uint8_t);
template Value Value::make_closure_native(mem::Arena &, String_data *, size_t, types::Function_type, Native_fn, uint8_t);
template Value Value::make_iterator(mem::Arena &, uint8_t);

// VM Context Instantiations (Runtime) - THESE FIX THE LINKER ERRORS!
template Value Value::make_string(vm::Vm_context &, std::string_view, uint8_t);
template Value Value::make_array(vm::Vm_context &, uint32_t, uint8_t);
template Value Value::make_model(vm::Vm_context &, types::Model_type, uint32_t, uint8_t);
template Value Value::make_union(vm::Vm_context &, String_data *, String_data *, Value, uint8_t);
template Value Value::make_closure_native(vm::Vm_context &, String_data *, size_t, types::Function_type, Native_fn, uint8_t);
template Value Value::make_iterator(vm::Vm_context &, uint8_t);

// Numeric Getters with Implicit Upcasting
int64_t Value::as_int() const
{
    switch (tag_) {
    case Value_tag::I8:
        return as.i8;
    case Value_tag::I16:
        return as.i16;
    case Value_tag::I32:
        return as.i32;
    case Value_tag::I64:
        return as.i64;
    case Value_tag::U8:
        return static_cast<int64_t>(as.u8);
    case Value_tag::U16:
        return static_cast<int64_t>(as.u16);
    case Value_tag::U32:
        return static_cast<int64_t>(as.u32);
    case Value_tag::U64: {
        if (as.u64 > static_cast<uint64_t>(std::numeric_limits<int64_t>::max())) {
            throw std::overflow_error("Phos VM Panic: Unsigned 64-bit integer overflowed when cast to Signed 64-bit.");
        }
        return static_cast<int64_t>(as.u64);
    }
    default:
        return 0;
    }
}

uint64_t Value::as_uint() const
{
    switch (tag_) {
    case Value_tag::I8:
        return static_cast<uint64_t>(as.i8);
    case Value_tag::I16:
        return static_cast<uint64_t>(as.i16);
    case Value_tag::I32:
        return static_cast<uint64_t>(as.i32);
    case Value_tag::I64:
        return static_cast<uint64_t>(as.i64);
    case Value_tag::U8:
        return as.u8;
    case Value_tag::U16:
        return as.u16;
    case Value_tag::U32:
        return as.u32;
    case Value_tag::U64:
        return as.u64;
    default:
        return 0;
    }
}

double Value::as_float() const
{
    switch (tag_) {
    case Value_tag::F16:
        return static_cast<double>(as.f16);
    case Value_tag::F32:
        return static_cast<double>(as.f32);
    case Value_tag::F64:
        return as.f64;
    default:
        if (is_integer()) {
            return static_cast<double>(as_int());
        }
        return 0.0;
    }
}

// Safe Conversions & Coercions
std::optional<std::int64_t> Value::try_as_i64() const
{
    if (tag_ >= Value_tag::I8 && tag_ <= Value_tag::I64) {
        return as_int();
    }
    if (tag_ >= Value_tag::U8 && tag_ <= Value_tag::U64) {
        if (as_uint() <= static_cast<std::uint64_t>(std::numeric_limits<std::int64_t>::max())) {
            return static_cast<std::int64_t>(as_uint());
        }
    }
    return std::nullopt;
}

std::optional<std::uint64_t> Value::try_as_u64() const
{
    if (tag_ >= Value_tag::U8 && tag_ <= Value_tag::U64) {
        return as_uint();
    }
    if (tag_ >= Value_tag::I8 && tag_ <= Value_tag::I64) {
        if (as_int() >= 0) {
            return static_cast<std::uint64_t>(as_int());
        }
    }
    return std::nullopt;
}

types::Primitive_kind Value::numeric_type() const
{
    switch (tag_) {
    case Value_tag::I8:
        return types::Primitive_kind::I8;
    case Value_tag::I16:
        return types::Primitive_kind::I16;
    case Value_tag::I32:
        return types::Primitive_kind::I32;
    case Value_tag::I64:
        return types::Primitive_kind::I64;
    case Value_tag::U8:
        return types::Primitive_kind::U8;
    case Value_tag::U16:
        return types::Primitive_kind::U16;
    case Value_tag::U32:
        return types::Primitive_kind::U32;
    case Value_tag::U64:
        return types::Primitive_kind::U64;
    case Value_tag::F16:
        return types::Primitive_kind::F16;
    case Value_tag::F32:
        return types::Primitive_kind::F32;
    case Value_tag::F64:
        return types::Primitive_kind::F64;
    default:
        return types::Primitive_kind::Any;
    }
}

std::optional<Value> Value::cast_numeric(types::Primitive_kind target_type) const
{
    if ((!is_number() && !is_bool()) || !types::is_numeric_primitive(target_type)) {
        return std::nullopt;
    }

    if (types::is_float_primitive(target_type)) {
        double val = is_bool() ? (as_bool() ? 1.0 : 0.0) : as_float();
        if (target_type == types::Primitive_kind::F32) {
            return Value(static_cast<float>(val));
        }
        if (target_type == types::Primitive_kind::F16) {
            return Value(static_cast<numeric::float16_t>(val));
        }
        return Value(val);
    }

    if (types::is_unsigned_integer_primitive(target_type)) {
        uint64_t val = is_bool() ? static_cast<uint64_t>(as_bool()) : ((is_float() && as_float() < 0) ? 0 : as_uint());
        if (target_type == types::Primitive_kind::U8) {
            return Value(static_cast<uint8_t>(val));
        }
        if (target_type == types::Primitive_kind::U16) {
            return Value(static_cast<uint16_t>(val));
        }
        if (target_type == types::Primitive_kind::U32) {
            return Value(static_cast<uint32_t>(val));
        }
        return Value(val);
    }

    int64_t val = is_bool() ? static_cast<int64_t>(as_bool()) : as_int();
    if (target_type == types::Primitive_kind::I8) {
        return Value(static_cast<int8_t>(val));
    }
    if (target_type == types::Primitive_kind::I16) {
        return Value(static_cast<int16_t>(val));
    }
    if (target_type == types::Primitive_kind::I32) {
        return Value(static_cast<int32_t>(val));
    }
    return Value(val);
}

std::optional<Value> Value::coerce_literal(types::Primitive_kind target_type) const
{
    if (!is_number() || !types::is_numeric_primitive(target_type)) {
        return std::nullopt;
    }

    if (types::is_float_primitive(target_type)) {
        return cast_numeric(target_type);
    }

    if (is_integer()) {
        int64_t v = as_int();
        if (target_type == types::Primitive_kind::I8 && v >= -128 && v <= 127) {
            return Value(static_cast<int8_t>(v));
        }
        if (target_type == types::Primitive_kind::I16 && v >= -32768 && v <= 32767) {
            return Value(static_cast<int16_t>(v));
        }
        if (target_type == types::Primitive_kind::I32 && v >= -2147483648LL && v <= 2147483647LL) {
            return Value(static_cast<int32_t>(v));
        }
        if (target_type == types::Primitive_kind::I64) {
            return Value(v);
        }

        if (types::is_unsigned_integer_primitive(target_type) && v >= 0) {
            uint64_t u = static_cast<uint64_t>(v);
            if (target_type == types::Primitive_kind::U8 && u <= 255) {
                return Value(static_cast<uint8_t>(u));
            }
            if (target_type == types::Primitive_kind::U16 && u <= 65535) {
                return Value(static_cast<uint16_t>(u));
            }
            if (target_type == types::Primitive_kind::U32 && u <= 4294967295ULL) {
                return Value(static_cast<uint32_t>(u));
            }
            if (target_type == types::Primitive_kind::U64) {
                return Value(u);
            }
        }
    }
    return std::nullopt;
}

std::string Value::to_string() const
{
    if (is_nil()) {
        return "nil";
    }
    if (tag_ == Value_tag::Bool) {
        return as.boolean ? "true" : "false";
    }

    if (is_integer()) {
        char buffer[64];
        auto res = std::to_chars(buffer, buffer + sizeof(buffer), as_int());

        if (res.ec == std::errc()) {
            return std::string(buffer, res.ptr);
        }
        return "<conversion_error>";
    }

    if (is_float()) {
        return std::format("{}", as_float());
    }

    switch (tag_) {
    case Value_tag::String:
        return std::string(as_string());
    case Value_tag::Array:
        return std::format("<array len {}>", as.arr->count);
    case Value_tag::Model:
        return std::format("<model {}>", as.model->signature.name);
    case Value_tag::Union:
        return std::format(
            "<{}::{}>",
            std::string_view(as.un->union_name->chars, as.un->union_name->length),
            std::string_view(as.un->variant_name->chars, as.un->variant_name->length));
    case Value_tag::Closure:
        return "<closure>";
    case Value_tag::Iterator:
        return "<iterator>";
    case Value_tag::Green_thread:
        return "<green thread>";
    default:
        break;
    }

    return "<unknown>";
}

std::string Value::to_debug_string(bool is_nested) const
{
    std::string res;

    if (is_nil()) {
        res = "nil";
    } else if (tag_ == Value_tag::Bool) {
        res = as.boolean ? "true" : "false";
    } else if (is_integer()) {
        res = std::to_string(as_int());
    } else if (is_float()) {
        res = std::format("{}", as_float());
    } else if (tag_ == Value_tag::String) {
        res = is_nested ? std::format("\"{}\"", as_string()) : std::string(as_string());
    } else if (tag_ == Value_tag::Array) {
        res = "[";
        auto elements = as_array_elements();
        for (size_t i = 0; i < elements.size(); ++i) {
            res += elements[i].to_debug_string(true);
            if (i != elements.size() - 1) {
                res += ", ";
            }
        }
        res += "]";
    } else if (tag_ == Value_tag::Model) {
        std::string m_name = as.model->signature.name;

        if (m_name.empty()) {
            res = "{";
        } else {
            res = std::format("{} {{", m_name);
        }

        for (uint32_t i = 0; i < as.model->field_count; ++i) {
            if (i < as.model->signature.fields.size()) {
                std::string f_name = as.model->signature.fields[i].first;
                if (!f_name.empty()) {
                    res += f_name + ": ";
                }
            }

            res += as.model->fields[i].to_debug_string(true);

            if (i != as.model->field_count - 1) {
                res += ", ";
            }
        }
        res += "}";
    } else if (tag_ == Value_tag::Union) {
        std::string_view u_name(as.un->union_name->chars, as.un->union_name->length);
        std::string_view v_name(as.un->variant_name->chars, as.un->variant_name->length);

        res = std::format("{}::{}", u_name, v_name);

        if (as.un->payload && !as.un->payload->is_nil()) {
            res += std::format("({})", as.un->payload->to_debug_string(true));
        }
    } else {
        res = to_string();
    }

    uint8_t visual_depth = option_depth_;
    if (is_nil() && visual_depth > 0) {
        visual_depth -= 1;
    }

    for (uint8_t i = 0; i < visual_depth; ++i) {
        res += "?";
    }

    return res;
}

bool Value::operator==(const Value &other) const
{
    if (option_depth_ != other.option_depth_) {
        return false;
    }
    if (tag_ != other.tag_) {
        return false;
    }

    switch (tag_) {
    case Value_tag::Nil:
        return true;
    case Value_tag::Bool:
        return as.boolean == other.as.boolean;

    case Value_tag::I8:
        return as.i8 == other.as.i8;
    case Value_tag::I16:
        return as.i16 == other.as.i16;
    case Value_tag::I32:
        return as.i32 == other.as.i32;
    case Value_tag::I64:
        return as.i64 == other.as.i64;
    case Value_tag::U8:
        return as.u8 == other.as.u8;
    case Value_tag::U16:
        return as.u16 == other.as.u16;
    case Value_tag::U32:
        return as.u32 == other.as.u32;
    case Value_tag::U64:
        return as.u64 == other.as.u64;
    case Value_tag::F16:
        return as.f16 == other.as.f16;
    case Value_tag::F32:
        return as.f32 == other.as.f32;
    case Value_tag::F64:
        return as.f64 == other.as.f64;

    case Value_tag::String:
        return as_string() == other.as_string();

    case Value_tag::Array:
        return as.arr == other.as.arr;
    case Value_tag::Model:
        return as.model == other.as.model;
    case Value_tag::Union:
        return as.un == other.as.un;
    case Value_tag::Closure:
        return as.closure == other.as.closure;
    case Value_tag::Iterator:
        return as.iter == other.as.iter;
    case Value_tag::Green_thread:
        return as.gt == other.as.gt;
    case Value_tag::Upvalue:
        return as.upvalue == other.as.upvalue;
    }
    return false;
}

types::Primitive_kind numeric_type_of(const Value &literal)
{
    if (literal.is_float()) {
        return types::Primitive_kind::F64;
    }
    return types::Primitive_kind::I32;
}

std::optional<Value> coerce_numeric_literal(const Value &literal, types::Primitive_kind target_type)
{
    if (literal.is_float()) {
        switch (target_type) {
        case types::Primitive_kind::F16:
        case types::Primitive_kind::F32:
        case types::Primitive_kind::F64:
            return literal.cast_numeric(target_type);
        default:
            return std::nullopt;
        }
    }

    if (literal.is_integer()) {
        int64_t raw_val = literal.as_int();

        switch (target_type) {
        case types::Primitive_kind::I8:
            if (raw_val >= std::numeric_limits<int8_t>::min() && raw_val <= std::numeric_limits<int8_t>::max()) {
                return Value(static_cast<int8_t>(raw_val));
            }
            break;
        case types::Primitive_kind::I16:
            if (raw_val >= std::numeric_limits<int16_t>::min() && raw_val <= std::numeric_limits<int16_t>::max()) {
                return Value(static_cast<int16_t>(raw_val));
            }
            break;
        case types::Primitive_kind::I32:
            if (raw_val >= std::numeric_limits<int32_t>::min() && raw_val <= std::numeric_limits<int32_t>::max()) {
                return Value(static_cast<int32_t>(raw_val));
            }
            break;
        case types::Primitive_kind::I64:
            return Value(static_cast<int64_t>(raw_val));

        case types::Primitive_kind::U8:
            if (raw_val >= 0 && raw_val <= std::numeric_limits<uint8_t>::max()) {
                return Value(static_cast<uint8_t>(raw_val));
            }
            break;
        case types::Primitive_kind::U16:
            if (raw_val >= 0 && raw_val <= std::numeric_limits<uint16_t>::max()) {
                return Value(static_cast<uint16_t>(raw_val));
            }
            break;
        case types::Primitive_kind::U32:
            if (raw_val >= 0 && raw_val <= std::numeric_limits<uint32_t>::max()) {
                return Value(static_cast<uint32_t>(raw_val));
            }
            break;
        case types::Primitive_kind::U64:
            if (raw_val >= 0) {
                return Value(static_cast<uint64_t>(raw_val));
            }
            break;

        case types::Primitive_kind::F16:
        case types::Primitive_kind::F32:
        case types::Primitive_kind::F64:
            return std::nullopt;

        default:
            return std::nullopt;
        }
    }

    return std::nullopt;
}

} // namespace phos
