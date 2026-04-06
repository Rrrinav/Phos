#pragma once

#include <string>
#include <unordered_map>
#include <iostream>
#include <optional>
#include <vector>
#include <type_traits>

#include "../value/value.hpp"
#include "../error/result.hpp"
#include "../parser/ast.hpp"
#include "../type-checker/type-checker.hpp"

namespace phos::vm
{

struct Call_frame
{
    mem::rc_ptr<Closure_value> closure;
    uint8_t *ip = nullptr;
    size_t stack_offset = 0;
};

struct Vm_config
{
    std::ostream *out_stream = &std::cout;
    std::ostream *err_stream = &std::cerr;
};

class Virtual_machine
{
public:
    std::string err{};

    Virtual_machine(Vm_config config = {}) : config_(config) {}

    Result<void> interpret(mem::rc_ptr<Closure_value> script_closure);

    void set_global(const std::string &name, Value val) { globals[name] = std::move(val); }
    std::optional<Value> get_global(const std::string &name) const
    {
        if (auto it = globals.find(name); it != globals.end())
            return it->second;
        return std::nullopt;
    }

    Value pop();
    Value peek(int distance) const;

    template <auto Func>
    void bind_native(const std::string &name, const std::vector<std::string> &params, const std::string &ret, phos::Type_checker &tc);
    template <auto Func>
    void bind_native_sig(const std::string &name, const std::vector<phos::Type_checker::Native_param> &params, const std::string &ret, phos::Type_checker &tc);

    Vm_config config_;

    mem::rc_ptr<Green_thread_value> current_thread;

    std::unordered_map<std::string, Value> globals;

    Result<void> run();

    void push(Value value);

    // Call Frame Managers
    Result<void> call_value(Value callee, int arg_count, Call_frame *&current_frame, uint8_t *&current_ip);
    Result<void> call_closure(mem::rc_ptr<Closure_value> closure, int arg_count, Call_frame *&current_frame, uint8_t *&current_ip);

    ast::Source_location get_loc(Call_frame *frame, uint8_t *ip);

    template <typename Op>
    Result<void> execute_binary_op(Op &&op, Call_frame *frame, uint8_t *ip);

    template <typename Op>
    Result<void> execute_unary_op(Op &&op, Call_frame *frame, uint8_t *ip);

    mem::rc_ptr<Upvalue_value> capture_upvalue(size_t stack_index);
    void close_upvalues(size_t last_stack_index);
};

// ============================================================================
// ZERO-OVERHEAD FFI ENGINE
// ============================================================================
namespace ffi
{
// Stack Extraction Traits (For the VM Execution)
template <typename T>
struct extract;
template <typename T>
    requires phos::is_numeric_cpp_v<T>
struct extract<T>
{
    static T get(const Value &v)
    {
        auto casted = phos::cast_numeric_value(v, phos::primitive_kind_for_cpp_numeric_v<T>);
        if (!casted)
            throw std::bad_variant_access();
        return std::get<T>(casted.value());
    }
};
template <>
struct extract<std::string>
{
    static std::string get(const Value &v) { return phos::get_string(v); }
};
template <>
struct extract<bool>
{
    static bool get(const Value &v) { return phos::get_bool(v); }
};
template <>
struct extract<mem::rc_ptr<Array_value>>
{
    static mem::rc_ptr<Array_value> get(const Value &v) { return phos::get_array(v); }
};
template <>
struct extract<mem::rc_ptr<Model_value>>
{
    static mem::rc_ptr<Model_value> get(const Value &v) { return phos::get_model(v); }
};
template <>
struct extract<mem::rc_ptr<Iterator_value>>
{
    static mem::rc_ptr<Iterator_value> get(const Value &v) { return phos::get_iterator(v); }
};
template <>
struct extract<Value>
{
    static Value get(const Value &v) { return v; }
};

// The Compile-Time Thunk Generator
template <auto Func>
struct Thunk;

template <typename R, typename... Args, R (*Func)(Args...)>
struct Thunk<Func>
{
    static constexpr uint8_t arity = sizeof...(Args);

    static Value call(Virtual_machine *vm, uint8_t arg_count) { return call_impl(vm, std::make_index_sequence<arity>{}); }

private:
    template <size_t... Is>
    static Value call_impl(Virtual_machine *vm, std::index_sequence<Is...>)
    {
        if constexpr (std::is_void_v<R>)
        {
            Func(extract<std::decay_t<Args>>::get(vm->peek(arity - 1 - Is))...);
            return Value(nullptr);
        }
        else
        {
            return Value(Func(extract<std::decay_t<Args>>::get(vm->peek(arity - 1 - Is))...));
        }
    }
};
}  // namespace ffi

template <auto Func>
inline void Virtual_machine::bind_native(const std::string &name,
                                         const std::vector<std::string> &params,
                                         const std::string &ret,
                                         phos::Type_checker &tc)
{
    // 1. Tell the Type Checker about the strict String Signatures!
    tc.define_native(name, params, ret);

    // 2. Tell the VM to run the C++ function!
    // We only need to define the runtime closure once, even if there are overloads.
    if (!get_global(name))
    {
        types::Function_type dummy_sig;  // The VM doesn't care about static types at runtime!
        auto native_closure = mem::make_rc<Closure_value>(name, ffi::Thunk<Func>::arity, dummy_sig, ffi::Thunk<Func>::call);
        set_global(name, Value(native_closure));
    }
}

template <auto Func>
inline void Virtual_machine::bind_native_sig(const std::string &name,
                                             const std::vector<phos::Type_checker::Native_param> &params,
                                             const std::string &ret,
                                             phos::Type_checker &tc)
{
    tc.define_native(name, params, ret);

    if (!get_global(name))
    {
        types::Function_type dummy_sig;
        auto native_closure = mem::make_rc<Closure_value>(name, ffi::Thunk<Func>::arity, dummy_sig, ffi::Thunk<Func>::call);
        set_global(name, Value(native_closure));
    }
}

template <typename Op>
Result<void> Virtual_machine::execute_binary_op(Op &&op, Call_frame *frame, uint8_t *ip)
{
    Value r = pop();
    Value l = pop();
    auto res = op(l, r, get_loc(frame, ip));
    if (!res)
        return std::unexpected(res.error());
    push(res.value());
    return {};
}

template <typename Op>
Result<void> Virtual_machine::execute_unary_op(Op &&op, Call_frame *frame, uint8_t *ip)
{
    Value v = pop();
    auto res = op(v, get_loc(frame, ip));
    if (!res)
        return std::unexpected(res.error());
    push(res.value());
    return {};
}

}  // namespace phos::vm
