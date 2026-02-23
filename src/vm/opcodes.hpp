#pragma once

#include <cstdint>
#include <string>

namespace phos::vm {

enum class Op_code : uint8_t {
    // --- Constants & Basics ---
    Constant, Nil, True, False, Pop,

    // --- Locals & Globals ---
    Get_local, Set_local, Get_global, Set_global, Define_global,

    // --- Upvalues (Closure Captures) ---
    Get_upvalue, Set_upvalue,

    // --- Control Flow ---
    Jump, Jump_if_false, Loop,

    // --- Optionals ---
    Jump_if_nil, Unwrap,

    // --- Arithmetic & Logic ---
    Add, Subtract, Multiply, Divide, Modulo, Not, Negate, Equal, Not_equal,
    Greater, Greater_equal, Less, Less_equal,

    // --- Arrays & Models ---
    Create_array, Get_index, Set_index,
    Get_field, Set_field, Unpack,

    // --- Unions ---
    Construct_union,
    Match_variant,

    // --- Functions & Closures ---
    Call, Tail_call, Return, Make_closure,

    // --- Green Threads ---
    Spawn, Yield, Await,

    // --- System ---
    Print, Halt
};

std::string op_code_to_string(Op_code code);

} // namespace phos::vm
