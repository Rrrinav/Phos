#pragma once

#include <array>
#include <chrono>
#include <vector>

#include "../value/value.hpp"  // Includes Native_function_value, Result, etc.
#include "native_signatures.hpp"

namespace phos::native
{

// This constexpr value must be updated if you add or remove a native function.
inline constexpr int NUM_NATIVE_FUNCS = 2;

// Returns a fixed-size array of all native function implementations.
inline std::array<std::shared_ptr<Native_function_value>, NUM_NATIVE_FUNCS> get_natives()
{
    auto signatures = get_native_signatures();
    std::array<std::shared_ptr<Native_function_value>, NUM_NATIVE_FUNCS> functions;

    // === len ===
    {
        std::string name = "len";
        auto signature_data = signatures.at(name);

        auto func = std::make_shared<Native_function_value>();
        func->name = name;
        func->arity = signature_data.allowed_params.size();
        func->code = [](const std::vector<Value> &args) -> Result<Value>
        {
            const auto &subject = args[0];
            if (is_string(subject))
                return Value((int64_t)get_string(subject).length());
            if (is_array(subject))
                return Value((int64_t)get_array(subject)->elements.size());
            return std::unexpected(err::msg("len() expects a string or an array.", "interpreter", 0, 0));
        };

        functions[0] = func;
    }

    // === clock ===
    {
        std::string name = "clock";

        auto func = std::make_shared<Native_function_value>();
        func->name = name;
        func->arity = 0;
        func->code = [](const std::vector<Value> &args) -> Result<Value>
        {
            auto time = std::chrono::system_clock::now().time_since_epoch();
            double millis = std::chrono::duration<double, std::milli>(time).count();
            return Value(millis);
        };

        functions[1] = func;
    }

    return functions;
}

}  // namespace phos::native
