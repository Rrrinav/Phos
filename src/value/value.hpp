#pragma once

#include <variant>
#include <cstdint>
#include <string>

#include "type.hpp"

namespace phos {
    using Value = std::variant<
                               int64_t,
                               double,
                               bool,
                               std::string,
                               std::nullptr_t,
                               std::monostate
                               >;
    struct Return_value
    {
        Value value;
        explicit Return_value(Value v) : value(std::move(v)) {}
    };

    struct Variable
    {
        types::Type type;
        Value value;
        Variable(types::Type t, Value v) : type(std::move(t)), value(std::move(v)) {}
    };
} // namespace phos
