#pragma once

#include <expected>

#include "./err.hpp"

namespace phos {

template <typename T>
using Result = std::expected<T, err::msg>;

} // namespace phos
