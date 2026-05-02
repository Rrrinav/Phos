#pragma once

#include "./err.hpp"

#include <expected>

namespace phos {

template <typename T>
using Result = std::expected<T, err::msg>;

} // namespace phos
