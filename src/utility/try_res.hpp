#pragma once
#include <concepts>

/**
 * @brief Concept to detect Result-like types (has value(), error(), operator bool())
 */
template <typename T>
concept ResultLike = requires(T t) {
    { t.has_value() } -> std::convertible_to<bool>;
    { t.error() };
    { t.value() };
};

/**
 * @brief Macro to propagate errors for phos::Result or std::expected.
 * Returns error if result is invalid; otherwise unwraps value.
 */
#define __Try(expr) \
({ \
    auto&& result = (expr); \
    static_assert(ResultLike<decltype(result)>, "__try expects a Result-like type"); \
    if (!result) return std::unexpected(result.error()); \
    std::move(result.value()); \
})


#define __TryOr(expr, val) \
({ \
    auto&& result = (expr); \
    static_assert(ResultLike<decltype(result)>, "__try expects a Result-like type"); \
    if (!result) return std::unexpected(result.error()); \
    std::move(result.value_or(val)); \
})

/**
 * @brief Macro for functions returning phos::Result<void>.
 * Checks for error and returns it if present.
 */
#define __TryVoid(expr) \
do { \
    auto&& result = (expr); \
    static_assert(ResultLike<decltype(result)>, "__try_void expects a Result-like type"); \
    if (!result) return std::unexpected(result.error()); \
} while(0)

/**
 * @brief Macro to ignore any expression or variable.
 */
#define __Ignore(...) (void)(::std::ignore = (__VA_ARGS__))

/**
 * @brief Propagates errors for Result-like types, ignores value if success.
 */
#define __TryIgnore(expr)                             \
do {                                                   \
    auto&& __tmp = (expr);                             \
    if constexpr (ResultLike<decltype(__tmp)>) {       \
        if (!__tmp) return std::unexpected(__tmp.error()); \
        (void)0; /* ignore value */                   \
    } else {                                           \
        (void)(__tmp); /* fallback: ignore anything else */ \
    }                                                  \
} while(0)
