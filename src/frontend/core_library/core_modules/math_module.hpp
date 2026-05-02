#pragma once

#include "core/value/value.hpp"
#include "frontend/environment/type_environment.hpp"

#include <algorithm>
#include <cmath>
#include <limits>
#include <numeric>
#include <span>

namespace phos::vm::modules {

struct math_native
{
    // --- Trigonometry ---
    static Value sin(mem::Arena &, std::span<Value> args)
    {
        return Value(std::sin(args[0].as_float()));
    }
    static Value cos(mem::Arena &, std::span<Value> args)
    {
        return Value(std::cos(args[0].as_float()));
    }
    static Value tan(mem::Arena &, std::span<Value> args)
    {
        return Value(std::tan(args[0].as_float()));
    }
    static Value asin(mem::Arena &, std::span<Value> args)
    {
        return Value(std::asin(args[0].as_float()));
    }
    static Value acos(mem::Arena &, std::span<Value> args)
    {
        return Value(std::acos(args[0].as_float()));
    }
    static Value atan(mem::Arena &, std::span<Value> args)
    {
        return Value(std::atan(args[0].as_float()));
    }
    static Value atan2(mem::Arena &, std::span<Value> args)
    {
        return Value(std::atan2(args[0].as_float(), args[1].as_float()));
    }

    // --- Hyperbolic ---
    static Value sinh(mem::Arena &, std::span<Value> args)
    {
        return Value(std::sinh(args[0].as_float()));
    }
    static Value cosh(mem::Arena &, std::span<Value> args)
    {
        return Value(std::cosh(args[0].as_float()));
    }
    static Value tanh(mem::Arena &, std::span<Value> args)
    {
        return Value(std::tanh(args[0].as_float()));
    }
    static Value asinh(mem::Arena &, std::span<Value> args)
    {
        return Value(std::asinh(args[0].as_float()));
    }
    static Value acosh(mem::Arena &, std::span<Value> args)
    {
        return Value(std::acosh(args[0].as_float()));
    }
    static Value atanh(mem::Arena &, std::span<Value> args)
    {
        return Value(std::atanh(args[0].as_float()));
    }

    // --- Exponents & Roots ---
    static Value pow(mem::Arena &, std::span<Value> args)
    {
        return Value(std::pow(args[0].as_float(), args[1].as_float()));
    }
    static Value sqrt(mem::Arena &, std::span<Value> args)
    {
        return Value(std::sqrt(args[0].as_float()));
    }
    static Value cbrt(mem::Arena &, std::span<Value> args)
    {
        return Value(std::cbrt(args[0].as_float()));
    }
    static Value exp(mem::Arena &, std::span<Value> args)
    {
        return Value(std::exp(args[0].as_float()));
    }
    static Value exp2(mem::Arena &, std::span<Value> args)
    {
        return Value(std::exp2(args[0].as_float()));
    }
    static Value expm1(mem::Arena &, std::span<Value> args)
    {
        return Value(std::expm1(args[0].as_float()));
    }
    static Value log(mem::Arena &, std::span<Value> args)
    {
        return Value(std::log(args[0].as_float()));
    }
    static Value log10(mem::Arena &, std::span<Value> args)
    {
        return Value(std::log10(args[0].as_float()));
    }
    static Value log2(mem::Arena &, std::span<Value> args)
    {
        return Value(std::log2(args[0].as_float()));
    }
    static Value log1p(mem::Arena &, std::span<Value> args)
    {
        return Value(std::log1p(args[0].as_float()));
    }
    static Value hypot(mem::Arena &, std::span<Value> args)
    {
        return Value(std::hypot(args[0].as_float(), args[1].as_float()));
    }
    static Value rsqrt(mem::Arena &, std::span<Value> args)
    {
        return Value(1.0 / std::sqrt(args[0].as_float()));
    }
    static Value fma(mem::Arena &, std::span<Value> args)
    {
        return Value(std::fma(args[0].as_float(), args[1].as_float(), args[2].as_float()));
    }

    // --- Rounding & Utilities ---
    static Value ceil(mem::Arena &, std::span<Value> args)
    {
        return Value(std::ceil(args[0].as_float()));
    }
    static Value floor(mem::Arena &, std::span<Value> args)
    {
        return Value(std::floor(args[0].as_float()));
    }
    static Value round(mem::Arena &, std::span<Value> args)
    {
        return Value(std::round(args[0].as_float()));
    }
    static Value trunc(mem::Arena &, std::span<Value> args)
    {
        return Value(std::trunc(args[0].as_float()));
    }
    static Value abs(mem::Arena &, std::span<Value> args)
    {
        return Value(std::abs(args[0].as_float()));
    }
    static Value fract(mem::Arena &, std::span<Value> args)
    {
        double v = args[0].as_float();
        return Value(v - std::trunc(v));
    }
    static Value sign(mem::Arena &, std::span<Value> args)
    {
        double v = args[0].as_float();
        if (v > 0.0) {
            return Value(1.0);
        }
        if (v < 0.0) {
            return Value(-1.0);
        }
        return Value(0.0);
    }
    static Value copy_sign(mem::Arena &, std::span<Value> args)
    {
        return Value(std::copysign(args[0].as_float(), args[1].as_float()));
    }
    static Value next_after(mem::Arena &, std::span<Value> args)
    {
        return Value(std::nextafter(args[0].as_float(), args[1].as_float()));
    }

    // --- Min / Max / Clamp ---
    static Value min(mem::Arena &, std::span<Value> args)
    {
        return Value(std::min(args[0].as_float(), args[1].as_float()));
    }
    static Value max(mem::Arena &, std::span<Value> args)
    {
        return Value(std::max(args[0].as_float(), args[1].as_float()));
    }
    static Value clamp(mem::Arena &, std::span<Value> args)
    {
        return Value(std::clamp(args[0].as_float(), args[1].as_float(), args[2].as_float()));
    }
    static Value clamp01(mem::Arena &, std::span<Value> args)
    {
        return Value(std::clamp(args[0].as_float(), 0.0, 1.0));
    }

    // --- Interpolation ---
    static Value lerp(mem::Arena &, std::span<Value> args)
    {
        double a = args[0].as_float();
        double b = args[1].as_float();
        double t = args[2].as_float();
        return Value(a + t * (b - a));
    }
    static Value lerp_clamped(mem::Arena &, std::span<Value> args)
    {
        double a = args[0].as_float();
        double b = args[1].as_float();
        double t = std::clamp(args[2].as_float(), 0.0, 1.0);
        return Value(a + t * (b - a));
    }
    static Value inverse_lerp(mem::Arena &, std::span<Value> args)
    {
        double a = args[0].as_float();
        double b = args[1].as_float();
        double v = args[2].as_float();
        if (b == a) {
            return Value(0.0);
        }
        return Value((v - a) / (b - a));
    }
    static Value remap(mem::Arena &, std::span<Value> args)
    {
        // remap(value, in_min, in_max, out_min, out_max)
        double v = args[0].as_float();
        double in_min = args[1].as_float();
        double in_max = args[2].as_float();
        double out_min = args[3].as_float();
        double out_max = args[4].as_float();
        if (in_max == in_min) {
            return Value(out_min);
        }
        double t = (v - in_min) / (in_max - in_min);
        return Value(out_min + t * (out_max - out_min));
    }
    static Value smooth_step(mem::Arena &, std::span<Value> args)
    {
        // smooth_step(edge0, edge1, x)
        double t = std::clamp((args[2].as_float() - args[0].as_float()) / (args[1].as_float() - args[0].as_float()), 0.0, 1.0);
        return Value(t * t * (3.0 - 2.0 * t));
    }
    static Value smoother_step(mem::Arena &, std::span<Value> args)
    {
        double t = std::clamp((args[2].as_float() - args[0].as_float()) / (args[1].as_float() - args[0].as_float()), 0.0, 1.0);
        return Value(t * t * t * (t * (t * 6.0 - 15.0) + 10.0));
    }

    // --- Angle Helpers ---
    static Value degrees(mem::Arena &, std::span<Value> args)
    {
        return Value(args[0].as_float() * (180.0 / 3.14159265358979323846));
    }
    static Value radians(mem::Arena &, std::span<Value> args)
    {
        return Value(args[0].as_float() * (3.14159265358979323846 / 180.0));
    }
    static Value wrap_angle(mem::Arena &, std::span<Value> args)
    {
        // Wraps angle in radians to [-pi, pi]
        double a = args[0].as_float();
        constexpr double pi = 3.14159265358979323846;
        constexpr double two_pi = 2.0 * pi;
        a = std::fmod(a + pi, two_pi);
        if (a < 0.0) {
            a += two_pi;
        }
        return Value(a - pi);
    }

    // --- Float Classification ---
    static Value is_nan(mem::Arena &, std::span<Value> args)
    {
        return Value(std::isnan(args[0].as_float()));
    }
    static Value is_inf(mem::Arena &, std::span<Value> args)
    {
        return Value(std::isinf(args[0].as_float()));
    }
    static Value is_finite(mem::Arena &, std::span<Value> args)
    {
        return Value(std::isfinite(args[0].as_float()));
    }
    static Value is_normal(mem::Arena &, std::span<Value> args)
    {
        return Value(std::isnormal(args[0].as_float()));
    }

    // --- Float Comparison Helpers ---
    // is_nearly_equal(a, b, epsilon) — absolute tolerance
    static Value is_nearly_equal(mem::Arena &, std::span<Value> args)
    {
        double a = args[0].as_float();
        double b = args[1].as_float();
        double eps = args[2].as_float();
        return Value(std::abs(a - b) <= eps);
    }
    // is_similar(a, b) — relative + absolute tolerance combined (ULP-safe for general use)
    static Value is_similar(mem::Arena &, std::span<Value> args)
    {
        double a = args[0].as_float();
        double b = args[1].as_float();
        // Handle infinities and exact equality (including both zero)
        if (a == b) {
            return Value(true);
        }
        // If either is NaN, they are not similar
        if (std::isnan(a) || std::isnan(b)) {
            return Value(false);
        }
        // If either is infinite but not equal (caught above), not similar
        if (std::isinf(a) || std::isinf(b)) {
            return Value(false);
        }
        constexpr double rel_tol = 1e-9;
        constexpr double abs_tol = 1e-12;
        double diff = std::abs(a - b);
        double larger = std::max(std::abs(a), std::abs(b));
        return Value(diff <= abs_tol + rel_tol * larger);
    }
    // is_similar_eps(a, b, rel_tol, abs_tol) — user-controlled tolerances
    static Value is_similar_eps(mem::Arena &, std::span<Value> args)
    {
        double a = args[0].as_float();
        double b = args[1].as_float();
        double rel_tol = args[2].as_float();
        double abs_tol = args[3].as_float();
        if (a == b) {
            return Value(true);
        }
        if (std::isnan(a) || std::isnan(b)) {
            return Value(false);
        }
        if (std::isinf(a) || std::isinf(b)) {
            return Value(false);
        }
        double diff = std::abs(a - b);
        double larger = std::max(std::abs(a), std::abs(b));
        return Value(diff <= abs_tol + rel_tol * larger);
    }
    // ulp_distance — number of representable floats between a and b
    static Value ulp_distance(mem::Arena &, std::span<Value> args)
    {
        double a = args[0].as_float();
        double b = args[1].as_float();
        if (std::isnan(a) || std::isnan(b)) {
            return Value(std::numeric_limits<double>::quiet_NaN());
        }
        int64_t ia, ib;
        std::memcpy(&ia, &a, sizeof(double));
        std::memcpy(&ib, &b, sizeof(double));
        // Flip sign bits to form a linear order
        if (ia < 0) {
            ia = static_cast<int64_t>(0x8000000000000000ULL) - ia;
        }
        if (ib < 0) {
            ib = static_cast<int64_t>(0x8000000000000000ULL) - ib;
        }
        return Value(static_cast<double>(std::abs(ia - ib)));
    }

    // --- Integer Math ---
    static Value gcd(mem::Arena &, std::span<Value> args)
    {
        int64_t a = args[0].as_int();
        int64_t b = args[1].as_int();
        return Value(static_cast<int64_t>(std::gcd(std::abs(a), std::abs(b))));
    }
    static Value lcm(mem::Arena &, std::span<Value> args)
    {
        int64_t a = args[0].as_int();
        int64_t b = args[1].as_int();
        return Value(static_cast<int64_t>(std::lcm(std::abs(a), std::abs(b))));
    }
    static Value factorial(mem::Arena &, std::span<Value> args)
    {
        int64_t n = args[0].as_int();
        if (n < 0) {
            return Value(static_cast<int64_t>(-1)); // error sentinel
        }
        int64_t result = 1;
        for (int64_t i = 2; i <= n; ++i) {
            result *= i;
        }
        return Value(result);
    }
    static Value fibonacci(mem::Arena &, std::span<Value> args)
    {
        int64_t n = args[0].as_int();
        if (n < 0) {
            return Value(static_cast<int64_t>(-1));
        }
        if (n <= 1) {
            return Value(n);
        }
        int64_t a = 0, b = 1;
        for (int64_t i = 2; i <= n; ++i) {
            int64_t c = a + b;
            a = b;
            b = c;
        }
        return Value(b);
    }
    static Value pow_int(mem::Arena &, std::span<Value> args)
    {
        int64_t base = args[0].as_int();
        int64_t exp = args[1].as_int();
        if (exp < 0) {
            return Value(static_cast<int64_t>(0));
        }
        int64_t result = 1;
        while (exp > 0) {
            if (exp & 1) {
                result *= base;
            }
            base *= base;
            exp >>= 1;
        }
        return Value(result);
    }
    static Value isqrt(mem::Arena &, std::span<Value> args)
    {
        int64_t n = args[0].as_int();
        if (n < 0) {
            return Value(static_cast<int64_t>(-1));
        }
        return Value(static_cast<int64_t>(std::sqrt(static_cast<double>(n))));
    }
    static Value is_prime(mem::Arena &, std::span<Value> args)
    {
        int64_t n = args[0].as_int();
        if (n < 2) {
            return Value(false);
        }
        if (n == 2) {
            return Value(true);
        }
        if (n % 2 == 0) {
            return Value(false);
        }
        for (int64_t i = 3; i * i <= n; i += 2) {
            if (n % i == 0) {
                return Value(false);
            }
        }
        return Value(true);
    }
    static Value digit_sum(mem::Arena &, std::span<Value> args)
    {
        int64_t n = std::abs(args[0].as_int());
        int64_t sum = 0;
        while (n > 0) {
            sum += n % 10;
            n /= 10;
        }
        return Value(sum);
    }
};

inline void register_math(Type_environment &env)
{
    // --- Trigonometry ---
    env.define_native("math::sin",             std::vector<std::string>{"f64"}, "f64", math_native::sin);
    env.define_native("math::cos",             std::vector<std::string>{"f64"}, "f64", math_native::cos);
    env.define_native("math::tan",             std::vector<std::string>{"f64"}, "f64", math_native::tan);
    env.define_native("math::asin",            std::vector<std::string>{"f64"}, "f64", math_native::asin);
    env.define_native("math::acos",            std::vector<std::string>{"f64"}, "f64", math_native::acos);
    env.define_native("math::atan",            std::vector<std::string>{"f64"}, "f64", math_native::atan);
    env.define_native("math::atan2",           std::vector<std::string>{"f64", "f64"}, "f64", math_native::atan2);

    // --- Hyperbolic ---
    env.define_native("math::sinh",            std::vector<std::string>{"f64"}, "f64", math_native::sinh);
    env.define_native("math::cosh",            std::vector<std::string>{"f64"}, "f64", math_native::cosh);
    env.define_native("math::tanh",            std::vector<std::string>{"f64"}, "f64", math_native::tanh);
    env.define_native("math::asinh",           std::vector<std::string>{"f64"}, "f64", math_native::asinh);
    env.define_native("math::acosh",           std::vector<std::string>{"f64"}, "f64", math_native::acosh);
    env.define_native("math::atanh",           std::vector<std::string>{"f64"}, "f64", math_native::atanh);

    // --- Exponents & Roots ---
    env.define_native("math::pow",             std::vector<std::string>{"f64", "f64"}, "f64", math_native::pow);
    env.define_native("math::sqrt",            std::vector<std::string>{"f64"}, "f64", math_native::sqrt);
    env.define_native("math::cbrt",            std::vector<std::string>{"f64"}, "f64", math_native::cbrt);
    env.define_native("math::exp",             std::vector<std::string>{"f64"}, "f64", math_native::exp);
    env.define_native("math::exp2",            std::vector<std::string>{"f64"}, "f64", math_native::exp2);
    env.define_native("math::expm1",           std::vector<std::string>{"f64"}, "f64", math_native::expm1);
    env.define_native("math::log",             std::vector<std::string>{"f64"}, "f64", math_native::log);
    env.define_native("math::log10",           std::vector<std::string>{"f64"}, "f64", math_native::log10);
    env.define_native("math::log2",            std::vector<std::string>{"f64"}, "f64", math_native::log2);
    env.define_native("math::log1p",           std::vector<std::string>{"f64"}, "f64", math_native::log1p);
    env.define_native("math::hypot",           std::vector<std::string>{"f64", "f64"}, "f64", math_native::hypot);
    env.define_native("math::rsqrt",           std::vector<std::string>{"f64"}, "f64", math_native::rsqrt);
    env.define_native("math::fma",             std::vector<std::string>{"f64", "f64", "f64"}, "f64", math_native::fma);

    // --- Rounding & Sign ---
    env.define_native("math::ceil",            std::vector<std::string>{"f64"}, "f64", math_native::ceil);
    env.define_native("math::floor",           std::vector<std::string>{"f64"}, "f64", math_native::floor);
    env.define_native("math::round",           std::vector<std::string>{"f64"}, "f64", math_native::round);
    env.define_native("math::trunc",           std::vector<std::string>{"f64"}, "f64", math_native::trunc);
    env.define_native("math::abs",             std::vector<std::string>{"f64"}, "f64", math_native::abs);
    env.define_native("math::fract",           std::vector<std::string>{"f64"}, "f64", math_native::fract);
    env.define_native("math::sign",            std::vector<std::string>{"f64"}, "f64", math_native::sign);
    env.define_native("math::copy_sign",       std::vector<std::string>{"f64", "f64"}, "f64", math_native::copy_sign);
    env.define_native("math::next_after",      std::vector<std::string>{"f64", "f64"}, "f64", math_native::next_after);

    // --- Min / Max / Clamp ---
    env.define_native("math::min",             std::vector<std::string>{"f64", "f64"}, "f64", math_native::min);
    env.define_native("math::max",             std::vector<std::string>{"f64", "f64"}, "f64", math_native::max);
    env.define_native("math::clamp",           std::vector<std::string>{"f64", "f64", "f64"}, "f64", math_native::clamp);
    env.define_native("math::clamp01",         std::vector<std::string>{"f64"}, "f64", math_native::clamp01);

    // --- Interpolation ---
    env.define_native("math::lerp",            std::vector<std::string>{"f64", "f64", "f64"}, "f64", math_native::lerp);
    env.define_native("math::lerp_clamped",    std::vector<std::string>{"f64", "f64", "f64"}, "f64", math_native::lerp_clamped);
    env.define_native("math::inverse_lerp",    std::vector<std::string>{"f64", "f64", "f64"}, "f64", math_native::inverse_lerp);
    env.define_native("math::remap",           std::vector<std::string>{"f64", "f64", "f64", "f64", "f64"}, "f64", math_native::remap);
    env.define_native("math::smooth_step",     std::vector<std::string>{"f64", "f64", "f64"}, "f64", math_native::smooth_step);
    env.define_native("math::smoother_step",   std::vector<std::string>{"f64", "f64", "f64"}, "f64", math_native::smoother_step);

    // --- Angle Helpers ---
    env.define_native("math::degrees",         std::vector<std::string>{"f64"}, "f64", math_native::degrees);
    env.define_native("math::radians",         std::vector<std::string>{"f64"}, "f64", math_native::radians);
    env.define_native("math::wrap_angle",      std::vector<std::string>{"f64"}, "f64", math_native::wrap_angle);

    // --- Float Classification ---
    env.define_native("math::is_nan",          std::vector<std::string>{"f64"}, "bool", math_native::is_nan);
    env.define_native("math::is_inf",          std::vector<std::string>{"f64"}, "bool", math_native::is_inf);
    env.define_native("math::is_finite",       std::vector<std::string>{"f64"}, "bool", math_native::is_finite);
    env.define_native("math::is_normal",       std::vector<std::string>{"f64"}, "bool", math_native::is_normal);

    // --- Float Comparison Helpers ---
    env.define_native("math::is_nearly_equal", std::vector<std::string>{"f64", "f64", "f64"}, "bool", math_native::is_nearly_equal);
    env.define_native("math::is_similar",      std::vector<std::string>{"f64", "f64"}, "bool", math_native::is_similar);
    env.define_native("math::is_similar_eps",  std::vector<std::string>{"f64", "f64", "f64", "f64"}, "bool", math_native::is_similar_eps);
    env.define_native("math::ulp_distance",    std::vector<std::string>{"f64", "f64"}, "f64", math_native::ulp_distance);

    // --- Integer Math ---
    env.define_native("math::gcd",             std::vector<std::string>{"i32", "i64"}, "i64", math_native::gcd);
    env.define_native("math::lcm",             std::vector<std::string>{"i32", "i64"}, "i64", math_native::lcm);
    env.define_native("math::factorial",       std::vector<std::string>{"i32"}, "i64", math_native::factorial);
    env.define_native("math::fibonacci",       std::vector<std::string>{"i32"}, "i64", math_native::fibonacci);
    env.define_native("math::pow_int",         std::vector<std::string>{"i32", "i32"}, "i64", math_native::pow_int);
    env.define_native("math::isqrt",           std::vector<std::string>{"i32"}, "i64", math_native::isqrt);
    env.define_native("math::is_prime",        std::vector<std::string>{"i32"}, "bool", math_native::is_prime);
    env.define_native("math::digit_sum",       std::vector<std::string>{"i32"}, "i64", math_native::digit_sum);

    // --- Constants ---
    env.define_native_const("math::pi",      Value(3.14159265358979323846));
    env.define_native_const("math::tau",     Value(6.28318530717958647692));
    env.define_native_const("math::e",       Value(2.71828182845904523536));
    env.define_native_const("math::phi",     Value(1.61803398874989484820));
    env.define_native_const("math::sqrt2",   Value(1.41421356237309504880));
    env.define_native_const("math::sqrt3",   Value(1.73205080756887729353));
    env.define_native_const("math::ln2",     Value(0.69314718055994530942));
    env.define_native_const("math::ln10",    Value(2.30258509299404568402));
    env.define_native_const("math::log2e",   Value(1.44269504088896340736));
    env.define_native_const("math::log10e",  Value(0.43429448190325182765));
    env.define_native_const("math::inf",     Value(std::numeric_limits<double>::infinity()));
    env.define_native_const("math::nan",     Value(std::numeric_limits<double>::quiet_NaN()));
    env.define_native_const("math::epsilon", Value(std::numeric_limits<double>::epsilon()));
    env.define_native_const("math::max_f64", Value(std::numeric_limits<double>::max()));
    env.define_native_const("math::min_f64", Value(std::numeric_limits<double>::lowest()));
    env.define_native_const("math::max_f32", Value(std::numeric_limits<float>::max()));
    env.define_native_const("math::min_f32", Value(std::numeric_limits<float>::lowest()));
    env.define_native_const("math::max_f16", Value(std::numeric_limits<float>::max()));
    env.define_native_const("math::min_f16", Value(std::numeric_limits<float>::lowest()));
    env.define_native_const("math::max_f16", Value(65504.0f));
    env.define_native_const("math::min_f16", Value(-65504.0f));
    env.define_native_const("math::smallest_f16", Value(6.10352e-5f));
    env.define_native_const("math::tiny_f16", Value(5.96046e-8f));

    env.define_native_const("math::max_i64", Value(std::numeric_limits<int64_t>::max()));
    env.define_native_const("math::min_i64", Value(std::numeric_limits<int64_t>::min()));
    env.define_native_const("math::max_i32", Value(std::numeric_limits<int32_t>::max()));
    env.define_native_const("math::min_i32", Value(std::numeric_limits<int32_t>::min()));
    env.define_native_const("math::max_i16", Value(std::numeric_limits<int16_t>::max()));
    env.define_native_const("math::min_i16", Value(std::numeric_limits<int16_t>::min()));
    env.define_native_const("math::max_i8",  Value(std::numeric_limits<int8_t>::max()));
    env.define_native_const("math::min_i8",  Value(std::numeric_limits<int8_t>::min()));

    env.define_native_const("math::max_u64", Value(std::numeric_limits<uint64_t>::max()));
    env.define_native_const("math::min_u64", Value(std::numeric_limits<uint64_t>::min()));
    env.define_native_const("math::max_u32", Value(std::numeric_limits<uint32_t>::max()));
    env.define_native_const("math::min_u32", Value(std::numeric_limits<uint32_t>::min()));
    env.define_native_const("math::max_u16", Value(std::numeric_limits<uint16_t>::max()));
    env.define_native_const("math::min_u16", Value(std::numeric_limits<uint16_t>::min()));
    env.define_native_const("math::max_u8", Value(std::numeric_limits<uint8_t>::max()));
    env.define_native_const("math::min_u8", Value(std::numeric_limits<uint8_t>::min()));
}

} // namespace phos::vm::modules

