#pragma once

#include "core/value/ffi_support.hpp"
#include "frontend/environment/type_environment.hpp"

#include <span>
#include <vector>

namespace phos::vm::modules {

using Ctx = vm::Vm_context;

inline types::Model_type ffi_demo_point_sig{};
inline types::Model_type ffi_demo_structural_point_sig{};

struct ffi_demo_native
{
    static Value translate(Ctx &ctx, std::span<Value> args)
    {
        const Value &point = args[0];
        int64_t dx = args[1].as_int();
        int64_t dy = args[2].as_int();
        const auto &point_sig = point.as_model()->signature.fields.empty() ? ffi_demo_point_sig : point.as_model()->signature;

        Value x = Value(ffi::require_model_field(point, point_sig, "x").as_int() + dx);
        Value y = Value(ffi::require_model_field(point, point_sig, "y").as_int() + dy);

        std::vector<Value> fields = {x, y};
        return ffi::make_model(ctx, point_sig, fields);
    }

    static Value magnitude_sq(Ctx &, std::span<Value> args)
    {
        const auto &point_sig = args[0].as_model()->signature.fields.empty() ? ffi_demo_structural_point_sig : args[0].as_model()->signature;
        int64_t x = ffi::require_model_field(args[0], point_sig, "x").as_int();
        int64_t y = ffi::require_model_field(args[0], point_sig, "y").as_int();
        return Value((x * x) + (y * y));
    }

    static Value sum_all(Ctx &, std::span<Value> args)
    {
        int64_t sum = 0;
        for (const Value &arg : args) {
            sum += arg.as_int();
        }
        return Value(sum);
    }
};

inline void register_ffi_demo(Type_environment &env)
{
    auto point_type = env.define_native_model("ffi_demo::Point", {{"x", env.tt.get_i64()}, {"y", env.tt.get_i64()}});
    ffi_demo_point_sig = env.tt.get(point_type).as<types::Model_type>();
    ffi_demo_structural_point_sig = env.tt.get(env.tt.model("", {{"x", env.tt.get_i64()}, {"y", env.tt.get_i64()}})).as<types::Model_type>();

    env.define_native(
        "ffi_demo::translate",
        std::vector<Native_param>{
            Native_param{.name = "point", .type_str = "ffi_demo::Point", .default_value = std::nullopt},
            Native_param{.name = "dx", .type_str = "i64", .default_value = std::nullopt},
            Native_param{.name = "dy", .type_str = "i64", .default_value = std::nullopt},
        },
        "ffi_demo::Point",
        ffi_demo_native::translate);

    env.define_native(
        "ffi_demo::magnitude_sq",
        std::vector<Native_param>{
            Native_param{.name = "point", .type_str = "model { x: i64; y: i64 }", .default_value = std::nullopt},
        },
        "i64",
        ffi_demo_native::magnitude_sq);

    env.define_native(
        "ffi_demo::sum_all",
        std::vector<Native_param>{
            Native_param{.name = "values", .type_str = "i64", .default_value = std::nullopt, .is_variadic = true},
        },
        "i64",
        ffi_demo_native::sum_all);
}

} // namespace phos::vm::modules
