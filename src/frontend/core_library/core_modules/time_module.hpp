#pragma once

#include "core/value/ffi_support.hpp"
#include "core/value/value.hpp"
#include "frontend/environment/type_environment.hpp"

#include <chrono>
#include <ctime>
#include <span>
#include <sstream>
#include <string>
#include <thread>
#include <vector>

namespace phos::vm::modules {

using Ctx = vm::Vm_context;

// Cache the signature so we can quickly instantiate models at runtime
inline types::Model_type time_datetime_sig{};

struct time_native
{
    // Returns monotonic time in seconds (as a high-precision float)
    static Value now(Ctx &, std::span<Value>)
    {
        auto ticks = std::chrono::steady_clock::now().time_since_epoch();
        return Value(std::chrono::duration<double>(ticks).count());
    }

    // Returns CPU time consumed by the program so far in seconds
    static Value cpu_time(Ctx &, std::span<Value>)
    {
        double cpu_time_used = static_cast<double>(std::clock()) / CLOCKS_PER_SEC;
        return Value(cpu_time_used);
    }

    // System Epoch Clocks
    static Value epoch(Ctx &, std::span<Value>)
    {
        auto now = std::chrono::system_clock::now().time_since_epoch();
        return Value(static_cast<int64_t>(std::chrono::duration_cast<std::chrono::seconds>(now).count()));
    }

    static Value epoch_ms(Ctx &, std::span<Value>)
    {
        auto now = std::chrono::system_clock::now().time_since_epoch();
        return Value(static_cast<int64_t>(std::chrono::duration_cast<std::chrono::milliseconds>(now).count()));
    }

    // Thread Control
    static Value sleep(Ctx &, std::span<Value> args)
    {
        int64_t ms = args[0].as_int();
        if (ms > 0) {
            std::this_thread::sleep_for(std::chrono::milliseconds(ms));
        }
        return Value(nullptr); // Void return
    }

    // DateTime Model Constructors
    static Value utc_datetime(Ctx &ctx, std::span<Value>)
    {
        auto now = std::chrono::system_clock::now();
        std::time_t current_time = std::chrono::system_clock::to_time_t(now);
        std::tm *tm_utc = std::gmtime(&current_time);

        std::vector<Value> fields = {
            Value(static_cast<int64_t>(tm_utc->tm_year + 1900)),
            Value(static_cast<int64_t>(tm_utc->tm_mon + 1)),
            Value(static_cast<int64_t>(tm_utc->tm_mday)),
            Value(static_cast<int64_t>(tm_utc->tm_hour)),
            Value(static_cast<int64_t>(tm_utc->tm_min)),
            Value(static_cast<int64_t>(tm_utc->tm_sec))};

        return ffi::make_model(ctx, time_datetime_sig, fields);
    }

    static Value local_datetime(Ctx &ctx, std::span<Value>)
    {
        auto now = std::chrono::system_clock::now();
        std::time_t current_time = std::chrono::system_clock::to_time_t(now);
        std::tm *tm_local = std::localtime(&current_time);

        std::vector<Value> fields = {
            Value(static_cast<int64_t>(tm_local->tm_year + 1900)),
            Value(static_cast<int64_t>(tm_local->tm_mon + 1)),
            Value(static_cast<int64_t>(tm_local->tm_mday)),
            Value(static_cast<int64_t>(tm_local->tm_hour)),
            Value(static_cast<int64_t>(tm_local->tm_min)),
            Value(static_cast<int64_t>(tm_local->tm_sec))};

        return ffi::make_model(ctx, time_datetime_sig, fields);
    }

    // Parsing and Arithmetic
    static Value parse_utc(Ctx &ctx, std::span<Value> args)
    {
        std::string date_str(args[0].as_string());
        std::tm tm_time = {};

        tm_time.tm_hour = 0;
        tm_time.tm_min = 0;
        tm_time.tm_sec = 0;

        int year = 0, month = 0, day = 0;
        int hour = 0, minute = 0, second = 0;
        char dash1, dash2, t_sep, colon1, colon2, z_char;

        std::istringstream ss(date_str);

        if (ss >> year >> dash1 >> month >> dash2 >> day && dash1 == '-' && dash2 == '-') {
            tm_time.tm_year = year - 1900;
            tm_time.tm_mon = month - 1;
            tm_time.tm_mday = day;

            if (ss >> t_sep >> hour >> colon1 >> minute >> colon2 >> second >> z_char && (t_sep == 'T' || t_sep == ' ') && colon1 == ':'
                && colon2 == ':') {
                tm_time.tm_hour = hour;
                tm_time.tm_min = minute;
                tm_time.tm_sec = second;
            }

            std::vector<Value> fields = {
                Value(static_cast<int64_t>(year)),
                Value(static_cast<int64_t>(month)),
                Value(static_cast<int64_t>(day)),
                Value(static_cast<int64_t>(tm_time.tm_hour)),
                Value(static_cast<int64_t>(tm_time.tm_min)),
                Value(static_cast<int64_t>(tm_time.tm_sec))};

            return ffi::make_model(ctx, time_datetime_sig, fields);
        }

        return Value(nullptr);
    }

    static Value diff(Ctx &, std::span<Value> args)
    {
        int64_t end = args[0].as_int();
        int64_t start = args[1].as_int();
        return Value(end - start);
    }
};

inline void register_time(Type_environment &env)
{
    // 1. Register the DateTime Model
    auto datetime_type = env.define_native_model(
        "time::Date_time",
        {
            {"year", env.tt.get_i64()},
            {"month", env.tt.get_i64()},
            {"day", env.tt.get_i64()},
            {"hour", env.tt.get_i64()},
            {"minute", env.tt.get_i64()},
            {"second", env.tt.get_i64()}
        }
    );

    // Cache the signature for the FFI builder
    time_datetime_sig = env.tt.get(datetime_type).as<types::Model_type>();

    // 2. High-Resolution Timers
    env.define_native("time::now", std::vector<Native_param>{}, "f64", time_native::now);
    env.define_native("time::cpu_time", std::vector<Native_param>{}, "f64", time_native::cpu_time);

    // 3. System Epoch Clocks
    env.define_native("time::epoch", std::vector<Native_param>{}, "i64", time_native::epoch);
    env.define_native("time::epoch_ms", std::vector<Native_param>{}, "i64", time_native::epoch_ms);

    // 4. Thread Control
    env.define_native(
        "time::sleep",
        std::vector<Native_param>{Native_param{.name = "ms", .type_str = "i64", .default_value = std::nullopt}},
        "void",
        time_native::sleep);

    // 5. Native Model Constructors
    env.define_native("time::utc_datetime", std::vector<Native_param>{}, "time::DateTime", time_native::utc_datetime);
    env.define_native("time::local_datetime", std::vector<Native_param>{}, "time::DateTime", time_native::local_datetime);

    env.define_native(
        "time::parse_utc",
        std::vector<Native_param>{Native_param{.name = "date_str", .type_str = "string", .default_value = std::nullopt}},
        "time::Date_time?", // Returns an optional DateTime model now!
        time_native::parse_utc);

    // 6. Parsing and Arithmetic
    env.define_native(
        "time::diff",
        std::vector<Native_param>{
            Native_param{.name = "end", .type_str = "i64", .default_value = std::nullopt},
            Native_param{.name = "start", .type_str = "i64", .default_value = std::nullopt}},
        "i64",
        time_native::diff);

    // 7. Constants
    env.define_native_const("time::MS_PER_SEC", Value(static_cast<int64_t>(1000)), "i64");
    env.define_native_const("time::US_PER_SEC", Value(static_cast<int64_t>(1000000)), "i64");
    env.define_native_const("time::NS_PER_SEC", Value(static_cast<int64_t>(1000000000)), "i64");
    env.define_native_const("time::SEC_PER_MIN", Value(static_cast<int64_t>(60)), "i64");
    env.define_native_const("time::MIN_PER_HOUR", Value(static_cast<int64_t>(60)), "i64");
    env.define_native_const("time::HOUR_PER_DAY", Value(static_cast<int64_t>(24)), "i64");
}

} // namespace phos::vm::modules
