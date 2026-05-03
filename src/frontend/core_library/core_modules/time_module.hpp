#pragma once

#include "core/value/value.hpp"
#include "frontend/environment/type_environment.hpp"

#include <chrono>
#include <ctime>
#include <iomanip>
#include <span>
#include <sstream>
#include <string>
#include <thread>

namespace phos::vm::modules {

struct time_native
{
    // Returns monotonic time in seconds (as a high-precision float)
    static Value now(mem::Arena &, std::span<Value>)
    {
        auto ticks = std::chrono::steady_clock::now().time_since_epoch();
        return Value(std::chrono::duration<double>(ticks).count());
    }

    // Returns CPU time consumed by the program so far in seconds
    static Value cpu_time(mem::Arena &, std::span<Value>)
    {
        double cpu_time_used = static_cast<double>(std::clock()) / CLOCKS_PER_SEC;
        return Value(cpu_time_used);
    }

    // --- System Epoch Clocks ---

    // Returns standard Unix epoch timestamp in seconds
    static Value epoch(mem::Arena &, std::span<Value>)
    {
        auto now = std::chrono::system_clock::now().time_since_epoch();
        return Value(static_cast<int64_t>(std::chrono::duration_cast<std::chrono::seconds>(now).count()));
    }

    // Returns standard Unix epoch timestamp in milliseconds
    static Value epoch_ms(mem::Arena &, std::span<Value>)
    {
        auto now = std::chrono::system_clock::now().time_since_epoch();
        return Value(static_cast<int64_t>(std::chrono::duration_cast<std::chrono::milliseconds>(now).count()));
    }

    // --- Thread Control ---

    static Value sleep(mem::Arena &, std::span<Value> args)
    {
        int64_t ms = args[0].as_int();
        if (ms > 0) {
            std::this_thread::sleep_for(std::chrono::milliseconds(ms));
        }
        return Value(nullptr); // Void return
    }

    // --- Formatting Utilities ---

    // Returns current UTC date/time as ISO 8601 string: "YYYY-MM-DDTHH:MM:SSZ"
    static Value utc_string(mem::Arena &arena, std::span<Value>)
    {
        auto now = std::chrono::system_clock::now();
        std::time_t current_time = std::chrono::system_clock::to_time_t(now);
        std::tm *tm_utc = std::gmtime(&current_time);

        std::ostringstream ss;
        ss << std::put_time(tm_utc, "%Y-%m-%dT%H:%M:%SZ");
        return Value::make_string(arena, ss.str());
    }

    // Returns current local date/time as a standard string: "YYYY-MM-DD HH:MM:SS"
    static Value local_string(mem::Arena &arena, std::span<Value>)
    {
        auto now = std::chrono::system_clock::now();
        std::time_t current_time = std::chrono::system_clock::to_time_t(now);
        std::tm *tm_local = std::localtime(&current_time);

        std::ostringstream ss;
        ss << std::put_time(tm_local, "%Y-%m-%d %H:%M:%S");
        return Value::make_string(arena, ss.str());
    }

    // --- Parsing and Arithmetic ---

    // Safely parses ISO 8601 strings manually
    static Value parse_utc(mem::Arena &, std::span<Value> args)
    {
        std::string date_str(args[0].as_string());
        std::tm tm_time = {};

        // Ensure defaults for time if only date is provided
        tm_time.tm_hour = 0;
        tm_time.tm_min = 0;
        tm_time.tm_sec = 0;

        int year = 0, month = 0, day = 0;
        int hour = 0, minute = 0, second = 0;
        char dash1, dash2, t_sep, colon1, colon2, z_char;

        std::istringstream ss(date_str);

        // Try to parse YYYY-MM-DD
        if (ss >> year >> dash1 >> month >> dash2 >> day && dash1 == '-' && dash2 == '-') {
            tm_time.tm_year = year - 1900;
            tm_time.tm_mon = month - 1;
            tm_time.tm_mday = day;

            // Try to continue parsing time: THH:MM:SSZ
            if (ss >> t_sep >> hour >> colon1 >> minute >> colon2 >> second >> z_char && (t_sep == 'T' || t_sep == ' ') && colon1 == ':'
                && colon2 == ':') {
                tm_time.tm_hour = hour;
                tm_time.tm_min = minute;
                tm_time.tm_sec = second;
            }

#if defined(_WIN32)
            std::time_t time = _mkgmtime(&tm_time);
#else
            std::time_t time = timegm(&tm_time);
#endif

            if (time != -1) {
                return Value(static_cast<int64_t>(time));
            }
        }

        // Return explicit Phos nil on failure
        return Value(nullptr);
    }

    // Returns the number of seconds between two epoch timestamps (end - start)
    static Value diff(mem::Arena &, std::span<Value> args)
    {
        int64_t end = args[0].as_int();
        int64_t start = args[1].as_int();
        return Value(end - start);
    }
};

inline void register_time(Type_environment &env)
{
    // --- High-Resolution Timers ---
    env.define_native("time::now", std::vector<std::string>{}, "f64", time_native::now);
    env.define_native("time::cpu_time", std::vector<std::string>{}, "f64", time_native::cpu_time);

    // --- System Epoch Clocks ---
    env.define_native("time::epoch", std::vector<std::string>{}, "i64", time_native::epoch);
    env.define_native("time::epoch_ms", std::vector<std::string>{}, "i64", time_native::epoch_ms);

    // --- Thread Control ---
    env.define_native("time::sleep", std::vector<std::string>{"i64"}, "void", time_native::sleep);

    // --- String Formatting ---
    env.define_native("time::utc_string", std::vector<std::string>{}, "string", time_native::utc_string);
    env.define_native("time::local_string", std::vector<std::string>{}, "string", time_native::local_string);

    // --- Parsing and Arithmetic ---
    // Note the 'i64?' return type for parse_utc. It returns an optional to force safe unwrapping in Phos.
    env.define_native("time::parse_utc", std::vector<std::string>{"string"}, "i64?", time_native::parse_utc);
    env.define_native("time::diff", std::vector<std::string>{"i64", "i64"}, "i64", time_native::diff);

    // --- Constants ---
    env.define_native_const("time::MS_PER_SEC", Value(static_cast<int64_t>(1000)));
    env.define_native_const("time::US_PER_SEC", Value(static_cast<int64_t>(1000000)));
    env.define_native_const("time::NS_PER_SEC", Value(static_cast<int64_t>(1000000000)));
    env.define_native_const("time::SEC_PER_MIN", Value(static_cast<int64_t>(60)));
    env.define_native_const("time::MIN_PER_HOUR", Value(static_cast<int64_t>(60)));
    env.define_native_const("time::HOUR_PER_DAY", Value(static_cast<int64_t>(24)));
}

} // namespace phos::vm::modules
