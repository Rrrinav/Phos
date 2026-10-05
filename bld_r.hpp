/*
  MIT
  Copyright Dec 2025, Rinav (github: rrrinav)

  Permission is hereby granted, free of charge,
  to any person obtaining a copy of this software and associated documentation files(the "Software"),
  to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute,
  sublicense, and / or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
  subject to the following conditions :

  The above copyright notice and this permission notice shall be included in all copies
  or
  substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS",
  WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
  DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

  Inspired by nob.h — github.com/tsoding/nob.h
*/

#ifndef B_LDR_HPP
#define B_LDR_HPP

#include <any>
#include <atomic>
#include <bit> // std::endian (bld::env)
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <expected>
#include <filesystem>
#include <flat_map>
#include <format>
#include <functional>
#include <iostream>
#include <memory>
#include <mutex>
#include <optional>
#include <ostream>
#include <print>
#include <ranges>
#include <signal.h>
#include <source_location>
#include <span>
#include <string>
#include <string_view>
#include <system_error>
// <thread> is implementation-only (see details::cpu_count()).
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

// Platform
#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <fcntl.h>
#include <io.h>
#include <process.h>
#include <windows.h>
#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#define STDOUT_FILENO 1
#define STDERR_FILENO 2
#endif
#else // POSIX / Linux
#include <fcntl.h>
#include <sys/types.h>
#endif
// NOTE: <poll.h>, <sched.h>, <sys/wait.h>, <unistd.h> are implementation-only
// and are included behind B_LDR_IMPLEMENTATION below to keep the interface light.

//   SECTION 01 — Errors & formatters ............. bld::Err
//   SECTION 02 — Logging ......................... bld::Logger, bld::log
//   SECTION 03 — Commands & Processes ............ bld::Cmd, bld::Proc, Fd_view
//   SECTION 04 — Execution ....................... bld::run, bld::capture, Task
//   SECTION 05 — Rebuild helpers ................. is_outdated, rebuild_this_when_*
//   SECTION 06 — Config .......................... bld::Config
//   SECTION 07 — Test helpers .................... bld::test
//   SECTION 08 — Filesystem ...................... bld::fs
//   SECTION 09 — String utilities ................ bld::str
//   SECTION 10 — Time ............................ bld::time
//   SECTION 11 — Environment (portable only) ..... bld::env  (kept low, if-constexpr-safe)
//   SECTION 12 — Formatters & inline templates ... std::formatter + templates

// SECTION 01 — Errors & formatters (bld::Err)
//   Declarations only. Definitions live in IMPL SECTION 01.

namespace bld {
struct Err
{
    using Error_pt = std::shared_ptr<Err>;
    std::error_code err;
    std::string msg{""};
    std::any payload{};
    Error_pt cause_{nullptr};

    static auto erc(std::errc code, std::string message = "") -> Err;
    static auto erno(int code, std::string message = "") -> Err;

    template <typename T>
    auto with_payload(T &&data) && -> Err;

    auto with_cause(Err root_cause) && -> Err;
    auto with_cause(Error_pt root_cause_ptr) && -> Err;
};
} // namespace bld

// clang-format off
template <>
struct std::formatter<bld::Err>
{
    enum class mode { plain, debug };
    mode fmt = mode::plain;
    constexpr auto parse(std::format_parse_context &ctx) -> std::format_parse_context::iterator;
    auto format(const bld::Err &err, std::format_context &ctx) const -> std::format_context::iterator;
};
// clang-format on

// SECTION 02 — Logging (bld::Logger, bld::log)
//   Declarations only. Definitions live in IMPL SECTION 02.

namespace bld::log::detail {
// A proxy for easier handling of streams.
// clang-format off
struct Stream_proxy
{
    std::ostream *ptr = &std::cerr;
    void operator=(std::ostream &os);
    std::ostream &get() const;
};
// clang-format on
} // namespace bld::log::detail

namespace bld {
using namespace std::string_view_literals;

struct Logger
{
    enum class Level { dbg = 0, inf = 1, wrn = 2, err = 3, ftl = 4 };

    struct Log_record
    {
        Level lvl;
        std::chrono::system_clock::time_point timestamp;
        std::string_view str;
    };

    using Logger_fn_t = std::function<void(std::ostream &, const Log_record &)>;
    inline static log::detail::Stream_proxy ostream;

    // clang-format off
    struct Default_logger_fn
    {
        inline static Level min_lvl = Level::dbg;
        inline static bool use_color = false;
        inline static constexpr std::string_view reset = "\x1b[0m";

        struct Style
        {
            std::string_view label;
            std::string_view color;
        };

        [[nodiscard]]
        static constexpr auto style(Level lvl) noexcept -> Style;

        auto operator()(std::ostream& stream, const Log_record& record) const -> void;
    };
    // clang-format on

    inline static std::atomic<Level> min_lvl{Level::inf};

    // Indentation for the default sink: N levels x indent_width spaces
    // prefixed to the message (after the "LEVEL: " tag). Single-threaded
    // schedulers just call bld::log::indent()/unindent(); the counter is
    // atomic so ad-hoc user threads don't corrupt it (interleaving may
    // still look ragged — indent is meant for single-threaded scripts).
    inline static std::atomic<int> indent_level{0};
    inline static constexpr int indent_width = 2;

    static auto set_logger_fn(Logger_fn_t fn, std::source_location loc = std::source_location::current()) -> void;

    inline static std::atomic<bool> logger_locked{false};
    inline static Logger_fn_t logger_fn = Default_logger_fn{};
    inline static std::mutex config_mtx;
};
} // namespace bld

namespace bld::log {

template <typename... Args>
inline void invoke_logger(bld::Logger::Level level, std::ostream &str, std::format_string<Args...> fmt, Args &&...args);

/// Sets the minimum severity that gets emitted (default: Level::inf).
inline void set_min_level(bld::Logger::Level lvl)
{
    bld::Logger::min_lvl.store(lvl, std::memory_order_relaxed);
}

/// Current indent level (see indent()/indent_scope).
[[nodiscard]] inline auto indent_level() -> int
{
    int n = bld::Logger::indent_level.load(std::memory_order_relaxed);
    return n < 0 ? 0 : n;
}

/// Pushes indentation for subsequent default-sink messages (clamped >= 0).
inline void indent(int n = 1)
{
    bld::Logger::indent_level.fetch_add(n, std::memory_order_relaxed);
    if (bld::Logger::indent_level.load(std::memory_order_relaxed) < 0) {
        bld::Logger::indent_level.store(0, std::memory_order_relaxed);
    }
}

/// Pops indentation pushed by indent() (clamped >= 0).
inline void unindent(int n = 1)
{
    indent(-n);
}

/// Sets the indent level directly (clamped >= 0).
inline void set_indent(int n)
{
    bld::Logger::indent_level.store(n < 0 ? 0 : n, std::memory_order_relaxed);
}

/// RAII indent: indents on construction, restores the previous level on
/// destruction. Nestable; non-copyable so a scope can't be dedented twice.
///
///   bld::log::i("building app");
///   {
///       bld::log::indent_scope nest;
///       bld::log::i("compiling foo.cpp");  // printed indented
///   }
///   bld::log::i("done");  // back at the outer level
struct indent_scope
{
    int prev{0};
    explicit indent_scope(int n = 1) : prev(indent_level())
    {
        indent(n);
    }
    ~indent_scope()
    {
        set_indent(prev);
    }
    indent_scope(const indent_scope &) = delete;
    indent_scope &operator=(const indent_scope &) = delete;
    indent_scope(indent_scope &&) = delete;
    indent_scope &operator=(indent_scope &&) = delete;
};

template <typename... Args>
void i(std::format_string<Args...> fmt, Args &&...args);

template <typename... Args>
void w(std::format_string<Args...> fmt, Args &&...args);

template <typename... Args>
void e(std::format_string<Args...> fmt, Args &&...args);

template <typename... Args>
void d(std::format_string<Args...> fmt, Args &&...args);

template <typename... Args>
void f(std::format_string<Args...> fmt, Args &&...args);

template <typename Os, typename... Args>
    requires std::derived_from<std::remove_reference_t<Os>, std::ostream>
void i(Os &str, std::format_string<Args...> fmt, Args &&...args);

template <typename Os, typename... Args>
    requires std::derived_from<std::remove_reference_t<Os>, std::ostream>
void w(Os &str, std::format_string<Args...> fmt, Args &&...args);

template <typename Os, typename... Args>
    requires std::derived_from<std::remove_reference_t<Os>, std::ostream>
void e(Os &str, std::format_string<Args...> fmt, Args &&...args);

template <typename Os, typename... Args>
    requires std::derived_from<std::remove_reference_t<Os>, std::ostream>
void d(Os &str, std::format_string<Args...> fmt, Args &&...args);

template <typename Os, typename... Args>
    requires std::derived_from<std::remove_reference_t<Os>, std::ostream>
void f(Os &str, std::format_string<Args...> fmt, Args &&...args);

} // namespace bld::log

namespace bld {
[[maybe_unused]] inline std::function<bool(std::string)> panic_callback = [](std::string str) {
    std::cout.flush();
    std::cerr.flush();
    std::println(std::cerr, "[B_LDR PANIC]: {}", str);
    return true;
};
auto panic(std::string s) -> void;
// Does nothing is ".exe" is already present
auto add_exe_on_win32([[maybe_unused]] std::string exec) -> std::string;
}; // namespace bld

// SECTION 03 — Commands & Processes (bld::Cmd, bld::Proc, Fd_view …)
//   Declarations only. Definitions live in IMPL SECTION 03.
namespace bld {
struct Cmd
{
    using value_type = std::vector<std::string>;
    using const_iterator = std::vector<std::string>::const_iterator;
    using iterator = std::vector<std::string>::iterator;
    using span_type = std::span<std::string>;

    // clang-format off
    std::vector<std::string> args_;

    Cmd() = default;

    template <typename... Ts> requires(std::convertible_to<Ts, std::string_view> && ...)
    explicit Cmd(Ts &&...ts);

    auto push(std::string_view s) -> void;

    template <typename... Ts>
    auto emplace_b(Ts &&...ts) -> std::string &;

    template <typename... Ts>
    auto emplace(const_iterator it, Ts &&...ts) -> std::string &;

    auto begin(this auto &self) noexcept;
    auto end(this auto &self) noexcept;
    auto size(this auto const &self) noexcept -> std::size_t;
    auto empty(this auto const &self) noexcept -> bool;
    auto span(this auto &self) noexcept;

    [[nodiscard]] auto argv() const -> std::vector<char*>;
    [[nodiscard]] auto str() const -> std::string;
    auto reset() -> void;
    // clang-format on
};
} // namespace bld

// clang-format off
template <>
struct std::formatter<bld::Cmd>
{
    enum class mode { plain, unquoted, debug };
    mode fmt = mode::plain;
    constexpr auto parse(std::format_parse_context &ctx) -> std::format_parse_context::iterator;
    auto format(const bld::Cmd &cmd, std::format_context &ctx) const -> std::format_context::iterator;
};
// clang-format on

namespace bld {

enum class Open_mode { read, write, append };

struct Fd_view
{
    using Native_t = int;
    static constexpr Native_t INVALID = -1;
    static constexpr Native_t DEFAULT_IN = STDIN_FILENO;
    static constexpr Native_t DEFAULT_OUT = STDOUT_FILENO;
    static constexpr Native_t DEFAULT_ERR = STDERR_FILENO;

    Native_t val{INVALID};

    constexpr Fd_view() = default;
    explicit constexpr Fd_view(Native_t v);

    [[nodiscard]] constexpr auto is_valid() const noexcept -> bool;
};

struct Owned_Fd
{
    Fd_view::Native_t handle_{Fd_view::INVALID};

    constexpr Owned_Fd() = default;
    explicit constexpr Owned_Fd(Fd_view::Native_t v);

    ~Owned_Fd();

    Owned_Fd(const Owned_Fd &) = delete;
    Owned_Fd &operator=(const Owned_Fd &) = delete;

    Owned_Fd(Owned_Fd &&other) noexcept;
    Owned_Fd &operator=(Owned_Fd &&other) noexcept;

    auto close() -> void;

    operator Fd_view() const;

    [[nodiscard]]
    static auto open(std::string_view path, Open_mode mode = Open_mode::read) -> std::expected<Owned_Fd, bld::Err>;
};

// Shared ownership for eager file routing. Copyable and lifetime-safe:
// a Proc_config holding a Shared_fd keeps the fd open as long as any copy lives.
// Child processes dup2() the fd at spawn, so parent copies closing later is harmless.
using Shared_fd = std::shared_ptr<Owned_Fd>;

// Single-stream route: either unset (inherit), a borrowed fd, a file path
// (opened lazily at spawn time), a shared owned fd (eager, lifetime-safe),
// or a borrowed string target (out_str/err_str/out_err_str capture into it).
// One slot replaces the old fd+path field pairs, so conflicting routing
// for the same stream becomes unrepresentable.
using Io_slot = std::variant<std::monostate, Fd_view, std::string, Shared_fd, std::string*>;

[[nodiscard]] inline auto io_is_set(const Io_slot &slot) noexcept -> bool
{
    return !std::holds_alternative<std::monostate>(slot);
}

[[nodiscard]] inline auto io_fd(const Io_slot &slot) noexcept -> Fd_view
{
    if (auto *f = std::get_if<Fd_view>(&slot)) {
        return *f;
    }
    if (auto *s = std::get_if<Shared_fd>(&slot)) {
        if (s && *s) {
            return Fd_view{(*s)->handle_};
        }
    }
    return Fd_view{Fd_view::INVALID};
}

[[nodiscard]] inline auto io_path(const Io_slot &slot) noexcept -> std::string_view
{
    if (auto *p = std::get_if<std::string>(&slot)) {
        return std::string_view{*p};
    }
    return std::string_view{};
}

[[nodiscard]] inline auto io_shared(const Io_slot &slot) noexcept -> Shared_fd
{
    if (auto *s = std::get_if<Shared_fd>(&slot)) {
        return *s;
    }
    return nullptr;
}

[[nodiscard]] inline auto io_str(const Io_slot &slot) noexcept -> std::string *
{
    if (auto *s = std::get_if<std::string *>(&slot)) {
        return *s;
    }
    return nullptr;
}

// Normalize helpers: INVALID / std-fd / empty-path all mean inherit (monostate).
[[nodiscard]] inline auto make_fd_slot(Fd_view f, int std_fd) noexcept -> Io_slot
{
    if (!f.is_valid() || f.val == std_fd || f.val == Fd_view::INVALID) {
        return Io_slot{std::monostate{}};
    }
    return Io_slot{f};
}

[[nodiscard]] inline auto make_path_slot(std::string s) noexcept -> Io_slot
{
    if (s.empty()) {
        return Io_slot{std::monostate{}};
    }
    return Io_slot{std::move(s)};
}

// Borrowed string target for out_str/err_str/out_err_str. Null means inherit.
[[nodiscard]] inline auto make_str_slot(std::string *p) noexcept -> Io_slot
{
    if (p == nullptr) {
        return Io_slot{std::monostate{}};
    }
    return Io_slot{p};
}

// Fd-based routing: redirect via already-open file descriptors.
// Unset (INVALID) means inherit the parent's stream.
struct Fd_routing
{
    Fd_view in{Fd_view::INVALID};
    Fd_view out{Fd_view::INVALID};
    Fd_view err{Fd_view::INVALID};
    bool merge_err_and_out{false};
};

// String-based routing: redirect via file paths, opened lazily at spawn time
// (truncate for out/err, readonly for in). Use this when you have a path and
// don't want to manage an Owned_Fd yourself. Empty means unset.
struct String_routing
{
    std::string in;
    std::string out;
    std::string err;
    bool merge_err_and_out{false};
};

// Unified per-stream route: either unset, an fd, a file path, or a shared owned fd.
// This is the variant form combining Fd_routing and String_routing.
using Route_value = Io_slot;
struct Route
{
    Route_value in;
    Route_value out;
    Route_value err;
    bool merge_err_and_out{false};
};

struct Proc_config
{
    std::string label{""};
    /// Working directory for this child. Empty means inherit the build script's directory.
    std::string cwd{""};
    bool async{false};
    /// Log-only preview: when true, execute() logs what would run and never
    /// spawns (no cwd validation, no process, no side effects). Set via
    /// bld::dry_run{} on run(cmd) — or on the batch call for
    /// run(tasks/plan/db). Rejected on Task/run_new: dry-run is per run()
    /// call, never per task (run_new always spawns).
    bool dry_run{false};
    std::source_location loc{std::source_location::current()};
    // Unified routing: one slot per stream (unset = inherit).
    // Replaces the old fd+path field pairs.
    Io_slot io_in;
    Io_slot io_out;
    Io_slot io_err;
    bool merge_err_and_out{false};

    [[nodiscard]] auto fd_route() const -> Fd_routing
    {
        return {.in = io_fd(io_in), .out = io_fd(io_out), .err = io_fd(io_err), .merge_err_and_out = merge_err_and_out};
    }
    [[nodiscard]] auto str_route() const -> String_routing
    {
        auto sv = [](const Io_slot &s) -> std::string {
            auto v = io_path(s);
            return std::string{v};
        };
        return {.in = sv(io_in), .out = sv(io_out), .err = sv(io_err), .merge_err_and_out = merge_err_and_out};
    }
    [[nodiscard]] auto route() const -> Route
    {
        return {.in = io_in, .out = io_out, .err = io_err, .merge_err_and_out = merge_err_and_out};
    }
};

struct Cmd_loc
{
    const Cmd &cmd;
    std::source_location loc;
    Cmd_loc(const Cmd &c, std::source_location l = std::source_location::current());
};

template <typename T>
concept Config_modifier_c = requires(T &&modifier, Proc_config &cfg) { modifier(cfg); };

template <typename... Configs>
constexpr auto validate_run_configs() -> void;

struct Exec_spec
{
    Cmd cmd;
    Proc_config cfg;
};

struct Proc_gid
{
#ifdef _WIN32
    using Native_t = void *;
    static constexpr Native_t INVALID = nullptr;
#else
    using Native_t = int;
    static constexpr Native_t INVALID = -1;
    static constexpr Native_t CREATE_NEW = 0;
#endif
    Native_t val{INVALID};
    constexpr Proc_gid() = default;
    explicit constexpr Proc_gid(Native_t v) : val(v)
    {}
    [[nodiscard]] constexpr auto is_valid() const noexcept -> bool
    {
        return val != INVALID;
    }
    constexpr auto operator==(const Proc_gid &other) const -> bool
    {
        return val == other.val;
    }
    constexpr auto operator!=(const Proc_gid &other) const -> bool
    {
        return val != other.val;
    }
};

using Proc_id = std::uint64_t;
inline constexpr Proc_id INVALID_PROC_ID = 0;

struct Task
{
    std::string name;
    Exec_spec spec;
    // Local dependency info used only when run(span<Task>) is given
    // deduce_dependency. Plan keeps its own external maps; Task carries
    // its own here so a bare span can also form a graph.
    std::vector<std::string> inputs;
    std::vector<std::string> outputs;
    std::vector<std::string> after;

    Task() = default;
    template <typename... Configs>
    explicit Task(Cmd_loc cl, Configs &&...confs);

    auto needs(std::string_view input) -> Task &;
    auto needs_from(std::initializer_list<std::string_view> ins) -> Task &;
    auto produces(std::string_view output) -> Task &;
    auto produces_to(std::initializer_list<std::string_view> outs) -> Task &;
    auto after_dep(std::string_view dep) -> Task &;
};

struct Proc
{
    enum class State : ::std::uint8_t { running, exited, signaled, stopped, continued };

    struct Status
    {
        State state{State::exited};
        int code{EXIT_SUCCESS};
    };

#ifdef _WIN32
    using P_id = void *;
    P_id id_{nullptr};
#else
    using P_id = pid_t;
    P_id id_{-1};
#endif
    Status status_{};
    Exec_spec spec;
    Proc_gid gid{};

    // String capture (out_str/err_str/out_err_str): the parent holds the read
    // ends of pipes whose write ends are the child's stdout/stderr. The
    // scheduler pumps them single-threaded (poll / PeekNamedPipe) in
    // wait()/try_wait()/wait_any()/wait_all()/capture_execute — no reader
    // threads are spawned. Targets are borrowed, never owned; they are
    // complete once the child is reaped and EOF is drained.
    int cap_out_fd_{-1};
    int cap_err_fd_{-1};
    std::string *cap_out_{nullptr};
    std::string *cap_err_{nullptr};

    explicit Proc() = default;
    explicit Proc(P_id id, const std::string &label_ = "");
    explicit Proc(P_id id, Proc_gid gid_, const std::string &label_ = "");

    ~Proc();

    Proc(const Proc &) = delete;
    Proc &operator=(const Proc &) = delete;

    Proc(Proc &&other) noexcept;
    Proc &operator=(Proc &&other) noexcept;

    [[nodiscard]]
    auto wait() -> std::expected<Status, bld::Err>;

    [[nodiscard]]
    auto try_wait() -> std::expected<Status, bld::Err>;

    auto kill(int sig = SIGTERM) -> void;

    // clang-format off
    [[nodiscard]] auto pid() const -> P_id;
    [[nodiscard]] auto pgid() const -> Proc_gid;
    [[nodiscard]] auto status() const -> Status;
    [[nodiscard]] auto is_running() const -> bool;
    [[nodiscard]] auto status_code() const -> int;
    // clang-format on

    [[nodiscard]]
    static auto spawn(const Exec_spec &spec, Proc_gid gid = {}) -> std::expected<Proc, bld::Err>;
    [[nodiscard]]
    static auto spawn(const Task &task, Proc_gid gid = {}) -> std::expected<Proc, bld::Err>;

    [[nodiscard]]
    static auto wait_pid(P_id pid, int options = 0) -> std::expected<Status, bld::Err>;

    [[nodiscard]]
    static auto try_wait_pid(P_id pid) -> std::expected<Status, bld::Err>;

    static auto parse_status(int wstatus) -> Status;

    // Drains available capture bytes without blocking. Called repeatedly
    // while the child runs and once more after it is reaped. Public so
    // wait_all() and user schedulers can pump siblings single-threaded.
    auto pump_capture_nonblocking() -> void;
    // True when this Proc owns string-capture pipes.
    [[nodiscard]] auto has_capture() const noexcept -> bool;
    // Drain to EOF + close + CRLF-normalize the borrowed targets.
    // Idempotent: no-op once fds are -1. Only call after the child is
    // reaped (or observed exited).
    auto finish_capture() -> void;

private:
    friend class Proc_group;
    auto update_status(int wstatus) -> void;
    // Deprecated alias kept for source compat (no threads remain to join).
    auto join_drain() -> void;
#ifdef _WIN32
    // Fetches the Win32 exit code, closes the process handle, finishes string
    // capture and marks the handle null. Shared by wait()/try_wait().
    auto reap_win_process() -> void;
#endif
};

// Internal guided-error messages for modifier-category mistakes. static_assert needs a
// string literal, so these are macros; they are undefined after the last run/capture
// template definition below. Each is asserted INLINE in the caller body (not only inside
// validate_*): that way the helpful message prints first, ahead of any follow-on error.
#define B_LDR_PROC_MODIFIERS_MSG \
    "API ERROR: bad modifier for run(cmd)/Task/run_new. Proc modifiers: async, label, cwd, dry_run, pipe, " \
    "out_/err_/in_ fd/file/lazy, out_err_fd/file/lazy, out_str/err_str/out_err_str. " \
    "Run modifiers go to run(batch,...); in_str/raw_crlf go to capture()."
#define B_LDR_RUN_MODIFIERS_MSG \
    "API ERROR: bad modifier for run(batch). Run modifiers: jobs (alias use_threads), max_async, deduce_dependency, " \
    "keep_going, dry_run, force, write_compile_commands. Put io/label/cwd on the Task. " \
    "No run(span<Proc>): use wait_all(procs)."
#define B_LDR_CAPTURE_MODIFIERS_MSG \
    "API ERROR: bad modifier for capture(cmd). Capture takes: in_fd/in_file/lazy_in_file, in_str, label, " \
    "dry_run, raw_crlf. Output is always merged; to capture from run(), use run(cmd, out_str{s})."

// Forward declaration: the full dry_run modifier (a Run/Proc/Capture modifier)
// is defined with the run modifiers below. run_new's template needs the name
// for its dry_run rejection ahead of that definition.
struct dry_run;

class Proc_group
{
public:
    Proc_group();
    ~Proc_group();

    Proc_group(const Proc_group &) = delete;
    Proc_group &operator=(const Proc_group &) = delete;
    Proc_group(Proc_group &&other) noexcept;
    Proc_group &operator=(Proc_group &&other) noexcept;

    [[nodiscard]] auto gid() const -> Proc_gid;
    [[nodiscard]] auto empty() const -> bool;
    [[nodiscard]] auto size() const -> std::size_t;

    template <typename... Configs>
    auto run_new(Cmd_loc cl, Configs &&...confs) -> std::expected<Proc_id, Err>
    {
        static_assert((Config_modifier_c<Configs> && ...), B_LDR_PROC_MODIFIERS_MSG);
        static_assert(
            !(std::is_same_v<std::remove_cvref_t<Configs>, dry_run> || ...),
            "API ERROR: dry_run is per run() call, not per spawned proc (run_new always spawns); put dry_run{} on the run()/capture() call.");
        Exec_spec spec;
        spec.cmd = cl.cmd;
        spec.cfg.loc = cl.loc;
        spec.cfg.async = true;
        validate_run_configs<Configs...>();
        (confs(spec.cfg), ...);
        return run_new(spec);
    }
    auto run_new(const Task &task) -> std::expected<Proc_id, Err>;
    auto run_new(const Exec_spec &spec) -> std::expected<Proc_id, Err>;

    auto add(Proc &&proc) -> Proc_id;
    [[nodiscard]] auto get(Proc_id id) -> std::expected<std::reference_wrapper<Proc>, Err>;
    auto remove(Proc_id id) -> bool;
    [[nodiscard]] auto wait_any() -> std::expected<Proc_id, Err>;
    auto signal(int sig = SIGTERM) -> void;
    auto terminate(int sig = SIGTERM) -> void;

private:
    struct Slot
    {
        Proc_id id{0};
        Proc proc{};
    };
    Proc_gid gid_{};
    std::vector<Slot> hive_{};
    std::vector<std::uint32_t> free_list_{};
    std::uint32_t generation_{1};
    std::size_t active_count_{0};
};
} // namespace bld

// clang-format off
template <>
struct std::formatter<bld::Proc::Status>
{
    enum class mode { plain, debug };
    mode fmt = mode::plain;

    constexpr auto parse(std::format_parse_context& ctx) -> std::format_parse_context::iterator;
    auto format(const bld::Proc::Status& s, std::format_context& ctx) const -> std::format_context::iterator;
};

template <>
struct std::formatter<bld::Proc>
{
    bool show_pid   = false;
    bool show_debug = false;

    constexpr auto parse(std::format_parse_context& ctx) -> std::format_parse_context::iterator;
    auto format(const bld::Proc& p, std::format_context& ctx) const -> std::format_context::iterator;
};
// clang-format on

namespace bld {

struct Capture_config
{
    std::string label{""};
    std::source_location loc{std::source_location::current()};
    // Unified stdin routing: unset (inherit), borrowed fd, path, or shared owned fd.
    // Use in_fd / in_file / lazy_in_file. Setting in_str as well is an error.
    Io_slot io_in;
    std::string_view in_str{""};
    /// Captured output is normalized from CRLF to LF. Always on: Windows programs emit CRLF,
    /// so captured text compares cleanly against "\n"-terminated strings (a no-op on Linux).
    bool normalize_crlf{true};
    /// Log-only preview: when true, capture() logs what would run and
    /// returns an empty string without spawning. Set via bld::dry_run{}.
    bool dry_run{false};
};

// SECTION 04 — Execution (bld::run, bld::capture, bld::Task)
//   Declarations only. Definitions live in IMPL SECTION 04.
namespace details {
auto execute(const bld::Cmd &cmd, const Proc_config &cfg, std::source_location loc = std::source_location::current())
    -> std::expected<bld::Proc, bld::Err>;

// Validates a single task/proc config: working directory exists.
auto check_proc_config(const Proc_config &cfg) -> std::expected<void, bld::Err>;

// Cross-platform pipe helpers shared by capture_execute and Proc's
// string capture (out_str/err_str/out_err_str). fds use -1 as empty.
// All capture I/O is single-threaded (poll / PeekNamedPipe); no helper
// spawns threads.
auto make_pipe(int fds[2], const char *name) -> std::expected<void, bld::Err>;
auto close_fd(int fd) -> void;
auto read_fd(int fd, void *buf, unsigned int count) -> int;
auto write_fd(int fd, const void *buf, unsigned int count) -> int;
auto set_nonblocking(int fd) -> void;
// Drains whatever is available on fd into out without blocking.
// Returns true when EOF/fatal is observed (caller should close fd and
// treat it as done); false means keep polling (including EAGAIN).
auto pump_fd_nonblocking(int fd, std::string &out) -> bool;
// Milliseconds sleep used to avoid busy-spinning while waiting for a
// child whose capture pipes have no data ready.
auto sleep_ms(int ms) -> void;

// Always captures merged stdout+stderr into one string.
// Returns the merged output on exit-code 0; on non-zero exit returns an Err
// with the merged output attached as std::string payload.
auto capture_execute(const bld::Cmd &cmd, bld::Capture_config &cap_cfg, std::source_location loc = std::source_location::current())
    -> std::expected<std::string, bld::Err>;
} // namespace details

// clang-format off
struct async
{
    auto operator()(Proc_config &cfg) const -> void;
};
struct label
{
    std::string val{""};
    auto operator()(bld::Proc_config &cfg) const -> void;
    auto operator()(bld::Capture_config &cfg) const -> void;
};
struct pipe
{
    bld::Fd_view out{bld::Fd_view::DEFAULT_OUT}, in{bld::Fd_view::DEFAULT_IN}, err{bld::Fd_view::DEFAULT_ERR};
    bool merge_err_and_out{false};
    auto operator()(Proc_config& cfg) const -> void;
};
struct out_fd
{
    bld::Fd_view fd{bld::Fd_view::INVALID};
    auto operator()(bld::Proc_config& cfg) const -> void;
};
struct err_fd
{
    bld::Fd_view fd{bld::Fd_view::INVALID};
    auto operator()(bld::Proc_config& cfg) const -> void;
};
struct in_fd
{
    bld::Fd_view fd{bld::Fd_view::INVALID};
    auto operator()(bld::Proc_config& cfg) const -> void;
    auto operator()(bld::Capture_config& cfg) const -> void;
};
// One borrowed fd for merged stdout+stderr (implies merging, like out_err_file).
struct out_err_fd
{
    bld::Fd_view fd{bld::Fd_view::INVALID};
    auto operator()(bld::Proc_config& cfg) const -> void;
};
struct out_file
{
    // Shared ownership: copying out_file (or the Proc_config it fills)
    // keeps the fd open. See Shared_fd / Io_slot.
    //
    // Two ways to build one:
    //   bld::out_file{"log.txt"}            opens now, fatal (log + exit) on failure.
    //   bld::out_file::open("log.txt")      opens now, returns expected for manual handling:
    //     if (auto f = bld::out_file::open("log.txt"); !f) { /* f.error() */ }
    //     else if (auto proc = bld::run(cmd, *f); !proc) { /* ... */ }
    // (*f unwraps the expected to the modifier run() takes.)
    bld::Shared_fd fd{};
    out_file() = default;
    out_file(std::string_view path, bld::Open_mode mode = bld::Open_mode::write);
    [[nodiscard]] static auto open(std::string_view path, bld::Open_mode mode = bld::Open_mode::write)
        -> std::expected<bld::out_file, bld::Err>;
    auto operator()(bld::Proc_config& cfg) const -> void;
};
struct err_file
{
    // Same two forms as out_file: direct ctor (fatal on failure) or
    // open() + *f for manual error handling.
    bld::Shared_fd fd{};
    err_file() = default;
    err_file(std::string_view path, bld::Open_mode mode = bld::Open_mode::write);
    [[nodiscard]] static auto open(std::string_view path, bld::Open_mode mode = bld::Open_mode::write)
        -> std::expected<bld::err_file, bld::Err>;
    auto operator()(bld::Proc_config& cfg) const -> void;
};
struct in_file
{
    // Same two forms as out_file: direct ctor (fatal on failure) or
    // open() + *f for manual error handling.
    bld::Shared_fd fd{};
    in_file() = default;
    in_file(std::string_view path);
    [[nodiscard]] static auto open(std::string_view path) -> std::expected<bld::in_file, bld::Err>;
    auto operator()(bld::Proc_config& cfg) const -> void;
    auto operator()(bld::Capture_config& cfg) const -> void;
};
struct out_err_file
{
    // Same two forms as out_file: direct ctor (fatal on failure) or
    // open() + *f for manual error handling.
    bld::Shared_fd fd{};
    out_err_file() = default;
    out_err_file(std::string_view path, bld::Open_mode mode = bld::Open_mode::write);
    [[nodiscard]] static auto open(std::string_view path, bld::Open_mode mode = bld::Open_mode::write)
        -> std::expected<bld::out_err_file, bld::Err>;
    auto operator()(bld::Proc_config& cfg) const -> void;
};
struct in_str
{
    std::string_view val;
    explicit in_str(std::string_view s);
    auto operator()(Capture_config& cfg) const -> void;
};
// Opened late during execution and closed by then only
struct lazy_out_file
{
    std::string path;
    explicit lazy_out_file(std::string_view p) : path(p) {}
    auto operator()(bld::Proc_config& cfg) const -> void;
};
struct lazy_err_file
{
    std::string path;
    explicit lazy_err_file(std::string_view p) : path(p) {}
    auto operator()(bld::Proc_config& cfg) const -> void;
};
struct lazy_out_err_file
{
    std::string path;
    explicit lazy_out_err_file(std::string_view p) : path(p) {}
    auto operator()(bld::Proc_config& cfg) const -> void;
};
struct lazy_in_file
{
    std::string path;
    explicit lazy_in_file(std::string_view p) : path(p) {}
    auto operator()(bld::Proc_config& cfg) const -> void;
    auto operator()(bld::Capture_config& cfg) const -> void;
};
// Capture a stream into a std::string via bld::run (NOT bld::capture, which is
// merged-only). The string is borrowed and must outlive the wait()/reap.
// out_str: stdout only (stderr inherits, never merged). err_str: stderr only.
// out_err_str: merged stdout+stderr (implies merging, like out_err_file).
struct out_str
{
    std::string *ptr{nullptr};
    explicit out_str(std::string &s) : ptr(&s)
    {}
    auto operator()(bld::Proc_config &cfg) const -> void;
};
struct err_str
{
    std::string *ptr{nullptr};
    explicit err_str(std::string &s) : ptr(&s)
    {}
    auto operator()(bld::Proc_config &cfg) const -> void;
};
struct out_err_str
{
    std::string *ptr{nullptr};
    explicit out_err_str(std::string &s) : ptr(&s)
    {}
    auto operator()(bld::Proc_config &cfg) const -> void;
};
/// Opts out of the default CRLF -> LF normalization of captured output.
struct raw_crlf
{
    auto operator()(Capture_config& cfg) const -> void;
};
/// Executes a command in `path` without changing the build script's own directory.
struct cwd
{
    std::string path;
    explicit cwd(std::string_view p) : path(p) {}
    auto operator()(bld::Proc_config &cfg) const -> void;
};

template <typename T>
constexpr bool is_out_mod_v = 
    std::is_same_v<std::remove_cvref_t<T>, out_fd> || 
    std::is_same_v<std::remove_cvref_t<T>, out_file> || 
    std::is_same_v<std::remove_cvref_t<T>, lazy_out_file> || 
    std::is_same_v<std::remove_cvref_t<T>, out_err_file> ||
    std::is_same_v<std::remove_cvref_t<T>, lazy_out_err_file> ||
    std::is_same_v<std::remove_cvref_t<T>, out_err_fd> ||
    std::is_same_v<std::remove_cvref_t<T>, out_str> ||
    std::is_same_v<std::remove_cvref_t<T>, out_err_str>;

template <typename T>
constexpr bool is_err_mod_v = 
    std::is_same_v<std::remove_cvref_t<T>, err_fd> || 
    std::is_same_v<std::remove_cvref_t<T>, err_file> || 
    std::is_same_v<std::remove_cvref_t<T>, lazy_err_file> || 
    std::is_same_v<std::remove_cvref_t<T>, out_err_file> ||
    std::is_same_v<std::remove_cvref_t<T>, lazy_out_err_file> ||
    std::is_same_v<std::remove_cvref_t<T>, out_err_fd> ||
    std::is_same_v<std::remove_cvref_t<T>, err_str> ||
    std::is_same_v<std::remove_cvref_t<T>, out_err_str>;

template <typename T>
constexpr bool is_in_mod_v = 
    std::is_same_v<std::remove_cvref_t<T>, in_fd> || 
    std::is_same_v<std::remove_cvref_t<T>, in_file> || 
    std::is_same_v<std::remove_cvref_t<T>, lazy_in_file> || 
    std::is_same_v<std::remove_cvref_t<T>, in_str>;

template <typename T>
constexpr bool is_batch_mod_v = 
    std::is_same_v<std::remove_cvref_t<T>, pipe>;

template <typename T>
constexpr bool is_cap_in_mod_v = is_in_mod_v<T>;

template <typename T>
constexpr bool is_async_mod_v = std::is_same_v<std::remove_cvref_t<T>, async>;

template <typename T>
constexpr bool is_label_mod_v = std::is_same_v<std::remove_cvref_t<T>, label>;

template <typename T>
constexpr bool is_cwd_mod_v = std::is_same_v<std::remove_cvref_t<T>, cwd>;
// clang-format on

template <typename... Configs>
auto run(Cmd_loc cl, Configs &&...confs) -> std::expected<bld::Proc, bld::Err>;

template <typename T>
concept Capture_modifier_c = requires(T &&modifier, Capture_config &cfg) { modifier(cfg); };

template <typename... Configs>
constexpr auto validate_capture_configs() -> void;

template <typename... Configs>
// Merged-only capture: returns merged stdout+stderr as a string on success (exit 0).
// Takes no out_* modifiers — only in_fd/in_file/lazy_in_file/in_str, label, raw_crlf.
auto capture(Cmd_loc cl, Configs &&...confs) -> std::expected<std::string, bld::Err>;

auto wait_all(std::span<bld::Proc> procs) -> std::expected<std::size_t, bld::Err>;

// Guard: wait_all takes no modifiers. Any extra argument selects this overload
// and fails with a message instead of "too many arguments" / "no match".
template <typename... Ts>
auto wait_all(std::span<bld::Proc> procs, Ts &&...) -> std::expected<std::size_t, bld::Err>
{
    static_assert(
        sizeof...(Ts) == 0,
        "API ERROR: wait_all(procs) takes no modifiers. Put io/label/cwd on the Task at spawn; run flags on run(batch,...).");
    return wait_all(procs);
}

struct Plan
{
    std::vector<Task> tasks;
    // Plan owns the graph — Task carries name + Exec_spec, Plan manages deps
    std::unordered_map<std::string, std::vector<std::string>> task_inputs;
    std::unordered_map<std::string, std::vector<std::string>> task_outputs;
    std::unordered_map<std::string, std::vector<std::string>> task_after;
    std::unordered_set<std::string> compile_commands;

    auto add(Task task) -> Task &;
    auto add(std::string name, Cmd cmd) -> Task &;
    auto add(std::string name, Exec_spec spec) -> Task &;

    auto needs(std::string_view task, std::string_view input) -> void;
    auto needs_from(std::string_view task, std::initializer_list<std::string_view> inputs) -> void;
    auto produces(std::string_view task, std::string_view output) -> void;
    auto produces_to(std::string_view task, std::initializer_list<std::string_view> outputs) -> void;
    auto after(std::string_view task, std::string_view dep) -> void;
    auto mark_compile_command(std::string_view task) -> void;
};

enum class Task_state { pending, running, skipped, succeeded, failed, cancelled };
struct Task_result
{
    Task_state state{Task_state::pending};
    bld::Proc::Status status{};
    std::chrono::milliseconds elapsed{};
    std::string message;
};
struct Run_result
{
    std::vector<bld::Task_result> tasks;
    std::size_t ran{0};
    std::size_t skipped{0};
    std::size_t failed{0};
    [[nodiscard]] auto ok() const noexcept -> bool
    {
        return failed == 0;
    }
};

enum class Failure_policy { stop, keep_going };
struct Run_config
{
    // Concurrency budget for the scheduler (scheduling width): max live
    // child processes. The scheduler itself is single-threaded — it spawns
    // processes (fork/CreateProcess) and reaps them via Proc_group::wait_any.
    // No worker threads are spawned; "threads" in older names means this width.
    //  nullopt       => max_parallel_count() - 1 (leave one core free), clamped >= 1.
    //  value <= 0    => max_parallel_count() + value, clamped >= 1 (0 => max).
    //  value  > 0    => min(value, max_parallel_count()).
    std::optional<int> use_threads{std::nullopt};
    // Cap on concurrently running async child processes (Proc_group size).
    //  0 => follow resolved parallel width (coupled default, old behaviour).
    //  >0 => absolute cap (may exceed CPU count for I/O-bound procs).
    std::size_t max_async{0};
    Failure_policy failure_policy{Failure_policy::stop};
    bool dry_run{false};
    bool force{false};
    std::string write_compile_commands;
    // For run(span<Task>): false => run all tasks independently;
    // true => build a dependency graph from Task.inputs/outputs/after.
    bool deduce_dependency{false};
};
// Canonical name: number of CPUs available for parallel child processes.
// max_thread_count() is kept as a deprecated alias (it never counted threads).
// NOTE: <thread> is implementation-only; this calls details::cpu_count()
// defined behind B_LDR_IMPLEMENTATION so includers don't parse <thread>.
namespace details {
[[nodiscard]] auto cpu_count() noexcept -> std::size_t;
} // namespace details
[[nodiscard]] inline auto max_parallel_count() -> std::size_t
{
    std::size_t m = details::cpu_count();
    return m == 0 ? 1 : m;
}
[[nodiscard]] inline auto max_thread_count() -> std::size_t
{
    return max_parallel_count();
}
// Canonical name for resolving the concurrency width. resolve_thread_count()
// is kept as a deprecated alias.
[[nodiscard]] inline auto resolve_parallel_width(std::optional<int> jobs_val) -> std::size_t
{
    std::size_t max_procs = max_parallel_count();
    int v = jobs_val.value_or(-1);
    if (v <= 0) {
        long resolved = static_cast<long>(max_procs) + static_cast<long>(v);
        if (resolved < 1) {
            resolved = 1;
        }
        return static_cast<std::size_t>(resolved);
    }
    std::size_t want = static_cast<std::size_t>(v);
    return want < max_procs ? want : max_procs;
}
[[nodiscard]] inline auto resolve_thread_count(std::optional<int> use_threads_val) -> std::size_t
{
    return resolve_parallel_width(use_threads_val);
}
[[nodiscard]] inline auto resolve_async_cap(std::size_t max_async_val, std::size_t resolved_width) -> std::size_t
{
    if (max_async_val == 0) {
        return resolved_width == 0 ? 1 : resolved_width;
    }
    return max_async_val;
}
// Canonical run modifier: cap on concurrently running child processes.
// `use_threads` is kept as a deprecated alias for source compat.
struct jobs
{
    std::optional<int> value{std::nullopt};
    jobs() = default;
    explicit jobs(int v) : value(v)
    {}
    explicit jobs(std::optional<int> v) : value(v)
    {}
    auto operator()(Run_config &cfg) const -> void;
};
struct use_threads : jobs
{
    using jobs::jobs;
};
struct max_async
{
    std::size_t value{0};
    explicit max_async(std::size_t v) : value(v)
    {}
    auto operator()(Run_config &cfg) const -> void;
};
struct deduce_dependency
{
    auto operator()(Run_config &cfg) const -> void;
};
struct keep_going
{
    auto operator()(Run_config &cfg) const -> void;
};
struct dry_run
{
    auto operator()(Run_config &cfg) const -> void;
    auto operator()(Proc_config &cfg) const -> void;
    auto operator()(Capture_config &cfg) const -> void;
};
struct force
{
    auto operator()(Run_config &cfg) const -> void;
};
struct write_compile_commands
{
    std::string path;
    explicit write_compile_commands(std::string_view p) : path(p)
    {}
    auto operator()(Run_config &cfg) const -> void;
};

struct Compilation_database
{
    std::string path;
    bool infer_outputs{true};
};
[[nodiscard]] inline auto compile_commands(std::string_view path, bool infer_outputs = true) -> Compilation_database
{
    return Compilation_database{std::string{path}, infer_outputs};
}

namespace details {
auto run_tasks(std::span<bld::Task> tasks, const bld::Run_config &cfg) -> std::expected<bld::Run_result, bld::Err>;
auto run_plan(Plan &plan, const bld::Run_config &cfg) -> std::expected<bld::Run_result, bld::Err>;
auto load_compile_commands(const bld::Compilation_database &database) -> std::expected<std::vector<bld::Task>, bld::Err>;
} // namespace details
template <typename T>
concept Run_modifier_c = requires(T &&modifier, Run_config &cfg) { modifier(cfg); };

template <typename T>
constexpr bool is_jobs_mod_v = std::is_same_v<std::remove_cvref_t<T>, jobs> || std::is_same_v<std::remove_cvref_t<T>, use_threads>;
template <typename T>
constexpr bool is_threads_mod_v = is_jobs_mod_v<T>;
template <typename T>
constexpr bool is_max_async_mod_v = std::is_same_v<std::remove_cvref_t<T>, max_async>;
template <typename T>
constexpr bool is_keep_going_mod_v = std::is_same_v<std::remove_cvref_t<T>, keep_going>;
template <typename T>
constexpr bool is_dry_run_mod_v = std::is_same_v<std::remove_cvref_t<T>, dry_run>;
template <typename T>
constexpr bool is_force_mod_v = std::is_same_v<std::remove_cvref_t<T>, force>;
template <typename T>
constexpr bool is_deduce_mod_v = std::is_same_v<std::remove_cvref_t<T>, deduce_dependency>;
template <typename T>
constexpr bool is_write_db_mod_v = std::is_same_v<std::remove_cvref_t<T>, write_compile_commands>;
template <typename... Options>
constexpr auto validate_run_options() -> void
{
    // NOTE: modifier-category is asserted inline by every caller
    // (run span/Plan/db) via B_LDR_RUN_MODIFIERS_MSG.
    constexpr int jobs_count = (is_jobs_mod_v<Options> + ... + 0);
    constexpr int async_count = (is_max_async_mod_v<Options> + ... + 0);
    constexpr int keep_going_count = (is_keep_going_mod_v<Options> + ... + 0);
    constexpr int dry_run_count = (is_dry_run_mod_v<Options> + ... + 0);
    constexpr int force_count = (is_force_mod_v<Options> + ... + 0);
    constexpr int deduce_count = (is_deduce_mod_v<Options> + ... + 0);
    constexpr int write_db_count = (is_write_db_mod_v<Options> + ... + 0);
    static_assert(jobs_count <= 1, "API ERROR: duplicate jobs/use_threads (at most one per run).");
    static_assert(async_count <= 1, "API ERROR: duplicate max_async (at most one per run).");
    static_assert(keep_going_count <= 1, "API ERROR: duplicate keep_going (at most one per run).");
    static_assert(dry_run_count <= 1, "API ERROR: duplicate dry_run (at most one per run).");
    static_assert(force_count <= 1, "API ERROR: duplicate force (at most one per run).");
    static_assert(deduce_count <= 1, "API ERROR: duplicate deduce_dependency (at most one per run).");
    static_assert(write_db_count <= 1, "API ERROR: duplicate write_compile_commands (at most one per run).");
}
// NOTE: io routing and friends are per-task (Proc_config) modifiers, enforced by
// static_assert inside validate_run_options: passing them to a batch run fails with
// a message pointing at the Task. Likewise run(span<Proc>) does not exist — use
// wait_all(span<Proc>), which takes no modifiers (extra args fail, see below).
template <typename... Options>
auto run(std::span<bld::Task> tasks, Options &&...options) -> std::expected<bld::Run_result, bld::Err>
{
    static_assert((Run_modifier_c<Options> && ...), B_LDR_RUN_MODIFIERS_MSG);
    validate_run_options<Options...>();
    Run_config cfg{};
    (options(cfg), ...);
    return details::run_tasks(tasks, cfg);
}
template <typename... Options>
auto run(Plan &plan, Options &&...options) -> std::expected<bld::Run_result, bld::Err>
{
    static_assert((Run_modifier_c<Options> && ...), B_LDR_RUN_MODIFIERS_MSG);
    validate_run_options<Options...>();
    Run_config cfg{};
    (options(cfg), ...);
    return details::run_plan(plan, cfg);
}
template <typename... Options>
auto run(const Compilation_database &database, Options &&...options) -> std::expected<bld::Run_result, bld::Err>
{
    static_assert((Run_modifier_c<Options> && ...), B_LDR_RUN_MODIFIERS_MSG);
    validate_run_options<Options...>();
    auto tasks = details::load_compile_commands(database);
    if (!tasks) {
        return std::unexpected(tasks.error());
    }
    Run_config cfg{};
    (options(cfg), ...);
    return details::run_tasks(std::span<bld::Task>{*tasks}, cfg);
}
} // namespace bld

// SECTION 05 — Rebuild helpers (is_outdated, rebuild_this_when_needed)
//   Declarations only. Definitions live in IMPL SECTION 05.
namespace bld {
[[nodiscard]]
auto is_outdated(std::string_view target, std::string_view source) -> bool;

template <std::ranges::range Range>
    requires std::convertible_to<std::ranges::range_value_t<Range>, std::string_view>
[[nodiscard]] auto is_outdated(std::string_view target, const Range &sources) -> bool;

auto get_current_cxx_compiler() -> std::string_view;

/// If the build script's own source is newer than the running executable, recompiles it and
/// re-executes it in its place (does not return on success). On failure, returns an Err so the
/// caller can decide how to handle it instead of the library calling std::exit.
auto rebuild_this_when_needed(int argc, char **argv, std::string_view compiler = "", std::source_location loc = std::source_location::current())
    -> std::expected<void, bld::Err>;

/// Same as rebuild_this_when_needed, but also tracks b_ldr.hpp itself and accepts extra compiler flags.
auto rebuild_this_when_needed_ext(
    int argc,
    char **argv,
    std::vector<std::string> flags = {},
    std::string_view compiler = "",
    std::source_location loc = std::source_location::current()) -> std::expected<void, bld::Err>;

} // namespace bld

// SECTION 06 — Config (bld::Config)
//   Declarations only. Definitions live in IMPL SECTION 06.
namespace bld {
class Config
{
public:
    enum val_t { Bool = 0, Int = 1, Double = 2, String = 3, String_arr = 4 };
    using value_type = std::variant<bool, int, double, std::string, std::vector<std::string>>;

    struct Option
    {
        val_t type;
        std::string description;
        value_type default_val;
        std::vector<std::string> choices{};
    };

    std::unordered_map<std::string, value_type> data{};
    std::flat_map<std::string, Option> options{};

    static auto get() -> Config &;
    Config(const Config &) = delete;
    Config &operator=(const Config &) = delete;
    Config(Config &&) = delete;
    Config &operator=(Config &&) = delete;

    auto add_option(std::string_view flag, val_t type, std::string_view desc, value_type def = false, std::vector<std::string> valid_choices = {})
        -> Config &;

    struct Proxy
    {
        const Config *cfg;
        std::string key;

        operator bool() const;
        operator std::string() const;
        operator int() const;
        operator double() const;
        operator std::vector<std::string>() const;
    };
    auto operator[](std::string_view key) const -> Proxy;

    /// Strips leading '-' characters so "jobs", "-jobs", "--jobs" are one key.
    static auto normalize_key(std::string_view key) -> std::string;

    /// Result of parse(). When help_requested is true, the caller should exit with EXIT_SUCCESS.
    struct Parse_outcome
    {
        bool help_requested{false};
    };

    auto print_help(std::string_view prog_name, std::string_view specific_opt = "") const -> void;
    /// Parses command line arguments. Never exits; on --help prints help and returns help_requested,
    /// on malformed input returns an Err and leaves the caller free to decide what to do.
    auto parse(int argc, char *argv[]) -> std::expected<Parse_outcome, bld::Err>;
    template <typename T>
    auto get_val(std::string_view key) const -> std::expected<T, bld::Err>;

private:
    Config() = default;
};
} // namespace bld

template <>
struct std::formatter<std::unordered_map<std::string, bld::Config::value_type>>
{
    constexpr auto parse(std::format_parse_context &ctx) -> std::format_parse_context::iterator;
    auto format(const std::unordered_map<std::string, bld::Config::value_type> &m, std::format_context &ctx) const
        -> std::format_context::iterator;
};

// SECTION 07 — Test helpers (bld::test)
//   Declarations only. Definitions live in IMPL SECTION 07.
namespace bld::test {

// clang-format off
enum class Edit_type : std::uint8_t { keep, insert, remove };
struct Edit
{
    Edit_type type;
    std::string_view content;
};
struct Frontier
{
    std::vector<std::ptrdiff_t> data;
    std::ptrdiff_t offset;
    explicit Frontier(std::ptrdiff_t max_d);
    auto operator[](std::ptrdiff_t k) -> std::ptrdiff_t &;
    auto operator[](std::ptrdiff_t k) const -> std::ptrdiff_t;
};
struct compute_diff_op
{
    bool same{false};
    std::vector<Edit> edits;
    operator bool() const;
};
// clang-format on

auto compute_diff(std::span<const std::string_view> original, std::span<const std::string_view> updated) -> compute_diff_op;
auto split_lines(std::string_view text) -> std::vector<std::string_view>;
auto compute_diff(std::initializer_list<std::string_view> original, std::initializer_list<std::string_view> updated) -> compute_diff_op;
auto compute_diff(std::string_view original, std::string_view updated) -> compute_diff_op;
} // namespace bld::test

template <>
struct std::formatter<bld::test::compute_diff_op>
{
    bool use_color = true;

    constexpr auto parse(std::format_parse_context &ctx) -> std::format_parse_context::iterator;
    auto format(const bld::test::compute_diff_op &diff, std::format_context &ctx) const -> std::format_context::iterator;
};

// SECTION 08 — Filesystem (bld::fs)
//   Declarations only. Definitions live in IMPL SECTION 08.
namespace bld::fs {
auto make_dir_if_not_exists(std::string_view path, bool create_parents = true, std::source_location loc = std::source_location::current()) noexcept
    -> bool;

struct Dir_entry
{
    std::filesystem::path path;
    std::filesystem::file_type type;
    int depth{0};

    [[nodiscard]] bool is_file() const noexcept;
    [[nodiscard]] bool is_dir() const noexcept;
    [[nodiscard]] bool is_symlink() const noexcept;
    [[nodiscard]] bool is_hidden() const noexcept;

    [[nodiscard]] std::string extension() const noexcept;
    [[nodiscard]] std::string stem() const noexcept;
    [[nodiscard]] std::string filename() const noexcept;
    [[nodiscard]] std::string parent() const noexcept;
};

enum class Walk_action { next, skip_dir, stop };

struct Walk_result
{
    Walk_action action = Walk_action::next;
    std::error_code error = {};

    constexpr Walk_result() = default;
    constexpr Walk_result(Walk_action a) noexcept;
    constexpr Walk_result(std::error_code ec) noexcept;
};

struct Walk_error
{
    enum class Kind { fs, visitor } kind;
    std::error_code code;

    [[nodiscard]] bool is_fs_error() const noexcept;
    [[nodiscard]] bool is_visitor_error() const noexcept;
    [[nodiscard]] std::string message() const;
};

template <typename T>
using Walk_result_t = std::expected<T, Walk_error>;

namespace detail {
template <typename V>
concept Void_visitor = std::invocable<V, const Dir_entry &> && std::same_as<std::invoke_result_t<V, const Dir_entry &>, void>;
template <typename V>
concept Result_visitor = std::invocable<V, const Dir_entry &> && std::convertible_to<std::invoke_result_t<V, const Dir_entry &>, Walk_result>;
template <typename V>
concept Valid_visitor = Void_visitor<V> || Result_visitor<V>;

template <Valid_visitor V>
auto invoke_visitor(V &&v, const Dir_entry &e) -> Walk_result;
} // namespace detail

class Dir_walker
{
public:
    explicit Dir_walker(std::string_view root);
    explicit Dir_walker(std::filesystem::path root);
    explicit Dir_walker(const std::string &root);
    explicit Dir_walker(const char *root);

    [[nodiscard]] auto recursive(bool v = true) noexcept -> Dir_walker &;
    [[nodiscard]] auto flat() noexcept -> Dir_walker &;
    [[nodiscard]] auto files_only(bool v = true) noexcept -> Dir_walker &;
    [[nodiscard]] auto exclude_dirs(bool v = true) noexcept -> Dir_walker &;
    [[nodiscard]] auto include_hidden(bool v = true) noexcept -> Dir_walker &;
    [[nodiscard]] auto follow_symlinks(bool v = true) noexcept -> Dir_walker &;
    [[nodiscard]] auto max_depth(int v) noexcept -> Dir_walker &;

    [[nodiscard]] auto ext(std::string e) -> Dir_walker &;
    [[nodiscard]] auto ext(std::initializer_list<std::string_view> exts) -> Dir_walker &;
    [[nodiscard]] auto named(std::string name) -> Dir_walker &;
    [[nodiscard]] auto skip(std::string dir_name) -> Dir_walker &;
    [[nodiscard]] auto skip(std::initializer_list<std::string_view> dir_names) -> Dir_walker &;

    template <typename Pred>
        requires std::predicate<Pred, const Dir_entry &>
    [[nodiscard]] auto where(Pred pred) -> Dir_walker &;
    template <detail::Valid_visitor V>
    [[nodiscard]] auto walk(V &&visitor, std::source_location caller = std::source_location::current()) const noexcept -> Walk_result_t<void>;
    [[nodiscard]] auto collect(std::source_location caller = std::source_location::current()) const noexcept -> Walk_result_t<std::vector<Dir_entry>>;
    [[nodiscard]]
    auto collect_paths(std::source_location caller = std::source_location::current()) const noexcept
        -> Walk_result_t<std::vector<std::filesystem::path>>;
    [[nodiscard]]
    auto count(std::source_location caller = std::source_location::current()) const noexcept -> Walk_result_t<std::size_t>;
    [[nodiscard]]
    auto any(std::source_location caller = std::source_location::current()) const noexcept -> Walk_result_t<bool>;
    [[nodiscard]]
    auto none(std::source_location caller = std::source_location::current()) const noexcept -> Walk_result_t<bool>;
    [[nodiscard]]
    auto first(std::source_location caller = std::source_location::current()) const noexcept -> Walk_result_t<std::optional<Dir_entry>>;
    [[nodiscard]]
    auto last(std::source_location caller = std::source_location::current()) const noexcept -> Walk_result_t<std::optional<Dir_entry>>;
    template <std::invocable<const Dir_entry &> F>
    [[nodiscard]]
    auto for_each(F &&fn, std::source_location caller = std::source_location::current()) const noexcept -> Walk_result_t<void>;
    template <typename Pred>
        requires std::predicate<Pred, const Dir_entry &>
    [[nodiscard]]
    auto partition(Pred pred, std::source_location caller = std::source_location::current()) const noexcept
        -> Walk_result_t<std::pair<std::vector<Dir_entry>, std::vector<Dir_entry>>>;
    template <typename T, std::invocable<T, const Dir_entry &> F>
    [[nodiscard]]
    auto fold(T init, F &&fn, std::source_location caller = std::source_location::current()) const noexcept -> Walk_result_t<T>;
    [[nodiscard]]
    auto subdir(std::string_view sub) const -> Dir_walker;
    template <std::invocable<Dir_walker &> F>
    [[nodiscard]] auto apply(F &&fn) -> Dir_walker &;

private:
    std::filesystem::path root_;
    bool recursive_ = true;
    bool include_dirs_ = false;
    bool include_hidden_ = false;
    bool follow_symlinks_ = false;
    int max_depth_ = std::numeric_limits<int>::max();
    std::vector<std::function<bool(const Dir_entry &)>> filters_;
    std::vector<std::string> skips_;

    [[nodiscard]] bool passes_filters(const Dir_entry &e) const;
    [[nodiscard]] bool is_skipped(const Dir_entry &e) const;
    [[nodiscard]] bool is_visible(const Dir_entry &e) const;
    static auto make_entry(const std::filesystem::directory_entry &raw, int d) -> Dir_entry;

    template <std::invocable<const Dir_entry &> F>
    [[nodiscard]]
    auto run(F &&fn, std::source_location caller) const noexcept -> Walk_result_t<void>;
};

template <detail::Valid_visitor V>
[[nodiscard]] inline auto walk(std::string_view root, V &&visitor, std::source_location caller = std::source_location::current()) noexcept;

[[nodiscard]] auto collect(std::string_view root, std::source_location caller = std::source_location::current()) noexcept
    -> Walk_result_t<std::vector<Dir_entry>>;

// "src/main.cpp" -> "main"
[[nodiscard]] auto stem(std::string_view path) noexcept -> std::string;
// "src/main.cpp" -> "main.cpp"
[[nodiscard]] auto name(std::string_view path) noexcept -> std::string;
// "src/main.cpp" -> ".cpp"
[[nodiscard]] auto extension(std::string_view path) noexcept -> std::string;
// "src/main.cpp" -> "src"
[[nodiscard]] auto parent_dir(std::string_view path) noexcept -> std::string;
[[nodiscard]] auto is_absolute(std::string_view path) noexcept -> bool;
[[nodiscard]] auto is_relative(std::string_view path) noexcept -> bool;

[[nodiscard]] auto exists(std::string_view path) noexcept -> bool;
[[nodiscard]] auto is_dir(std::string_view path) noexcept -> bool;
[[nodiscard]] auto is_file(std::string_view path) noexcept -> bool;
[[nodiscard]] auto is_symlink(std::string_view path) noexcept -> bool;
[[nodiscard]] auto is_empty(std::string_view path) noexcept -> std::expected<bool, bld::Err>;

// True when both paths resolve to the same file or directory, regardless of
// spelling (relative vs absolute, "./x" vs "x", symlinks, hard links).
// Missing/unresolvable paths report false, never throw.
[[nodiscard]] auto same_file(std::string_view a, std::string_view b) noexcept -> bool;

template <typename... Paths>
    requires(std::convertible_to<Paths, std::string_view> && ...)
[[nodiscard]] auto join(Paths &&...paths) -> std::string;

auto file_size(std::string_view path) noexcept -> std::expected<std::uintmax_t, bld::Err>;
auto last_write_time(std::string_view path) noexcept -> std::expected<std::filesystem::file_time_type, bld::Err>;

auto copy_file(std::string_view from, std::string_view to, bool overwrite = false) noexcept -> std::expected<void, bld::Err>;
auto rename(std::string_view from, std::string_view to) noexcept -> std::expected<void, bld::Err>;

auto create_symlink(std::string_view target, std::string_view link) noexcept -> std::expected<void, bld::Err>;
auto create_hard_link(std::string_view target, std::string_view link) noexcept -> std::expected<void, bld::Err>;
auto read_symlink(std::string_view path) noexcept -> std::expected<std::string, bld::Err>;

auto current_path() noexcept -> std::expected<std::string, bld::Err>;
auto set_current_path(std::string_view path) noexcept -> std::expected<void, bld::Err>;
auto absolute(std::string_view path) noexcept -> std::expected<std::string, bld::Err>;
auto canonical(std::string_view path) noexcept -> std::expected<std::string, bld::Err>;
auto relative(std::string_view path, std::string_view base) noexcept -> std::expected<std::string, bld::Err>;

auto read_file(std::string_view path) noexcept -> std::expected<std::string, bld::Err>;
auto write_file(std::string_view path, std::string_view content) noexcept -> std::expected<void, bld::Err>;
auto append_file(std::string_view path, std::string_view content) noexcept -> std::expected<void, bld::Err>;

template <typename... Paths>
    requires(std::convertible_to<Paths, std::string_view> && ...)
auto remove(Paths &&...paths) noexcept -> std::expected<void, bld::Err>;

template <typename... Paths>
    requires(std::convertible_to<Paths, std::string_view> && ...)
auto make_dirs(Paths &&...paths) noexcept -> std::expected<void, bld::Err>;

inline auto find_all_files(std::string_view root) noexcept -> Walk_result_t<std::vector<std::string>>;

template <typename... Exts>
    requires(std::convertible_to<Exts, std::string_view> && ...)
auto find_by_ext(std::string_view root, Exts &&...exts) noexcept -> Walk_result_t<std::vector<std::string>>;

template <typename... Names>
    requires(std::convertible_to<Names, std::string_view> && ...)
auto find_by_name(std::string_view root, Names &&...names) noexcept -> Walk_result_t<std::vector<std::string>>;

struct Cpp_module
{
    std::string name;
    std::filesystem::path file;
    std::vector<std::string> imports;
};

std::vector<Cpp_module> scan_modules(const std::string &path, std::vector<std::string> extensions = {"cppm", "ixx"});
} // namespace bld::fs

// SECTION 09 — String utilities (bld::str)
//   Declarations only. Definitions live in IMPL SECTION 09.
namespace bld::str {
[[nodiscard]] auto trim_left(std::string_view s) noexcept -> std::string_view;
[[nodiscard]] auto trim_right(std::string_view s) noexcept -> std::string_view;
[[nodiscard]] auto trim(std::string_view s) noexcept -> std::string_view;
[[nodiscard]] auto split(std::string_view s, char delimiter) -> std::vector<std::string_view>;
[[nodiscard]] auto split(std::string_view s, std::string_view delimiter) -> std::vector<std::string_view>;
[[nodiscard]] auto to_lower(std::string_view s) -> std::string;
[[nodiscard]] auto to_upper(std::string_view s) -> std::string;
[[nodiscard]] auto replace_all(std::string_view s, std::string_view from, std::string_view to) -> std::string;
[[nodiscard]] auto parse_int(std::string_view s, int base = 10) noexcept -> std::expected<int, bld::Err>;
[[nodiscard]] auto parse_double(std::string_view s) noexcept -> std::expected<double, bld::Err>;
[[nodiscard]] auto parse_bool(std::string_view s) noexcept -> std::expected<bool, bld::Err>;
template <std::ranges::range Range>
    requires std::convertible_to<std::ranges::range_value_t<Range>, std::string_view>
[[nodiscard]] auto join(const Range &range, std::string_view delimiter) -> std::string;
} // namespace bld::str

// SECTION 10 — Time (bld::time)
//   Declarations only. Definitions live in IMPL SECTION 10.
namespace bld::time {
struct stamp
{
    using clock_t = std::chrono::steady_clock;
    using time_point_t = clock_t::time_point;
    time_point_t tp_{clock_t::now()};
    stamp() = default;
    auto reset() noexcept -> std::chrono::nanoseconds;
    [[nodiscard]] auto elapsed() const noexcept -> std::chrono::nanoseconds;
    [[nodiscard]] auto since(const stamp &baseline) const noexcept -> std::chrono::nanoseconds;
    operator time_point_t() const noexcept
    {
        return tp_;
    }
};

[[nodiscard]] auto now() noexcept -> stamp;
[[nodiscard]] auto since(const stamp &baseline) noexcept -> std::chrono::nanoseconds;
[[nodiscard]] auto format(std::chrono::nanoseconds ns) -> std::string;
}; // namespace bld::time

// SECTION 11 — Environment (bld::env) — consteval + runtime, header-only, kept low
//   Declarations + definitions together (must be header-visible) except
//   runtime defs live in IMPL 11. Only portable if-constexpr-safe queries
//   + *_name() diagnostics. For non-portable OS/arch API use #ifdef.
//   Subsections: 11.1 Compiler (incl. *_name), 11.2 Arch/OS names,
//                11.3 Build, 11.4 Runtime (thread-safe)
// clang-format off
namespace bld::env {
[[nodiscard]] consteval bool is_clang() noexcept
{
#if defined(__clang__)
    return true;
#else
    return false;
#endif
}
[[nodiscard]] consteval bool is_gcc() noexcept
{
#if defined(__GNUC__) && !defined(__clang__)
    return true;
#else
    return false;
#endif
}
[[nodiscard]] consteval bool is_msvc() noexcept
{
#if defined(_MSC_VER)
    return true;
#else
    return false;
#endif
}

enum class Compiler { gcc, clang, msvc, unknown };

[[nodiscard]] consteval Compiler compiler() noexcept
{
    if (is_clang()) return Compiler::clang;
    if (is_gcc()) return Compiler::gcc;
    if (is_msvc()) return Compiler::msvc;
    return Compiler::unknown;
}

[[nodiscard]] consteval std::string_view compiler_name() noexcept
{
    if (is_clang()) return "clang";
    if (is_gcc()) return "gcc";
    if (is_msvc()) return "msvc";
    return "unknown";
}

[[nodiscard]] consteval int compiler_version_major() noexcept
{
#if defined(__clang__)
    return __clang_major__;
#elif defined(__GNUC__) && !defined(__clang__)
    return __GNUC__;
#elif defined(_MSC_VER)
    return _MSC_VER / 100;
#else
    return 0;
#endif
}
[[nodiscard]] consteval int compiler_version_minor() noexcept
{
#if defined(__clang__)
    return __clang_minor__;
#elif defined(__GNUC__) && !defined(__clang__)
    return __GNUC_MINOR__;
#elif defined(_MSC_VER)
    return _MSC_VER % 100;
#else
    return 0;
#endif
}
[[nodiscard]] consteval int compiler_version_patch() noexcept
{
#if defined(__clang__)
    return __clang_patchlevel__;
#elif defined(__GNUC__) && !defined(__clang__)
    return __GNUC_PATCHLEVEL__;
#elif defined(_MSC_VER)
#if defined(_MSC_FULL_VER)
    return _MSC_FULL_VER % 100000;
#else
    return 0;
#endif
#else
    return 0;
#endif
}

[[nodiscard]] consteval bool is_libstdcpp() noexcept
{
#if defined(__GLIBCXX__)
    return true;
#else
    return false;
#endif
}
[[nodiscard]] consteval bool is_libcpp() noexcept
{
#if defined(_LIBCPP_VERSION)
    return true;
#else
    return false;
#endif
}
[[nodiscard]] consteval bool is_msvc_stl() noexcept
{
#if defined(_MSVC_STL_VERSION) || (defined(_MSC_VER) && !defined(__clang__) && !defined(__GNUC__))
    return true;
#else
    return false;
#endif
}

[[nodiscard]] consteval std::string_view stdlib_name() noexcept
{
    if (is_libcpp()) return "libc++";
    if (is_libstdcpp()) return "libstdc++";
    if (is_msvc_stl()) return "msvc-stl";
    return "unknown";
}

[[nodiscard]] consteval long cxx_standard() noexcept
{
#if defined(_MSVC_LANG)
    return _MSVC_LANG;
#else
    return __cplusplus;
#endif
}

[[nodiscard]] consteval std::string_view cxx_standard_name() noexcept
{
    constexpr long v = cxx_standard();
    if (v >= 202302L) return "c++23";
    if (v >= 202002L) return "c++20";
    if (v >= 201703L) return "c++17";
    if (v >= 201402L) return "c++14";
    if (v >= 201103L) return "c++11";
    return "pre-c++11";
}

[[nodiscard]] consteval bool is_cpp11() noexcept { return cxx_standard() >= 201103L; }
[[nodiscard]] consteval bool is_cpp14() noexcept { return cxx_standard() >= 201402L; }
[[nodiscard]] consteval bool is_cpp17() noexcept { return cxx_standard() >= 201703L; }
[[nodiscard]] consteval bool is_cpp20() noexcept { return cxx_standard() >= 202002L; }
[[nodiscard]] consteval bool is_cpp23() noexcept { return cxx_standard() >= 202302L; }

// 11.2 — Arch & OS names (portable diagnostics) + portable queries
//   arch_name()/os_name() + is_32bit/is_64bit/endianness are safe for
//   if constexpr; for intrinsics like SSE/NEON still use #ifdef.

[[nodiscard]] consteval bool is_32bit() noexcept { return sizeof(void*) == 4; }
[[nodiscard]] consteval bool is_64bit() noexcept { return sizeof(void*) == 8; }

[[nodiscard]] consteval std::endian endian() noexcept
{
#if defined(__BYTE_ORDER__) && defined(__ORDER_LITTLE_ENDIAN__) && defined(__ORDER_BIG_ENDIAN__)
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    return std::endian::little;
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    return std::endian::big;
#else
    return std::endian::native;
#endif
#else
    return std::endian::native;
#endif
}
[[nodiscard]] consteval bool is_little_endian() noexcept { return endian() == std::endian::little; }
[[nodiscard]] consteval bool is_big_endian() noexcept { return endian() == std::endian::big; }

[[nodiscard]] consteval std::string_view os_name() noexcept
{
#if defined(_WIN32)
    return "windows";
#elif defined(__ANDROID__)
    return "android";
#elif defined(__APPLE__) && defined(TARGET_OS_IPHONE) && TARGET_OS_IPHONE
    return "ios";
#elif defined(__APPLE__) && defined(__MACH__)
    return "macos";
#elif defined(__FreeBSD__)
    return "freebsd";
#elif defined(__OpenBSD__)
    return "openbsd";
#elif defined(__NetBSD__)
    return "netbsd";
#elif defined(__DragonFly__)
    return "dragonfly";
#elif defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(__DragonFly__)
    return "bsd";
#elif defined(__linux__)
    return "linux";
#elif defined(__EMSCRIPTEN__)
    return "emscripten";
#elif defined(__wasm__) || defined(__wasm32__) || defined(__wasm64__)
    return "wasm";
#elif defined(__unix__)
    return "unix";
#elif defined(_POSIX_VERSION)
    return "posix";
#else
    return "unknown";
#endif
}

[[nodiscard]] consteval std::string_view arch_name() noexcept
{
#if defined(__x86_64__) || defined(__amd64__) || defined(_M_X64) || defined(_M_AMD64)
    return "x86_64";
#elif defined(__i386__) || defined(_M_IX86)
    return "x86";
#elif defined(__aarch64__) || defined(__arm64__) || defined(_M_ARM64) || defined(_M_ARM64EC)
    return "aarch64";
#elif defined(__arm__) || defined(_M_ARM)
    return "arm";
#elif defined(__riscv) && __riscv_xlen == 64
    return "riscv64";
#elif defined(__riscv) && __riscv_xlen == 32
    return "riscv32";
#elif defined(__riscv)
    return "riscv";
#elif defined(__powerpc64__) || defined(__ppc64__) || defined(__POWERPC64__)
    return "ppc64";
#elif defined(__powerpc__) || defined(__ppc__) || defined(_M_PPC)
    return "ppc";
#elif defined(__wasm__) || defined(__wasm32__) || defined(__wasm64__)
    return "wasm";
#else
    return "unknown";
#endif
}

// 11.4 — Runtime environment (thread-safe, for CC/CFLAGS etc.)
//   getenv/setenv are not thread-safe — every has/get/set/unset/get_all
//   locks the same global mutex, so concurrent read+write is safe.
//   No setup needed — mutex is a thread-safe Meyers singleton.

[[nodiscard]] auto has(std::string_view key) -> bool;
[[nodiscard]] auto get(std::string_view key) -> std::optional<std::string>;
[[nodiscard]] auto get_or(std::string_view key, std::string_view fallback) -> std::string;
[[nodiscard]] auto get_all() -> std::unordered_map<std::string, std::string>;

auto set(std::string_view key, std::string_view value, bool overwrite = true) -> std::expected<void, Err>;
auto unset(std::string_view key) -> std::expected<void, Err>;

} // namespace bld::env

// SECTION 12 — Formatters & inline templates (header-only)
//   Must stay in header: std::formatter specializations + constexpr/templates.
//   Search "SECTION 12" to find.
namespace std {
constexpr auto formatter<bld::Err>::parse(format_parse_context &ctx) -> format_parse_context::iterator
{
    auto it = ctx.begin();
    if (it != ctx.end() && *it != '}') {
        switch (*it) {
        case '?': fmt = mode::debug; break;
        case 'p': fmt = mode::plain; break;
        default: throw format_error("invalid Cmd format");
        }
        ++it;
    }
    return it;
}

constexpr auto formatter<bld::Cmd>::parse(format_parse_context &ctx) -> format_parse_context::iterator
{
    auto it = ctx.begin();
    if (it != ctx.end() && *it != '}') {
        switch (*it) {
        case 'q': fmt = mode::unquoted; break;
        case '?': fmt = mode::debug; break;
        case 'p': fmt = mode::plain; break;
        default: throw format_error("invalid Cmd format");
        }
        ++it;
    }
    return it;
}

constexpr auto formatter<bld::Proc::Status>::parse(format_parse_context &ctx) -> format_parse_context::iterator
{
    auto it = ctx.begin();
    if (it != ctx.end() && *it != '}') {
        switch (*it) {
        case 'p': fmt = mode::plain; break;
        case '?': fmt = mode::debug; break;
        default: throw format_error("invalid Proc::Status format specifier");
        }
        ++it;
    }
    return it;
}

constexpr auto formatter<bld::Proc>::parse(format_parse_context &ctx) -> format_parse_context::iterator
{
    auto it = ctx.begin();
    while (it != ctx.end() && *it != '}') {
        switch (*it) {
        case 'p': show_pid = true; break;
        case '?': show_debug = true; break;
        default: throw format_error("invalid Proc format specifier");
        }
        ++it;
    }
    return it;
}

constexpr auto formatter<unordered_map<string, bld::Config::value_type>>::parse(format_parse_context &ctx) -> format_parse_context::iterator
{
    auto it = ctx.begin();
    return it;
}

constexpr auto formatter<bld::test::compute_diff_op>::parse(format_parse_context &ctx) -> format_parse_context::iterator
{
    auto it = ctx.begin();
    auto end = ctx.end();
    if (it != end && *it != '}') {
        if (*it == 'n') {
            use_color = false; ++it;
        } else if (*it == 'c') {
            use_color = true; ++it;
        } else {
            throw format_error("invalid format specifier for compute_diff_op");
        }
    }
    if (it != end && *it != '}') {
        throw format_error("invalid format specifier for compute_diff_op");
    }
    return it;
}

} // namespace std
// clang-format on

namespace bld {

template <typename T>
auto Err::with_payload(T &&data) && -> Err
{
    payload = std::forward<T>(data);
    return std::move(*this);
}

constexpr auto Logger::Default_logger_fn::style(Level lvl) noexcept -> Style
{
    using namespace std::string_view_literals;
    switch (lvl) {
    case Level::dbg:
        return {"[DEBUG]", "\x1b[38;2;120;170;255m"sv};
    case Level::inf:
        return {"[INFO] ", "\x1b[38;2;0;200;120m"sv};
    case Level::wrn:
        return {"[WARN] ", "\x1b[38;2;255;180;0m"sv};
    case Level::err:
        return {"[ERROR]", "\x1b[38;2;255;64;64m"sv};
    case Level::ftl:
        return {"[FATAL]", "\x1b[38;2;200;0;0m"sv};
    }
    std::unreachable();
}

template <typename... Ts>
    requires(std::convertible_to<Ts, std::string_view> && ...)
Cmd::Cmd(Ts &&...ts)
{
    args_.reserve(sizeof...(Ts));
    (args_.emplace_back(std::forward<Ts>(ts)), ...);
}

template <typename... Ts>
auto Cmd::emplace_b(Ts &&...ts) -> std::string &
{
    return args_.emplace_back(std::forward<Ts>(ts)...);
}

template <typename... Ts>
auto Cmd::emplace(const_iterator it, Ts &&...ts) -> std::string &
{
    return args_.emplace(it, std::forward<Ts>(ts)...);
}

auto Cmd::begin(this auto &self) noexcept
{
    return self.args_.begin();
}
auto Cmd::end(this auto &self) noexcept
{
    return self.args_.end();
}
auto Cmd::size(this auto const &self) noexcept -> std::size_t
{
    return self.args_.size();
}
auto Cmd::empty(this auto const &self) noexcept -> bool
{
    return self.args_.empty();
}
auto Cmd::span(this auto &self) noexcept
{
    return std::span{self.args_};
}
constexpr Fd_view::Fd_view(Native_t v) : val(v)
{}
constexpr auto Fd_view::is_valid() const noexcept -> bool
{
    return val != INVALID;
}
constexpr Owned_Fd::Owned_Fd(Fd_view::Native_t v) : handle_(v)
{}
template <typename... Configs>
constexpr auto validate_run_configs() -> void
{
    // NOTE: modifier-category is asserted inline by every caller
    // (run / Task / run_new) via B_LDR_PROC_MODIFIERS_MSG, so the helpful
    // message prints ahead of any follow-on error.
    constexpr int out_count = (bld::is_out_mod_v<Configs> + ... + 0);
    constexpr int err_count = (bld::is_err_mod_v<Configs> + ... + 0);
    constexpr int in_count = (bld::is_in_mod_v<Configs> + ... + 0);
    constexpr int batch_count = (bld::is_batch_mod_v<Configs> + ... + 0);
    constexpr int async_count = (bld::is_async_mod_v<Configs> + ... + 0);
    constexpr int label_count = (bld::is_label_mod_v<Configs> + ... + 0);
    constexpr int cwd_count = (bld::is_cwd_mod_v<Configs> + ... + 0);
    constexpr int dry_run_count = (bld::is_dry_run_mod_v<Configs> + ... + 0);
    static_assert(
        out_count <= 1,
        "API ERROR: duplicate out_* modifiers (at most one of out_fd/out_file/lazy_out_file/out_err_file/out_err_fd/out_str/out_err_str).");
    static_assert(
        err_count <= 1,
        "API ERROR: duplicate err_* modifiers (at most one of err_fd/err_file/lazy_err_file/out_err_file/out_err_fd/err_str/out_err_str).");
    static_assert(in_count <= 1, "API ERROR: duplicate in_* modifiers (at most one per command).");
    static_assert(batch_count <= 1, "API ERROR: duplicate pipe (at most one per command).");
    static_assert(async_count <= 1, "API ERROR: duplicate async (at most one per command).");
    static_assert(label_count <= 1, "API ERROR: duplicate label (at most one per command).");
    static_assert(cwd_count <= 1, "API ERROR: duplicate cwd (at most one per command).");
    static_assert(dry_run_count <= 1, "API ERROR: duplicate dry_run (at most one per command).");
    static_assert(
        batch_count == 0 || (out_count == 0 && err_count == 0 && in_count == 0),
        "API ERROR: pipe cannot be combined with per-stream routing.");
}

template <typename... Configs>
auto run(Cmd_loc cl, Configs &&...confs) -> std::expected<bld::Proc, bld::Err>
{
    static_assert((Config_modifier_c<Configs> && ...), B_LDR_PROC_MODIFIERS_MSG);
    bld::validate_run_configs<Configs...>();

    Proc_config cfg{};
    (confs(cfg), ...);
    if (cfg.loc.line() == std::source_location::current().line()) {
        cfg.loc = cl.loc;
    }
    return bld::details::execute(cl.cmd, cfg, cl.loc);
}

template <typename... Configs>
constexpr auto validate_capture_configs() -> void
{
    // NOTE: modifier-category is asserted inline by capture() via
    // B_LDR_CAPTURE_MODIFIERS_MSG.
    constexpr int in_count = (bld::is_cap_in_mod_v<Configs> + ... + 0);
    static_assert(in_count <= 1, "API ERROR: duplicate capture stdin (at most one of in_fd/in_file/lazy_in_file/in_str).");
    constexpr int dry_run_count = (bld::is_dry_run_mod_v<Configs> + ... + 0);
    static_assert(dry_run_count <= 1, "API ERROR: duplicate dry_run (at most one per capture).");
}

template <typename... Configs>
auto capture(Cmd_loc cl, Configs &&...confs) -> std::expected<std::string, bld::Err>
{
    static_assert((Capture_modifier_c<Configs> && ...), B_LDR_CAPTURE_MODIFIERS_MSG);
    bld::validate_capture_configs<Configs...>();

    Capture_config cap_cfg{};
    (confs(cap_cfg), ...);
    if (cap_cfg.loc.line() == std::source_location::current().line()) {
        cap_cfg.loc = cl.loc;
    }

    return bld::details::capture_execute(cl.cmd, cap_cfg, cl.loc);
}

template <typename... Configs>
Task::Task(Cmd_loc cl, Configs &&...confs)
{
    static_assert((Config_modifier_c<Configs> && ...), B_LDR_PROC_MODIFIERS_MSG);
    static_assert(
        !(bld::is_dry_run_mod_v<Configs> || ...),
        "API ERROR: dry_run is per run() call, not per Task; put dry_run{} on the run()/capture() call.");
    spec.cmd = cl.cmd;
    spec.cfg.loc = cl.loc;
    spec.cfg.async = true;
    bld::validate_run_configs<Configs...>();
    (confs(spec.cfg), ...);
}

// Messages above were only needed by the run/capture/Task templates.
#undef B_LDR_PROC_MODIFIERS_MSG
#undef B_LDR_RUN_MODIFIERS_MSG
#undef B_LDR_CAPTURE_MODIFIERS_MSG

template <std::ranges::range Range>
    requires std::convertible_to<std::ranges::range_value_t<Range>, std::string_view>
auto is_outdated(std::string_view target, const Range &sources) -> bool
{
    namespace fs = std::filesystem;
    std::error_code ec;
    if (!fs::exists(target, ec)) {
        bld::log::w("Target '{}' does not exist. Rebuild required.", target);
        return true;
    }
    auto target_time = fs::last_write_time(target, ec);
    if (ec) {
        bld::log::w("Failed to read timestamp for target '{}': {}. Defaulting to rebuild.", target, ec.message());
        return true;
    }
    for (const auto &src : sources) {
        std::string_view source{src};
        if (!fs::exists(source, ec)) {
            bld::log::w("Source file missing: '{}'. Forcing rebuild.", source);
            return true;
        }
        auto source_time = fs::last_write_time(source, ec);
        if (ec) {
            bld::log::w("Failed to read timestamp for source '{}': {}. Defaulting to rebuild.", source, ec.message());
            return true;
        }
        if (target_time < source_time) {
            return true;
        }
    }
    return false;
}

template <typename T>
auto Config::get_val(std::string_view key) const -> std::expected<T, bld::Err>
{
    std::string nkey = Config::normalize_key(key);
    auto it = data.find(nkey);
    if (it == data.end()) {
        return std::unexpected(bld::Err::erc(std::errc::invalid_argument, std::format("Configuration key '{}' not found", nkey)));
    }
    if (auto *p = std::get_if<T>(&it->second)) {
        return *p;
    }
    return std::unexpected(
        bld::Err::erc(std::errc::invalid_argument, std::format("Configuration key '{}' has mismatched type", nkey)));
}

} // namespace bld

namespace bld::log {

template <typename... Args>
inline void invoke_logger(bld::Logger::Level level, std::ostream &str, std::format_string<Args...> fmt, Args &&...args)
{
    if (level < bld::Logger::min_lvl.load(std::memory_order_relaxed)) {
        return;
    }
    std::string formatted_str = std::format(fmt, std::forward<Args>(args)...);
    bld::Logger::Log_record record{.lvl = level, .timestamp = std::chrono::system_clock::now(), .str = formatted_str};
    std::invoke(bld::Logger::logger_fn, str, record);
}

template <typename... Args>
void i(std::format_string<Args...> fmt, Args &&...args)
{
    invoke_logger(bld::Logger::Level::inf, bld::Logger::ostream.get(), fmt, std::forward<Args>(args)...);
}

template <typename... Args>
void w(std::format_string<Args...> fmt, Args &&...args)
{
    invoke_logger(bld::Logger::Level::wrn, bld::Logger::ostream.get(), fmt, std::forward<Args>(args)...);
}

template <typename... Args>
void e(std::format_string<Args...> fmt, Args &&...args)
{
    invoke_logger(bld::Logger::Level::err, bld::Logger::ostream.get(), fmt, std::forward<Args>(args)...);
}

template <typename... Args>
void d(std::format_string<Args...> fmt, Args &&...args)
{
    invoke_logger(bld::Logger::Level::dbg, bld::Logger::ostream.get(), fmt, std::forward<Args>(args)...);
}

template <typename... Args>
void f(std::format_string<Args...> fmt, Args &&...args)
{
    invoke_logger(bld::Logger::Level::ftl, bld::Logger::ostream.get(), fmt, std::forward<Args>(args)...);
}

template <typename Os, typename... Args>
    requires std::derived_from<std::remove_reference_t<Os>, std::ostream>
void i(Os &str, std::format_string<Args...> fmt, Args &&...args)
{
    invoke_logger(bld::Logger::Level::inf, str, fmt, std::forward<Args>(args)...);
}

template <typename Os, typename... Args>
    requires std::derived_from<std::remove_reference_t<Os>, std::ostream>
void w(Os &str, std::format_string<Args...> fmt, Args &&...args)
{
    invoke_logger(bld::Logger::Level::wrn, str, fmt, std::forward<Args>(args)...);
}

template <typename Os, typename... Args>
    requires std::derived_from<std::remove_reference_t<Os>, std::ostream>
void e(Os &str, std::format_string<Args...> fmt, Args &&...args)
{
    invoke_logger(bld::Logger::Level::err, str, fmt, std::forward<Args>(args)...);
}

template <typename Os, typename... Args>
    requires std::derived_from<std::remove_reference_t<Os>, std::ostream>
void d(Os &str, std::format_string<Args...> fmt, Args &&...args)
{
    invoke_logger(bld::Logger::Level::dbg, str, fmt, std::forward<Args>(args)...);
}

template <typename Os, typename... Args>
    requires std::derived_from<std::remove_reference_t<Os>, std::ostream>
void f(Os &str, std::format_string<Args...> fmt, Args &&...args)
{
    invoke_logger(bld::Logger::Level::ftl, str, fmt, std::forward<Args>(args)...);
}

} // namespace bld::log
#endif // B_LDR_HPP

// Implementation — Definitions (behind B_LDR_IMPLEMENTATION)
//   Only one TU should define B_LDR_IMPLEMENTATION. Mirrors header order.
//   Search "IMPL SECTION" to jump. Most non-template definitions live here.
#ifdef B_LDR_IMPLEMENTATION
#ifndef B_LDR_IMPLEMENTATION_ONCE
#define B_LDR_IMPLEMENTATION_ONCE

#include <cerrno>
#include <charconv>
#include <fstream>
#include <iterator>
#include <thread> // only for hardware_concurrency(); no threads are spawned

#ifndef _WIN32
// Implementation-only POSIX headers (kept out of the interface above).
#include <poll.h>
#include <sched.h>
#include <sys/wait.h>
#include <unistd.h>
#endif

#ifndef _WIN32
// POSIX environ for bld::env::get_all()
extern char **environ;
#endif

namespace bld::details {
// CPU count backing max_parallel_count(). Defined here so <thread> stays
// out of the public interface.
[[nodiscard]] inline auto cpu_count() noexcept -> std::size_t
{
    std::size_t m = std::thread::hardware_concurrency();
    return m == 0 ? 1 : m;
}
} // namespace bld::details

auto bld::panic(std::string s) -> void
{
    if (bld::panic_callback(s)) {
        std::exit(EXIT_FAILURE);
    }
}

auto bld::add_exe_on_win32([[maybe_unused]] std::string exec) -> std::string
{
#ifdef _WIN32
    if (!exec.ends_with(".exe")) {
        exec.append(".exe");
    }
#endif
    return exec;
}

// IMPL SECTION 01 — Errors (bld::Err)
auto bld::Err::erc(std::errc code, std::string message) -> Err
{
    return Err{.err = std::make_error_code(code), .msg = std::move(message)};
}

auto bld::Err::erno(int code, std::string message) -> Err
{
    return Err{.err = std::error_code(code, std::generic_category()), .msg = std::move(message)};
}

auto bld::Err::with_cause(Err root_cause) && -> Err
{
    cause_ = std::make_shared<Err>(std::move(root_cause));
    return std::move(*this);
}

auto bld::Err::with_cause(Error_pt root_cause_ptr) && -> Err
{
    cause_ = std::move(root_cause_ptr);
    return std::move(*this);
}

auto std::formatter<bld::Err>::format(const bld::Err &err, std::format_context &ctx) const -> std::format_context::iterator
{
    auto out = ctx.out();
    switch (fmt) {
    case mode::plain:
        if (!err.msg.empty()) {
            out = std::format_to(out, "{}: {}", err.err.message(), err.msg);
        } else {
            out = std::format_to(out, "{}", err.err.message());
        }
        break;
    case mode::debug:
        if (!err.msg.empty()) {
            out = std::format_to(out, "[{}]: {}: {}", err.err.category().name(), err.err.message(), err.msg);
        } else {
            out = std::format_to(out, "[{}]: {}", err.err.category().name(), err.err.message());
        }
        break;
    }
    if (err.cause_) {
        out = std::format_to(out, "\n      -> caused by: {}", *err.cause_);
    }
    return out;
}

void bld::log::detail::Stream_proxy::operator=(std::ostream &os)
{
    ptr = &os;
}

std::ostream &bld::log::detail::Stream_proxy::get() const
{
    return *ptr;
}

// IMPL SECTION 02 — Logging (bld::Logger, bld::log)
auto bld::Logger::Default_logger_fn::operator()(std::ostream &stream, const Log_record &record) const -> void
{
    if (record.lvl < min_lvl) {
        return;
    }
    const auto s = style(record.lvl);
    int depth = indent_level.load(std::memory_order_relaxed);
    std::string pad(static_cast<std::size_t>(depth < 0 ? 0 : depth) * static_cast<std::size_t>(indent_width), ' ');
    if (use_color) {
        std::println(stream, "{}{}{}: {}{}", s.color, s.label, reset, pad, record.str);
    } else {
        std::println(stream, "{}: {}{}", s.label, pad, record.str);
    }
}

auto bld::Logger::set_logger_fn(Logger_fn_t fn, std::source_location loc) -> void
{
    std::lock_guard guard(config_mtx);
    if (logger_locked.load()) {
        throw std::runtime_error{
            std::format("{}:{}:{}: err: Logger already set, you cannot set it twice", loc.file_name(), loc.line(), loc.column())};
    }
    logger_locked = true;
    logger_fn = std::move(fn);
}

// IMPL SECTION 03 — Commands & Processes (bld::Cmd, bld::Proc, Fd_view)
auto bld::Cmd::push(std::string_view s) -> void
{
    args_.emplace_back(s);
}

[[nodiscard]] auto bld::Cmd::argv() const -> std::vector<char *>
{
    auto out = args_ | std::views::transform([](std::string const &s) { return const_cast<char *>(s.c_str()); }) | std::ranges::to<std::vector>();
    out.push_back(nullptr);
    return out;
}

[[nodiscard]] auto bld::Cmd::str() const -> std::string
{
    return args_ | std::views::join_with(std::string_view{" "}) | std::ranges::to<std::string>();
}

auto bld::Cmd::reset() -> void
{
    args_.clear();
}

auto std::formatter<bld::Cmd>::format(const bld::Cmd &cmd, std::format_context &ctx) const -> std::format_context::iterator
{
    auto out = ctx.out();
    switch (fmt) {
    case mode::plain:
        out = std::format_to(out, "\"{}\"", cmd.str());
        break;
    case mode::unquoted:
        out = std::format_to(out, "{}", cmd.str());
        break;
    case mode::debug:
        out = std::format_to(out, "{}", cmd.args_);
        break;
    }
    return out;
}

bld::Cmd_loc::Cmd_loc(const Cmd &c, std::source_location l) : cmd(c), loc(l)
{}

bld::Proc::Proc(P_id id, const std::string &label_) : Proc(id, Proc_gid{}, label_)
{
}

bld::Proc::Proc(P_id id, Proc_gid gid_, const std::string &label_) : id_(id), gid(gid_)
{
#ifdef _WIN32
    auto val = reinterpret_cast<std::uintptr_t>(id);
#else
    auto val = id;
#endif
    if (label_.empty()) {
        constexpr ::std::size_t size{12};
        char buf[size];
        if (auto [end, ec] = std::to_chars(buf, buf + size, val); ec == std::errc{}) {
            spec.cfg.label = std::string{buf, end};
        }
    } else {
        spec.cfg.label = label_;
    }
    status_ = Status{.state = State::running};
}

bld::Proc::~Proc()
{
#ifdef _WIN32
    if (id_ != nullptr && status_.state == State::running) {
        this->kill(9);
        std::ignore = this->wait();
    }
#else
    if (id_ > 0 && status_.state == State::running) {
        this->kill(SIGKILL);
        std::ignore = this->wait();
    }
#endif
    // Covers statuses set externally (e.g. Proc_group::wait_any) without wait().
    finish_capture();
}

#ifdef _WIN32
bld::Proc::Proc(Proc &&other) noexcept
    : id_(std::exchange(other.id_, nullptr)), status_(other.status_), spec(std::move(other.spec)), gid(other.gid),
      cap_out_fd_(std::exchange(other.cap_out_fd_, -1)), cap_err_fd_(std::exchange(other.cap_err_fd_, -1)),
      cap_out_(std::exchange(other.cap_out_, nullptr)), cap_err_(std::exchange(other.cap_err_, nullptr))
{}
#else
bld::Proc::Proc(Proc &&other) noexcept
    : id_(std::exchange(other.id_, -1)), status_(other.status_), spec(std::move(other.spec)), gid(other.gid),
      cap_out_fd_(std::exchange(other.cap_out_fd_, -1)), cap_err_fd_(std::exchange(other.cap_err_fd_, -1)),
      cap_out_(std::exchange(other.cap_out_, nullptr)), cap_err_(std::exchange(other.cap_err_, nullptr))
{}
#endif

auto bld::Proc::operator=(Proc &&other) noexcept -> Proc &
{
    if (this != &other) {
#ifdef _WIN32
        if (id_ != nullptr && status_.state == State::running) {
            kill(9);
            std::ignore = wait();
        }
        id_ = std::exchange(other.id_, nullptr);
#else
        if (id_ > 0 && status_.state == State::running) {
            kill(SIGKILL);
            std::ignore = wait();
        }
        id_ = std::exchange(other.id_, -1);
#endif
        // wait() above drains when it reaps, but statuses set externally
        // (Proc_group::wait_any) can leave capture pipes half-drained.
        // The child is gone there, so finishing is exact. Unconditional and safe.
        finish_capture();
        status_ = other.status_;
        spec = std::move(other.spec);
        gid = other.gid;
        // Take over capture pipes (exchange to avoid double-close).
        cap_out_fd_ = std::exchange(other.cap_out_fd_, -1);
        cap_err_fd_ = std::exchange(other.cap_err_fd_, -1);
        cap_out_ = std::exchange(other.cap_out_, nullptr);
        cap_err_ = std::exchange(other.cap_err_, nullptr);
    }
    return *this;
}

auto bld::Proc::has_capture() const noexcept -> bool
{
    return cap_out_fd_ >= 0 || cap_err_fd_ >= 0;
}

auto bld::Proc::pump_capture_nonblocking() -> void
{
    if (cap_out_fd_ >= 0 && cap_out_ != nullptr) {
        if (details::pump_fd_nonblocking(cap_out_fd_, *cap_out_)) {
            details::close_fd(cap_out_fd_);
            cap_out_fd_ = -1;
        }
    } else if (cap_out_fd_ >= 0) {
        details::close_fd(cap_out_fd_);
        cap_out_fd_ = -1;
    }
    if (cap_err_fd_ >= 0 && cap_err_ != nullptr) {
        if (cap_err_fd_ == cap_out_fd_) {
            // Merged into the same pipe: nothing extra to do.
        } else if (details::pump_fd_nonblocking(cap_err_fd_, *cap_err_)) {
            details::close_fd(cap_err_fd_);
            cap_err_fd_ = -1;
        }
    } else if (cap_err_fd_ >= 0) {
        details::close_fd(cap_err_fd_);
        cap_err_fd_ = -1;
    }
}

auto bld::Proc::finish_capture() -> void
{
    // The child is dead (or never existed) here, so EOF arrives without
    // blocking: pump until both pipes report EOF. Sleep briefly between
    // rounds so we don't spin if the kernel hasn't delivered EOF yet.
    for (int i = 0; i < 5000 && has_capture(); ++i) {
        pump_capture_nonblocking();
        if (has_capture()) {
            details::sleep_ms(1);
        }
    }
    if (cap_out_fd_ >= 0) {
        details::close_fd(cap_out_fd_);
        cap_out_fd_ = -1;
    }
    if (cap_err_fd_ >= 0) {
        details::close_fd(cap_err_fd_);
        cap_err_fd_ = -1;
    }
    // Captured text matches capture(): CRLF -> LF (no-op on Linux).
    auto *out = std::exchange(cap_out_, nullptr);
    auto *err = std::exchange(cap_err_, nullptr);
    if (out != nullptr) {
        *out = bld::str::replace_all(*out, "\r\n", "\n");
    }
    if (err != nullptr && err != out) {
        *err = bld::str::replace_all(*err, "\r\n", "\n");
    }
}

auto bld::Proc::join_drain() -> void
{
    finish_capture();
}

#ifdef _WIN32
auto bld::Proc::reap_win_process() -> void
{
    DWORD exit_code = 0;
    if (::GetExitCodeProcess(static_cast<HANDLE>(id_), &exit_code)) {
        status_ = {State::exited, static_cast<int>(exit_code)};
    }
    ::CloseHandle(static_cast<HANDLE>(id_));
    id_ = nullptr;
    finish_capture();
}
#endif

auto bld::Proc::wait() -> std::expected<Status, bld::Err>
{
#ifdef _WIN32
    if (!id_ || status_.state != State::running) {
        finish_capture();
        return status_;
    }
    if (!has_capture()) {
        if (::WaitForSingleObject(static_cast<HANDLE>(id_), INFINITE) == WAIT_FAILED) {
            return std::unexpected(bld::Err::erno(GetLastError(), "WaitForSingleObject failed"));
        }
        reap_win_process();
        return status_;
    }
    // With string capture: pump pipes while waiting so the child never
    // blocks on a full pipe. Short slices keep latency low.
    while (true) {
        pump_capture_nonblocking();
        DWORD res = ::WaitForSingleObject(static_cast<HANDLE>(id_), 10);
        if (res == WAIT_OBJECT_0) {
            reap_win_process();
            return status_;
        }
        if (res == WAIT_FAILED) {
            return std::unexpected(bld::Err::erno(GetLastError(), "WaitForSingleObject failed"));
        }
        // WAIT_TIMEOUT: loop and pump again.
    }
#else
    if (id_ <= 0 || status_.state != State::running) {
        finish_capture();
        return status_;
    }
    if (!has_capture()) {
        int wstatus = 0;
        if (::waitpid(id_, &wstatus, 0) == -1) {
            if (errno == ECHILD) {
                status_ = {State::exited, 255};
                finish_capture();
                return status_;
            }
            return std::unexpected(bld::Err::erno(errno, "waitpid failed"));
        }
        update_status(wstatus);
        finish_capture();
        return status_;
    }
    // With string capture: interleave non-blocking pipe pumps with
    // waitpid(WNOHANG) + poll() so large outputs never deadlock.
    while (true) {
        pump_capture_nonblocking();
        int wstatus = 0;
        pid_t r = ::waitpid(id_, &wstatus, WNOHANG);
        if (r == -1) {
            if (errno == EINTR) {
                continue;
            }
            if (errno == ECHILD) {
                status_ = {State::exited, 255};
                finish_capture();
                return status_;
            }
            return std::unexpected(bld::Err::erno(errno, "waitpid failed"));
        }
        if (r > 0) {
            update_status(wstatus);
            if (status_.state == State::exited || status_.state == State::signaled) {
                finish_capture();
                return status_;
            }
            // Stopped/continued: keep pumping; child may write again.
        }
        if (!has_capture()) {
            details::sleep_ms(1);
            continue;
        }
        struct pollfd pfds[2];
        int n = 0;
        if (cap_out_fd_ >= 0) {
            pfds[n].fd = cap_out_fd_;
            pfds[n].events = POLLIN;
            pfds[n].revents = 0;
            ++n;
        }
        if (cap_err_fd_ >= 0 && cap_err_fd_ != cap_out_fd_) {
            pfds[n].fd = cap_err_fd_;
            pfds[n].events = POLLIN;
            pfds[n].revents = 0;
            ++n;
        }
        ::poll(pfds, static_cast<nfds_t>(n), 10);
    }
#endif
}

auto bld::Proc::try_wait() -> std::expected<Status, bld::Err>
{
#ifdef _WIN32
    if (!id_ || status_.state != State::running) {
        finish_capture();
        return status_;
    }
    pump_capture_nonblocking();
    DWORD res = ::WaitForSingleObject(static_cast<HANDLE>(id_), 0);
    if (res == WAIT_OBJECT_0) {
        reap_win_process();
    } else if (res == WAIT_FAILED) {
        return std::unexpected(bld::Err::erno(GetLastError(), "WaitForSingleObject failed"));
    }
    return status_;
#else
    if (id_ <= 0 || status_.state != State::running) {
        finish_capture();
        return status_;
    }
    pump_capture_nonblocking();
    int wstatus = 0;
    P_id res = ::waitpid(id_, &wstatus, WNOHANG);

    if (res == -1) {
        if (errno == ECHILD) {
            status_ = {State::exited, 255};
            finish_capture();
            return status_;
        }
        return std::unexpected(bld::Err::erno(errno, "waitpid WNOHANG failed"));
    }

    if (res > 0) {
        update_status(wstatus);
        // Finish only once the child is done. Stopped/continued children may
        // resume writing, so their pipes must keep being pumped.
        if (status_.state == State::exited || status_.state == State::signaled) {
            finish_capture();
        }
    }

    return status_;
#endif
}

auto bld::Proc::kill(int sig) -> void
{
#ifdef _WIN32
    if (id_ && status_.state == State::running) {
        ::TerminateProcess(static_cast<HANDLE>(id_), static_cast<UINT>(sig));
    }
#else
    if (id_ > 0 && status_.state == State::running) {
        if (::kill(-id_, sig) != 0) {
            std::ignore = ::kill(id_, sig);
        }
    }
#endif
}

auto bld::Proc::pid() const -> P_id
{
    return id_;
}

auto bld::Proc::pgid() const -> Proc_gid
{
    return gid;
}

auto bld::Proc::status() const -> Status
{
    return status_;
}

auto bld::Proc::is_running() const -> bool
{
    return status_.state == State::running;
}

auto bld::Proc::status_code() const -> int
{
    return status_.code;
}

auto bld::details::check_proc_config(const Proc_config &cfg) -> std::expected<void, bld::Err>
{
    if (!cfg.cwd.empty()) {
        std::error_code ec;
        if (!std::filesystem::exists(cfg.cwd, ec)) {
            return std::unexpected(
                Err::erc(std::errc::no_such_file_or_directory, std::format("working directory '{}' does not exist", cfg.cwd)));
        }
        if (ec) {
            return std::unexpected(Err::erc(std::errc::io_error, std::format("cannot stat working directory '{}': {}", cfg.cwd, ec.message())));
        }
        if (!std::filesystem::is_directory(cfg.cwd, ec)) {
            return std::unexpected(Err::erc(std::errc::not_a_directory, std::format("working directory '{}' is not a directory", cfg.cwd)));
        }
        if (ec) {
            return std::unexpected(Err::erc(std::errc::io_error, std::format("cannot stat working directory '{}': {}", cfg.cwd, ec.message())));
        }
    }
    return {};
}

auto bld::details::make_pipe(int fds[2], const char *name) -> std::expected<void, bld::Err>
{
#ifdef _WIN32
    if (::_pipe(fds, 4096, _O_BINARY) == -1) {
        return std::unexpected(bld::Err::erno(errno, std::format("{} pipe failed", name)));
    }
#else
    if (::pipe(fds) == -1) {
        return std::unexpected(bld::Err::erno(errno, std::format("{} pipe failed", name)));
    }
    ::fcntl(fds[0], F_SETFD, FD_CLOEXEC);
    ::fcntl(fds[1], F_SETFD, FD_CLOEXEC);
#endif
    return {};
}

auto bld::details::close_fd(int fd) -> void
{
    if (fd >= 0) {
#ifdef _WIN32
        ::_close(fd);
#else
        ::close(fd);
#endif
    }
}

auto bld::details::read_fd(int fd, void *buf, unsigned int count) -> int
{
#ifdef _WIN32
    return ::_read(fd, buf, count);
#else
    return static_cast<int>(::read(fd, buf, count));
#endif
}

auto bld::details::write_fd(int fd, const void *buf, unsigned int count) -> int
{
#ifdef _WIN32
    return ::_write(fd, buf, count);
#else
    return static_cast<int>(::write(fd, buf, count));
#endif
}

auto bld::details::set_nonblocking(int fd) -> void
{
    if (fd < 0) {
        return;
    }
#ifdef _WIN32
    // CRT pipes have no O_NONBLOCK: callers use PeekNamedPipe to avoid
    // blocking _read instead. Nothing to set here.
    (void)fd;
#else
    int flags = ::fcntl(fd, F_GETFL, 0);
    if (flags != -1) {
        ::fcntl(fd, F_SETFL, flags | O_NONBLOCK);
    }
#endif
}

auto bld::details::sleep_ms(int ms) -> void
{
    if (ms <= 0) {
        return;
    }
#ifdef _WIN32
    ::Sleep(static_cast<DWORD>(ms));
#else
    struct timespec ts;
    ts.tv_sec = ms / 1000;
    ts.tv_nsec = static_cast<long>((ms % 1000) * 1000000L);
    ::nanosleep(&ts, nullptr);
#endif
}

// Single-threaded pipe pump: read everything currently available without
// blocking. True => EOF (or fatal error): the pipe is done.
auto bld::details::pump_fd_nonblocking(int fd, std::string &out) -> bool
{
    if (fd < 0) {
        return true;
    }
#ifdef _WIN32
    HANDLE h = reinterpret_cast<HANDLE>(::_get_osfhandle(fd));
    if (h == INVALID_HANDLE_VALUE) {
        return true;
    }
    char buf[4096];
    while (true) {
        DWORD avail = 0;
        if (!::PeekNamedPipe(h, nullptr, 0, nullptr, &avail, nullptr)) {
            // Broken/closed pipe => EOF. Anything left was already read.
            return true;
        }
        if (avail == 0) {
            return false;
        }
        DWORD want = avail < sizeof(buf) ? avail : static_cast<DWORD>(sizeof(buf));
        int n = ::_read(fd, buf, static_cast<unsigned int>(want));
        if (n > 0) {
            out.append(buf, static_cast<std::size_t>(n));
            continue;
        }
        if (n == 0) {
            return true;
        }
        return false;
    }
#else
    char buf[4096];
    while (true) {
        ssize_t n = ::read(fd, buf, sizeof(buf));
        if (n > 0) {
            out.append(buf, static_cast<std::size_t>(n));
            continue;
        }
        if (n == 0) {
            return true;
        }
        if (errno == EINTR) {
            continue;
        }
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            return false;
        }
        return true;
    }
#endif
}

namespace bld::details {
// Resolves one Io_slot to an effective fd (+ lazily opened Owned_Fd when the
// slot holds a path). Unset / std-fd / empty-path / invalid-shared all mean
// inherit. Extracted from Proc::spawn; behavior unchanged.
inline auto resolve_io_slot(const Io_slot &slot, int std_fd, Open_mode mode_for_path)
    -> std::expected<std::pair<Fd_view, Owned_Fd>, Err>
{
    if (std::holds_alternative<std::monostate>(slot)) {
        return std::pair{Fd_view{std_fd}, Owned_Fd{}};
    }
    if (auto *f = std::get_if<Fd_view>(&slot)) {
        if (!f->is_valid() || f->val == std_fd || f->val == Fd_view::INVALID) {
            return std::pair{Fd_view{std_fd}, Owned_Fd{}};
        }
        return std::pair{*f, Owned_Fd{}};
    }
    if (auto *p = std::get_if<std::string>(&slot)) {
        if (p->empty()) {
            return std::pair{Fd_view{std_fd}, Owned_Fd{}};
        }
        auto r = Owned_Fd::open(*p, mode_for_path);
        if (!r) {
            return std::unexpected(std::move(r.error()));
        }
        Fd_view v{r->handle_};
        return std::pair{v, std::move(*r)};
    }
    if (auto *s = std::get_if<Shared_fd>(&slot)) {
        if (!s || !*s || (*s)->handle_ == Fd_view::INVALID) {
            return std::pair{Fd_view{std_fd}, Owned_Fd{}};
        }
        return std::pair{Fd_view{(*s)->handle_}, Owned_Fd{}};
    }
    return std::pair{Fd_view{std_fd}, Owned_Fd{}};
}
} // namespace bld::details

auto bld::Proc::spawn(const Exec_spec &spec, Proc_gid gid) -> std::expected<Proc, Err>
{
    if (spec.cmd.empty()) {
        return std::unexpected(Err::erc(std::errc::invalid_argument, "Command cannot be empty"));
    }
    if (auto ok = details::check_proc_config(spec.cfg); !ok) {
        return std::unexpected(std::move(ok.error()));
    }
    const auto &cmd = spec.cmd;
    const auto &cfg = spec.cfg;
    // String capture (out_str/err_str/out_err_str): borrowed string* slots are
    // piped below; the parent keeps the read ends and pumps them
    // single-threaded in wait()/wait_any()/capture_execute. Separate output
    // stays separate: only out_err_str (which sets merge_err_and_out) merges.
    std::string *cap_out = io_str(cfg.io_out);
    std::string *cap_err = io_str(cfg.io_err);
    // One pipe when both slots name the same string (out_err_str).
    bool cap_merged = cap_out != nullptr && cap_out == cap_err;
    if (cap_merged && !cfg.merge_err_and_out) {
        // A single string cannot hold two separate streams without merging;
        // out_err_str{s} is the merged form (only reachable hand-built).
        return std::unexpected(Err::erc(
            std::errc::invalid_argument, "same string captures both stdout and stderr without merging; use out_err_str{s}"));
    }
    // Resolve one Io_slot per stream. A slot holds a single alternative
    // (unset / borrowed fd / path / shared owned fd / string target), so
    // fd-vs-path conflicts are unrepresentable by construction.
    // Note: string* slots fall through to inherit here; they are piped
    // separately below (cap pipes override eff_out/eff_err).
    auto r_in = details::resolve_io_slot(cfg.io_in, STDIN_FILENO, Open_mode::read);
    if (!r_in) {
        return std::unexpected(std::move(r_in.error()));
    }
    auto r_out = details::resolve_io_slot(cfg.io_out, STDOUT_FILENO, Open_mode::write);
    if (!r_out) {
        return std::unexpected(std::move(r_out.error()));
    }
    Owned_Fd opened_err;
    Fd_view eff_err{STDERR_FILENO};
    if (cfg.merge_err_and_out) {
        eff_err = Fd_view{STDERR_FILENO}; // merged later via dup2
    } else {
        auto r_err = details::resolve_io_slot(cfg.io_err, STDERR_FILENO, Open_mode::write);
        if (!r_err) {
            return std::unexpected(std::move(r_err.error()));
        }
        eff_err = r_err->first;
        opened_err = std::move(r_err->second);
    }
    Fd_view eff_in = r_in->first;
    Owned_Fd opened_in = std::move(r_in->second);
    Fd_view eff_out = r_out->first;
    Owned_Fd opened_out = std::move(r_out->second);
    // Keep shared owned fds alive until after fork (they live in cfg already,
    // but hold local copies for lazily-opened paths above).
    // Note: cfg.io_* Shared_fd alternatives stay alive via spec copy in Proc.
    //
    // String-capture pipes. The write ends override eff_out/eff_err below; the
    // read ends are kept by the returned Proc and pumped single-threaded.
    // The parent closes its write-end copies right after spawning so pumps
    // see EOF when the child exits.
    int cap_pipe_out[2]{-1, -1}, cap_pipe_err[2]{-1, -1};
    bool want_out_cap = cap_out != nullptr;
    bool want_err_cap = cap_err != nullptr && !cap_merged; // merged err flows through the stdout pipe
    auto close_cap_pipes = [&]() {
        details::close_fd(cap_pipe_out[0]);
        details::close_fd(cap_pipe_out[1]);
        details::close_fd(cap_pipe_err[0]);
        details::close_fd(cap_pipe_err[1]);
    };
    auto close_cap_writes = [&]() {
        details::close_fd(cap_pipe_out[1]);
        cap_pipe_out[1] = -1;
        details::close_fd(cap_pipe_err[1]);
        cap_pipe_err[1] = -1;
    };
    if (want_out_cap) {
        if (auto res = details::make_pipe(cap_pipe_out, "stdout"); !res) {
            return std::unexpected(std::move(res.error()));
        }
        eff_out = Fd_view{cap_pipe_out[1]};
    }
    if (want_err_cap) {
        if (auto res = details::make_pipe(cap_pipe_err, "stderr"); !res) {
            close_cap_pipes();
            return std::unexpected(std::move(res.error()));
        }
        eff_err = Fd_view{cap_pipe_err[1]};
    }
    // Attaches capture pipes to a freshly spawned Proc. Called once, right
    // before returning, on both platform branches. Read ends become
    // non-blocking so the single-threaded pumps never stall the scheduler.
    auto attach_capture = [&](Proc &p) {
        if (want_out_cap) {
            details::set_nonblocking(cap_pipe_out[0]);
            p.cap_out_fd_ = cap_pipe_out[0];
            p.cap_out_ = cap_out;
            cap_pipe_out[0] = -1; // ownership moved to Proc
        }
        if (want_err_cap) {
            details::set_nonblocking(cap_pipe_err[0]);
            p.cap_err_fd_ = cap_pipe_err[0];
            p.cap_err_ = cap_err;
            cap_pipe_err[0] = -1; // ownership moved to Proc
        }
    };
#ifdef _WIN32
    std::string cmd_str = cmd.str();
    std::string cwd_str{cfg.cwd};

    STARTUPINFOEXA siex;
    ZeroMemory(&siex, sizeof(siex));
    siex.StartupInfo.cb = sizeof(siex);
    siex.StartupInfo.dwFlags = STARTF_USESTDHANDLES;
    auto to_handle = [](Fd_view f, HANDLE std_h) -> HANDLE {
        if (!f.is_valid() || f.val == STDIN_FILENO || f.val == STDOUT_FILENO || f.val == STDERR_FILENO) {
            return std_h;
        }
        return reinterpret_cast<HANDLE>(_get_osfhandle(f.val));
    };
    siex.StartupInfo.hStdInput = to_handle(eff_in, GetStdHandle(STD_INPUT_HANDLE));
    siex.StartupInfo.hStdOutput = to_handle(eff_out, GetStdHandle(STD_OUTPUT_HANDLE));
    if (cfg.merge_err_and_out) {
        siex.StartupInfo.hStdError = siex.StartupInfo.hStdOutput;
    } else {
        siex.StartupInfo.hStdError = to_handle(eff_err, GetStdHandle(STD_ERROR_HANDLE));
    }

    // Exact handle inheritance: without this, the child (and any grandchild
    // it spawns, e.g. `cmd /c sort`) inherits EVERY inheritable handle —
    // including the parent's write end of the child's own stdin pipe — so a
    // program reading stdin to EOF never sees EOF and hangs. POSIX gets this
    // via CLOEXEC; here the attribute list restricts inheritance to the
    // three std handles. Falls back to plain STARTUPINFO if unavailable.
    HANDLE inherit_list[3]{nullptr, nullptr, nullptr};
    int inherit_count = 0;
    for (HANDLE h : {siex.StartupInfo.hStdInput, siex.StartupInfo.hStdOutput, siex.StartupInfo.hStdError}) {
        if (h != nullptr) {
            bool dup = false;
            for (int i = 0; i < inherit_count; ++i) {
                if (inherit_list[i] == h) {
                    dup = true;
                    break;
                }
            }
            if (!dup) {
                inherit_list[inherit_count++] = h;
            }
        }
    }
    for (int i = 0; i < inherit_count; ++i) {
        ::SetHandleInformation(inherit_list[i], HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT);
    }
    bool use_attr_list = false;
    if (inherit_count > 0) {
        SIZE_T attr_size = 0;
        ::InitializeProcThreadAttributeList(nullptr, 1, 0, &attr_size);
        siex.lpAttributeList = static_cast<LPPROC_THREAD_ATTRIBUTE_LIST>(::HeapAlloc(::GetProcessHeap(), 0, attr_size));
        if (siex.lpAttributeList != nullptr
            && ::InitializeProcThreadAttributeList(siex.lpAttributeList, 1, 0, &attr_size)
            && ::UpdateProcThreadAttribute(
                siex.lpAttributeList, 0, PROC_THREAD_ATTRIBUTE_HANDLE_LIST, inherit_list,
                static_cast<SIZE_T>(inherit_count) * sizeof(HANDLE), nullptr, nullptr)) {
            use_attr_list = true;
        } else {
            if (siex.lpAttributeList != nullptr) {
                ::HeapFree(::GetProcessHeap(), 0, siex.lpAttributeList);
                siex.lpAttributeList = nullptr;
            }
        }
    }

    PROCESS_INFORMATION pi;
    ZeroMemory(&pi, sizeof(pi));

    BOOL created = ::CreateProcessA(
        nullptr,
        cmd_str.data(),
        nullptr,
        nullptr,
        TRUE,
        CREATE_SUSPENDED | (use_attr_list ? EXTENDED_STARTUPINFO_PRESENT : 0),
        nullptr,
        cwd_str.empty() ? nullptr : cwd_str.c_str(),
        &siex.StartupInfo,
        &pi);
    if (siex.lpAttributeList != nullptr) {
        ::DeleteProcThreadAttributeList(siex.lpAttributeList);
        ::HeapFree(::GetProcessHeap(), 0, siex.lpAttributeList);
        siex.lpAttributeList = nullptr;
    }
    if (!created) {
        close_cap_pipes();
        return std::unexpected(Err::erno(GetLastError(), "CreateProcess failed").with_payload(cmd));
    }
    if (gid.is_valid()) {
        ::AssignProcessToJobObject(static_cast<HANDLE>(gid.val), pi.hProcess);
    }
    ::ResumeThread(pi.hThread);
    CloseHandle(pi.hThread);
    // Parent drops its write-end copies so pumps see EOF at child exit.
    // (The child inherits its own duplicates; they close when it exits.
    // Pumps finish only after reaping/exit-observe, so EOF is exact.)
    close_cap_writes();
    Proc p;
    p.id_ = pi.hProcess;
    p.spec = spec;
    p.gid = gid;
    if (p.spec.cfg.label.empty()) {
        p.spec.cfg.label = cmd_str;
    }
    p.status_ = Status{.state = State::running};
    attach_capture(p);
    return p;
#else
    P_id pid = ::fork();
    if (pid < 0) {
        close_cap_pipes();
        return std::unexpected(Err::erno(errno, "Fork failed").with_payload(cmd));
    }
    if (pid == 0) {
        if (gid.is_valid() && gid.val != Proc_gid::CREATE_NEW) {
            ::setpgid(0, gid.val);
        } else {
            std::ignore = ::setpgid(0, 0);
        }
        if (!cfg.cwd.empty() && ::chdir(std::string{cfg.cwd}.c_str()) != 0) {
            ::_exit(127);
        }
        auto dup_if_needed = [](Fd_view f, int target) {
            if (f.is_valid() && f.val != target && f.val >= 0) {
                ::dup2(f.val, target);
            }
        };
        dup_if_needed(eff_in, STDIN_FILENO);
        dup_if_needed(eff_out, STDOUT_FILENO);
        if (cfg.merge_err_and_out) {
            ::dup2(STDOUT_FILENO, STDERR_FILENO);
        } else {
            dup_if_needed(eff_err, STDERR_FILENO);
        }
        auto close_if_needed = [](Fd_view f) {
            if (f.is_valid() && f.val > STDERR_FILENO) {
                ::close(f.val);
            }
        };
        close_if_needed(eff_in);
        close_if_needed(eff_out);
        close_if_needed(eff_err);
        // String-capture read ends belong to the parent's pumps.
        if (cap_pipe_out[0] >= 0) {
            ::close(cap_pipe_out[0]);
        }
        if (cap_pipe_err[0] >= 0) {
            ::close(cap_pipe_err[0]);
        }

        auto argv = cmd.argv();
        ::execvp(argv[0], argv.data());
        ::_exit(127);
    }
    // parent: ensure child is in its own group so group signals reach the tree
    if (!gid.is_valid() || gid.val == Proc_gid::CREATE_NEW) {
        ::setpgid(pid, pid);
    } else {
        ::setpgid(pid, gid.val);
    }
    // Parent drops its write-end copies so pumps see EOF at child exit.
    close_cap_writes();
    Proc p;
    p.id_ = pid;
    p.spec = spec;
    if (gid.is_valid() && gid.val == Proc_gid::CREATE_NEW) {
        p.gid = Proc_gid{pid};
    } else {
        p.gid = gid.is_valid() ? gid : Proc_gid{pid};
    }
    if (p.spec.cfg.label.empty()) {
        p.spec.cfg.label = cmd.str();
    }
    p.status_ = Status{.state = State::running};
    attach_capture(p);
    return p;
#endif
}
auto bld::Proc::spawn(const Task &task, Proc_gid gid) -> std::expected<Proc, Err>
{
    return spawn(task.spec, gid);
}

auto bld::Proc::wait_pid(P_id pid, int options) -> std::expected<Status, bld::Err>
{
#ifdef _WIN32
    (void)options;
    if (!pid) {
        return Status{.state = State::exited, .code = 255};
    }
    if (::WaitForSingleObject(static_cast<HANDLE>(pid), INFINITE) == WAIT_FAILED) {
        return std::unexpected(bld::Err::erno(GetLastError(), "wait_pid failed"));
    }
    DWORD exit_code = 0;
    GetExitCodeProcess(static_cast<HANDLE>(pid), &exit_code);
    return Status{.state = State::exited, .code = static_cast<int>(exit_code)};
#else
    int wstatus = 0;
    P_id res = ::waitpid(pid, &wstatus, options);
    if (res == -1) {
        if (errno == ECHILD) {
            return Status{.state = State::exited, .code = 255};
        }
        return std::unexpected(bld::Err::erno(errno, "waitpid failed"));
    }
    if (res == 0) {
        return Status{.state = State::running, .code = 0};
    }
    return parse_status(wstatus);
#endif
}

auto bld::Proc::try_wait_pid(P_id pid) -> std::expected<Status, bld::Err>
{
#ifdef _WIN32
    if (!pid) {
        return Status{.state = State::exited, .code = 255};
    }
    DWORD res = ::WaitForSingleObject(static_cast<HANDLE>(pid), 0);
    if (res == WAIT_OBJECT_0) {
        DWORD exit_code = 0;
        GetExitCodeProcess(static_cast<HANDLE>(pid), &exit_code);
        return Status{.state = State::exited, .code = static_cast<int>(exit_code)};
    }
    return Status{.state = State::running, .code = 0};
#else
    return wait_pid(pid, WNOHANG);
#endif
}

auto bld::Proc::parse_status(int wstatus) -> Status
{
    Status s{};

#ifdef _WIN32
    // Windows process exit code
    if (wstatus == 0) {
        s.state = State::exited;
        s.code = 0;
    } else {
        s.state = State::exited;
        s.code = wstatus;
    }
#else
    if (WIFEXITED(wstatus)) {
        s.state = State::exited;
        s.code = static_cast<int>(WEXITSTATUS(wstatus));
    } else if (WIFSIGNALED(wstatus)) {
        s.state = State::signaled;
        s.code = static_cast<int>(WTERMSIG(wstatus));
    } else if (WIFSTOPPED(wstatus)) {
        s.state = State::stopped;
        s.code = static_cast<int>(WSTOPSIG(wstatus));
    } else if (WIFCONTINUED(wstatus)) {
        s.state = State::continued;
        s.code = 0;
    }
#endif

    return s;
}

auto bld::Proc::update_status(int wstatus) -> void
{
    status_ = parse_status(wstatus);
}

// IMPL SECTION 03b — Proc_group (manages Proc lifetimes as a group)
bld::Proc_group::Proc_group()
{
#ifdef _WIN32
    HANDLE job = ::CreateJobObjectA(nullptr, nullptr);
    if (job) {
        JOBOBJECT_EXTENDED_LIMIT_INFORMATION info{};
        info.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
        ::SetInformationJobObject(job, JobObjectExtendedLimitInformation, &info, sizeof(info));
        gid_ = Proc_gid{job};
    } else {
        bld::panic(std::format("Failed to create Win32 Job Object: {}", GetLastError()));
    }
#else
    gid_ = Proc_gid{Proc_gid::CREATE_NEW};
#endif
}
bld::Proc_group::~Proc_group()
{
    terminate(9);
#ifdef _WIN32
    if (gid_.val) {
        ::CloseHandle(static_cast<HANDLE>(gid_.val));
    }
#endif
}
bld::Proc_group::Proc_group(Proc_group &&other) noexcept
    : gid_(std::exchange(other.gid_, Proc_gid{})), hive_(std::move(other.hive_)), free_list_(std::move(other.free_list_)),
      generation_(std::exchange(other.generation_, 1)), active_count_(std::exchange(other.active_count_, 0))
{}
bld::Proc_group &bld::Proc_group::operator=(Proc_group &&other) noexcept
{
    if (this != &other) {
        terminate(9);
#ifdef _WIN32
        if (gid_.val) {
            ::CloseHandle(static_cast<HANDLE>(gid_.val));
        }
#endif
        gid_ = std::exchange(other.gid_, Proc_gid{});
        hive_ = std::move(other.hive_);
        free_list_ = std::move(other.free_list_);
        generation_ = std::exchange(other.generation_, 1);
        active_count_ = std::exchange(other.active_count_, 0);
    }
    return *this;
}
auto bld::Proc_group::gid() const -> Proc_gid
{
    return gid_;
}
auto bld::Proc_group::empty() const -> bool
{
    return active_count_ == 0;
}
auto bld::Proc_group::size() const -> std::size_t
{
    return active_count_;
}
auto bld::Proc_group::run_new(const Task &task) -> std::expected<Proc_id, Err>
{
    return run_new(task.spec);
}
auto bld::Proc_group::run_new(const Exec_spec &spec) -> std::expected<Proc_id, Err>
{
    Exec_spec s = spec;
    s.cfg.async = true;
    auto proc = Proc::spawn(s, gid_);
    if (!proc) {
        return std::unexpected(proc.error());
    }
    return add(std::move(*proc));
}
auto bld::Proc_group::add(Proc &&proc) -> Proc_id
{
    auto incoming_gid = proc.pgid();
#ifndef _WIN32
    if (gid_.val == Proc_gid::CREATE_NEW) {
        gid_ = incoming_gid;
    }
#endif
    if (incoming_gid != gid_) {
        bld::panic(
            std::format(
                "Proc_group: mismatched GID (expected {}, got {})",
#ifdef _WIN32
                (void *)gid_.val,
                (void *)incoming_gid.val
#else
                (int)gid_.val,
                (int)incoming_gid.val
#endif
                ));
    }
    std::uint32_t idx;
    if (!free_list_.empty()) {
        idx = free_list_.back();
        free_list_.pop_back();
    } else {
        idx = static_cast<std::uint32_t>(hive_.size());
        hive_.emplace_back();
    }
    Proc_id nid = (static_cast<Proc_id>(generation_) << 32) | idx;
    hive_[idx].id = nid;
    hive_[idx].proc = std::move(proc);
    ++active_count_;
    return nid;
}
auto bld::Proc_group::get(Proc_id id) -> std::expected<std::reference_wrapper<Proc>, Err>
{
    std::uint32_t idx = static_cast<std::uint32_t>(id & 0xFFFFFFFF);
    if (idx < hive_.size() && hive_[idx].id == id) {
        return std::ref(hive_[idx].proc);
    }
    return std::unexpected(Err::erc(std::errc::invalid_argument, std::format("Invalid Proc_id {}", id)));
}
auto bld::Proc_group::remove(Proc_id id) -> bool
{
    std::uint32_t idx = static_cast<std::uint32_t>(id & 0xFFFFFFFF);
    if (idx < hive_.size() && hive_[idx].id == id) {
        hive_[idx].id = 0;
        hive_[idx].proc = Proc{};
        free_list_.push_back(idx);
        --active_count_;
        ++generation_;
#ifndef _WIN32
        // A pgid dies with its last member. Reset so the next spawn starts a
        // fresh group instead of joining a dead one (waitpid would ECHILD).
        if (active_count_ == 0) {
            gid_ = Proc_gid{Proc_gid::CREATE_NEW};
        }
#endif
        return true;
    }
    return false;
}
auto bld::Proc_group::wait_any() -> std::expected<Proc_id, Err>
{
    if (active_count_ == 0) {
        return std::unexpected(Err::erc(std::errc::no_child_process, "Proc_group empty"));
    }
#ifdef _WIN32
    // Pump every member's capture pipes while waiting: a blocking
    // WaitForMultipleObjects(INFINITE) would let a sibling's pipe fill and
    // deadlock the child that is ready to exit.
    while (true) {
        std::vector<HANDLE> handles;
        std::vector<Proc_id> ids;
        for (auto &slot : hive_) {
            if (slot.id != 0 && slot.proc.is_running()) {
                slot.proc.pump_capture_nonblocking();
                handles.push_back(static_cast<HANDLE>(slot.proc.pid()));
                ids.push_back(slot.id);
            }
        }
        if (handles.empty()) {
            return std::unexpected(Err::erc(std::errc::no_child_process, "No running procs"));
        }
        DWORD batch = static_cast<DWORD>(std::min(handles.size(), static_cast<std::size_t>(MAXIMUM_WAIT_OBJECTS)));
        DWORD res = ::WaitForMultipleObjects(batch, handles.data(), FALSE, 10);
        if (res >= WAIT_OBJECT_0 && res < WAIT_OBJECT_0 + batch) {
            Proc_id fid = ids[res - WAIT_OBJECT_0];
            auto st = get(fid).value().get().wait();
            if (!st) {
                return std::unexpected(st.error());
            }
            return fid;
        }
        if (res == WAIT_FAILED) {
            return std::unexpected(Err::erno(GetLastError(), "WaitForMultipleObjects failed"));
        }
        // WAIT_TIMEOUT: loop and pump again.
    }
#else
    bool any_running = false;
    for (auto &slot : hive_) {
        if (slot.id != 0 && slot.proc.is_running()) {
            any_running = true;
            break;
        }
    }
    if (!any_running) {
        return std::unexpected(Err::erc(std::errc::no_child_process, "No running procs"));
    }
    bool any_capture = false;
    for (auto &slot : hive_) {
        if (slot.id != 0 && slot.proc.is_running() && slot.proc.has_capture()) {
            any_capture = true;
            break;
        }
    }
    if (!any_capture) {
        while (true) {
            int wstatus = 0;
            pid_t pid = ::waitpid(-gid_.val, &wstatus, 0);
            if (pid == -1) {
                if (errno == EINTR) {
                    continue;
                }
                return std::unexpected(Err::erno(errno, "waitpid on group failed"));
            }
            for (auto &slot : hive_) {
                if (slot.id != 0 && slot.proc.pid() == pid) {
                    slot.proc.status_ = Proc::parse_status(wstatus);
                    return slot.id;
                }
            }
        }
    }
    // With string capture anywhere in the group: pump every member while
    // polling for exits so no pipe fills and deadlocks a sibling.
    while (true) {
        for (auto &slot : hive_) {
            if (slot.id != 0 && slot.proc.is_running()) {
                slot.proc.pump_capture_nonblocking();
            }
        }
        int wstatus = 0;
        pid_t pid = ::waitpid(-gid_.val, &wstatus, WNOHANG);
        if (pid > 0) {
            for (auto &slot : hive_) {
                if (slot.id != 0 && slot.proc.pid() == pid) {
                    slot.proc.status_ = Proc::parse_status(wstatus);
                    return slot.id;
                }
            }
            continue;
        }
        if (pid == -1 && errno != EINTR) {
            return std::unexpected(Err::erno(errno, "waitpid on group failed"));
        }
        struct pollfd pfds[64];
        int n = 0;
        for (auto &slot : hive_) {
            if (slot.id == 0 || !slot.proc.is_running() || n >= 64) {
                continue;
            }
            if (slot.proc.cap_out_fd_ >= 0) {
                pfds[n].fd = slot.proc.cap_out_fd_;
                pfds[n].events = POLLIN;
                pfds[n].revents = 0;
                ++n;
                if (n >= 64) {
                    break;
                }
            }
            if (slot.proc.cap_err_fd_ >= 0 && slot.proc.cap_err_fd_ != slot.proc.cap_out_fd_) {
                pfds[n].fd = slot.proc.cap_err_fd_;
                pfds[n].events = POLLIN;
                pfds[n].revents = 0;
                ++n;
            }
        }
        if (n == 0) {
            details::sleep_ms(1);
        } else {
            ::poll(pfds, static_cast<nfds_t>(n), 10);
        }
    }
#endif
}
auto bld::Proc_group::signal(int sig) -> void
{
#ifdef _WIN32
    if (gid_.val) {
        ::TerminateJobObject(static_cast<HANDLE>(gid_.val), static_cast<UINT>(sig));
    }
#else
    if (gid_.val != Proc_gid::CREATE_NEW && gid_.val != Proc_gid::INVALID) {
        ::kill(-gid_.val, sig);
    }
#endif
}
auto bld::Proc_group::terminate(int sig) -> void
{
    signal(sig);
    hive_.clear();
    free_list_.clear();
    active_count_ = 0;
    ++generation_;
#ifndef _WIN32
    // Same as remove(): a drained pgid is dead, start fresh next spawn.
    gid_ = Proc_gid{Proc_gid::CREATE_NEW};
#endif
}

auto std::formatter<bld::Proc::Status>::format(const bld::Proc::Status &s, std::format_context &ctx) const -> std::format_context::iterator
{
    auto out = ctx.out();
    switch (s.state) {
    case bld::Proc::State::running:
        out = std::format_to(out, "{{ running");
        break;
    case bld::Proc::State::exited:
        out = std::format_to(out, "{{ exited");
        break;
    case bld::Proc::State::signaled:
        out = std::format_to(out, "{{ signaled");
        break;
    case bld::Proc::State::stopped:
        out = std::format_to(out, "{{ stopped");
        break;
    case bld::Proc::State::continued:
        out = std::format_to(out, "{{ continued");
        break;
    default:
        std::unreachable();
    }
    switch (s.state) {
    case bld::Proc::State::exited:
        out = fmt == mode::debug ? std::format_to(out, ", code = {} }}", s.code) : std::format_to(out, ", {} }}", s.code);
        break;
    case bld::Proc::State::signaled:
        out = fmt == mode::debug ? std::format_to(out, ", sig = {} }}", s.code) : std::format_to(out, ", {} }}", s.code);
        break;
    case bld::Proc::State::running:
    case bld::Proc::State::stopped:
    case bld::Proc::State::continued:
        out = std::format_to(out, " }}");
        break;
    default:
        std::unreachable();
    }
    return out;
}

auto std::formatter<bld::Proc>::format(const bld::Proc &p, std::format_context &ctx) const -> std::format_context::iterator
{
    auto out = ctx.out();
    if (show_debug) {
        if (show_pid) {
            out = std::format_to(out, "{{ pid = {}, label = \"{}\", status = {:?} }}", p.id_, p.spec.cfg.label, p.status_);
        } else {
            out = std::format_to(out, "{{ label = \"{}\", status = {:?} }}", p.spec.cfg.label, p.status_);
        }
    } else {
        if (show_pid) {
            out = std::format_to(out, "{{ {}, \"{}\", {} }}", p.id_, p.spec.cfg.label, p.status_);
        } else {
            out = std::format_to(out, "{{ \"{}\", {} }}", p.spec.cfg.label, p.status_);
        }
    }
    return out;
}

bld::Owned_Fd::~Owned_Fd()
{
    close();
}

bld::Owned_Fd::Owned_Fd(Owned_Fd &&other) noexcept : handle_(std::exchange(other.handle_, Fd_view::INVALID))
{}

auto bld::Owned_Fd::operator=(Owned_Fd &&other) noexcept -> Owned_Fd &
{
    if (this != &other) {
        close();
        handle_ = std::exchange(other.handle_, Fd_view::INVALID);
    }
    return *this;
}

auto bld::Owned_Fd::close() -> void
{
    if (handle_ != Fd_view::INVALID && handle_ != Fd_view::DEFAULT_IN && handle_ != Fd_view::DEFAULT_OUT && handle_ != Fd_view::DEFAULT_ERR) {
#ifdef _WIN32
        ::_close(handle_);
#else
        ::close(handle_);
#endif
        handle_ = Fd_view::INVALID;
    }
}

bld::Owned_Fd::operator Fd_view() const
{
    return Fd_view{handle_};
}

auto bld::Owned_Fd::open(std::string_view path, Open_mode mode) -> std::expected<Owned_Fd, bld::Err>
{
    if (path.empty()) {
        return std::unexpected(bld::Err::erc(std::errc::invalid_argument, "Cannot open an empty path"));
    }
    int flags = 0;
#ifdef _WIN32
    switch (mode) {
    case Open_mode::read:
        flags = _O_RDONLY | _O_BINARY;
        break;
    case Open_mode::write:
        flags = _O_WRONLY | _O_CREAT | _O_TRUNC | _O_BINARY;
        break;
    case Open_mode::append:
        flags = _O_WRONLY | _O_CREAT | _O_APPEND | _O_BINARY;
        break;
    }
    int fd = ::_open(std::string{path}.c_str(), flags, 0666);
#else
    switch (mode) {
    case Open_mode::read:
        flags = O_RDONLY;
        break;
    case Open_mode::write:
        flags = O_WRONLY | O_CREAT | O_TRUNC;
        break;
    case Open_mode::append:
        flags = O_WRONLY | O_CREAT | O_APPEND;
        break;
    }
    int fd = ::open(std::string{path}.c_str(), flags, 0666);
#endif
    if (fd == Fd_view::INVALID) {
        return std::unexpected(bld::Err::erno(errno, std::format("Failed to open file: '{}'", path)));
    }
    return Owned_Fd{fd};
}

auto bld::async::operator()(Proc_config &cfg) const -> void
{
    cfg.async = true;
}

auto bld::label::operator()(bld::Proc_config &cfg) const -> void
{
    cfg.label = val;
}

auto bld::label::operator()(bld::Capture_config &cfg) const -> void
{
    cfg.label = val;
}

auto bld::pipe::operator()(Proc_config &cfg) const -> void
{
    cfg.io_out = make_fd_slot(out, Fd_view::DEFAULT_OUT);
    cfg.io_err = make_fd_slot(err, Fd_view::DEFAULT_ERR);
    cfg.io_in = make_fd_slot(in, Fd_view::DEFAULT_IN);
    cfg.merge_err_and_out = merge_err_and_out;
}

auto bld::out_fd::operator()(bld::Proc_config &cfg) const -> void
{
    cfg.io_out = make_fd_slot(fd, Fd_view::DEFAULT_OUT);
}
auto bld::err_fd::operator()(bld::Proc_config &cfg) const -> void
{
    cfg.io_err = make_fd_slot(fd, Fd_view::DEFAULT_ERR);
}
auto bld::in_fd::operator()(bld::Proc_config &cfg) const -> void
{
    cfg.io_in = make_fd_slot(fd, Fd_view::DEFAULT_IN);
}
auto bld::in_fd::operator()(bld::Capture_config &cfg) const -> void
{
    cfg.io_in = make_fd_slot(fd, Fd_view::DEFAULT_IN);
}

auto bld::out_err_fd::operator()(bld::Proc_config &cfg) const -> void
{
    cfg.io_out = make_fd_slot(fd, Fd_view::DEFAULT_OUT);
    cfg.io_err = make_fd_slot(fd, Fd_view::DEFAULT_ERR);
    cfg.merge_err_and_out = true;
}

namespace bld::details {
// Shared eager-open helpers for out/err/in/out_err_file. Messages and log
// tags are parameters so the public wrappers keep byte-identical errors.
inline auto open_shared_for_write(std::string_view path, bld::Open_mode mode, std::string_view parent_err, std::string_view log_tag)
    -> std::expected<Shared_fd, bld::Err>
{
    std::filesystem::path p{path};
    if (p.has_parent_path() && !std::filesystem::exists(p.parent_path())) {
        return std::unexpected(bld::Err::erc(std::errc::no_such_file_or_directory, std::format("{} '{}' does not exist", parent_err, path)));
    }
    auto res = bld::Owned_Fd::open(path, mode);
    if (!res) {
        return std::unexpected(std::move(res.error()));
    }
    bld::log::d("{} {} (file: '{}')", log_tag, res->handle_, path);
    return std::make_shared<bld::Owned_Fd>(std::move(*res));
}

inline auto open_shared_for_read(std::string_view path) -> std::expected<Shared_fd, bld::Err>
{
    if (!std::filesystem::exists(path)) {
        return std::unexpected(bld::Err::erc(std::errc::no_such_file_or_directory, std::format("Path '{}' for reading input doesn't exist", path)));
    }
    auto res = bld::Owned_Fd::open(path, bld::Open_mode::read);
    if (!res) {
        return std::unexpected(std::move(res.error()));
    }
    bld::log::d("Opened in fd: {} (file: '{}')", res->handle_, path);
    return std::make_shared<bld::Owned_Fd>(std::move(*res));
}

inline void assign_single(Io_slot &slot, const Shared_fd &fd)
{
    if (fd && fd->handle_ != Fd_view::INVALID) {
        slot = Io_slot{fd};
    } else {
        slot = Io_slot{std::monostate{}};
    }
}

inline void assign_merged(Proc_config &cfg, const Shared_fd &fd)
{
    if (fd && fd->handle_ != Fd_view::INVALID) {
        cfg.io_out = Io_slot{fd};
        cfg.io_err = Io_slot{fd};
    } else {
        cfg.io_out = Io_slot{std::monostate{}};
        cfg.io_err = Io_slot{std::monostate{}};
    }
    cfg.merge_err_and_out = true;
}
} // namespace bld::details

auto bld::out_file::open(std::string_view path, bld::Open_mode mode) -> std::expected<bld::out_file, bld::Err>
{
    auto fd = bld::details::open_shared_for_write(path, mode, "Parent directory for output file", "Opened out fd:");
    if (!fd) {
        return std::unexpected(std::move(fd.error()));
    }
    bld::out_file f;
    f.fd = std::move(*fd);
    return f;
}
bld::out_file::out_file(std::string_view path, bld::Open_mode mode)
{
    auto res = open(path, mode);
    if (!res) {
        bld::log::f("Fatal: Could not open output file '{}': {}", path, res.error().msg);
        std::exit(EXIT_FAILURE);
    }
    fd = std::move(res->fd);
}
auto bld::out_file::operator()(bld::Proc_config &cfg) const -> void
{
    bld::details::assign_single(cfg.io_out, fd);
}

// --- err_file ---
auto bld::err_file::open(std::string_view path, bld::Open_mode mode) -> std::expected<bld::err_file, bld::Err>
{
    auto fd = bld::details::open_shared_for_write(path, mode, "Parent directory for error log", "Opened err fd:");
    if (!fd) {
        return std::unexpected(std::move(fd.error()));
    }
    bld::err_file f;
    f.fd = std::move(*fd);
    return f;
}
bld::err_file::err_file(std::string_view path, bld::Open_mode mode)
{
    auto res = open(path, mode);
    if (!res) {
        bld::log::f("Fatal: Could not open error file '{}': {}", path, res.error().msg);
        std::exit(EXIT_FAILURE);
    }
    fd = std::move(res->fd);
}
auto bld::err_file::operator()(bld::Proc_config &cfg) const -> void
{
    bld::details::assign_single(cfg.io_err, fd);
}

// --- in_file ---
auto bld::in_file::open(std::string_view path) -> std::expected<bld::in_file, bld::Err>
{
    auto fd = bld::details::open_shared_for_read(path);
    if (!fd) {
        return std::unexpected(std::move(fd.error()));
    }
    bld::in_file f;
    f.fd = std::move(*fd);
    return f;
}
bld::in_file::in_file(std::string_view path)
{
    auto res = open(path);
    if (!res) {
        bld::log::f("Fatal: Could not open input file '{}': {}", path, res.error().msg);
        std::exit(EXIT_FAILURE);
    }
    fd = std::move(res->fd);
}
auto bld::in_file::operator()(bld::Proc_config &cfg) const -> void
{
    bld::details::assign_single(cfg.io_in, fd);
}
auto bld::in_file::operator()(bld::Capture_config &cfg) const -> void
{
    bld::details::assign_single(cfg.io_in, fd);
}

// --- out_err_file ---
auto bld::out_err_file::open(std::string_view path, bld::Open_mode mode) -> std::expected<bld::out_err_file, bld::Err>
{
    auto fd = bld::details::open_shared_for_write(path, mode, "Parent directory for out/err log", "Opened out_err fd:");
    if (!fd) {
        return std::unexpected(std::move(fd.error()));
    }
    bld::out_err_file f;
    f.fd = std::move(*fd);
    return f;
}
bld::out_err_file::out_err_file(std::string_view path, bld::Open_mode mode)
{
    auto res = open(path, mode);
    if (!res) {
        bld::log::f("Fatal: Could not open out/err file '{}': {}", path, res.error().msg);
        std::exit(EXIT_FAILURE);
    }
    fd = std::move(res->fd);
}
auto bld::out_err_file::operator()(bld::Proc_config &cfg) const -> void
{
    bld::details::assign_merged(cfg, fd);
}

auto bld::lazy_out_file::operator()(bld::Proc_config &cfg) const -> void
{
    cfg.io_out = make_path_slot(path);
}

auto bld::lazy_err_file::operator()(bld::Proc_config &cfg) const -> void
{
    cfg.io_err = make_path_slot(path);
}

auto bld::lazy_out_err_file::operator()(bld::Proc_config &cfg) const -> void
{
    cfg.io_out = make_path_slot(path);
    cfg.io_err = make_path_slot(path);
    cfg.merge_err_and_out = true;
}

auto bld::lazy_in_file::operator()(bld::Proc_config &cfg) const -> void
{
    cfg.io_in = make_path_slot(path);
}

auto bld::lazy_in_file::operator()(bld::Capture_config &cfg) const -> void
{
    cfg.io_in = make_path_slot(path);
}

auto bld::out_str::operator()(bld::Proc_config &cfg) const -> void
{
    cfg.io_out = make_str_slot(ptr);
}

auto bld::err_str::operator()(bld::Proc_config &cfg) const -> void
{
    cfg.io_err = make_str_slot(ptr);
}

auto bld::out_err_str::operator()(bld::Proc_config &cfg) const -> void
{
    cfg.io_out = make_str_slot(ptr);
    cfg.io_err = make_str_slot(ptr);
    cfg.merge_err_and_out = true;
}

bld::in_str::in_str(std::string_view s) : val(s)
{}

auto bld::in_str::operator()(Capture_config &cfg) const -> void
{
    cfg.in_str = val;
}

auto bld::raw_crlf::operator()(Capture_config &cfg) const -> void
{
    cfg.normalize_crlf = false;
}

auto bld::cwd::operator()(Proc_config &cfg) const -> void
{
    cfg.cwd = path;
}

auto bld::Plan::add(Task task) -> Task &
{
    if (task.name.empty()) {
        if (!task.spec.cmd.empty()) {
            task.name = task.spec.cmd.str();
        } else {
            task.name = "task";
        }
    }
    // dedup like before: output -> cmd
    std::string base = task.name;
    int n = 1;
    auto exists = [&](const std::string &s) {
        for (auto &t : tasks) {
            if (t.name == s) {
                return true;
            }
        }
        return false;
    };
    while (exists(task.name)) {
        task.name = std::format("{}#{}", base, n++);
    }
    tasks.push_back(std::move(task));
    return tasks.back();
}
auto bld::Plan::add(std::string name, Cmd cmd) -> Task &
{
    Exec_spec spec;
    spec.cmd = std::move(cmd);
    return add(std::move(name), std::move(spec));
}
auto bld::Plan::add(std::string name, Exec_spec spec) -> Task &
{
    Task t;
    t.name = std::move(name);
    t.spec = std::move(spec);
    return add(std::move(t));
}
auto bld::Plan::needs(std::string_view task, std::string_view input) -> void
{
    task_inputs[std::string{task}].emplace_back(input);
}
auto bld::Plan::needs_from(std::string_view task, std::initializer_list<std::string_view> inputs) -> void
{
    for (auto s : inputs) {
        needs(task, s);
    }
}
auto bld::Plan::produces(std::string_view task, std::string_view output) -> void
{
    task_outputs[std::string{task}].emplace_back(output);
}
auto bld::Plan::produces_to(std::string_view task, std::initializer_list<std::string_view> outputs) -> void
{
    for (auto s : outputs) {
        produces(task, s);
    }
}
auto bld::Plan::after(std::string_view task, std::string_view dep) -> void
{
    task_after[std::string{task}].emplace_back(dep);
}
auto bld::Plan::mark_compile_command(std::string_view task) -> void
{
    compile_commands.insert(std::string{task});
}
auto bld::jobs::operator()(Run_config &cfg) const -> void
{
    cfg.use_threads = value;
}
auto bld::max_async::operator()(Run_config &cfg) const -> void
{
    cfg.max_async = value;
}
auto bld::deduce_dependency::operator()(Run_config &cfg) const -> void
{
    cfg.deduce_dependency = true;
}
auto bld::keep_going::operator()(Run_config &cfg) const -> void
{
    cfg.failure_policy = Failure_policy::keep_going;
}
auto bld::dry_run::operator()(Run_config &cfg) const -> void
{
    cfg.dry_run = true;
}
auto bld::dry_run::operator()(Proc_config &cfg) const -> void
{
    cfg.dry_run = true;
}
auto bld::dry_run::operator()(Capture_config &cfg) const -> void
{
    cfg.dry_run = true;
}
auto bld::force::operator()(Run_config &cfg) const -> void
{
    cfg.force = true;
}
auto bld::write_compile_commands::operator()(Run_config &cfg) const -> void
{
    cfg.write_compile_commands = path;
}
auto bld::Task::needs(std::string_view input) -> Task &
{
    inputs.emplace_back(input);
    return *this;
}
auto bld::Task::needs_from(std::initializer_list<std::string_view> ins) -> Task &
{
    for (auto s : ins) {
        inputs.emplace_back(s);
    }
    return *this;
}
auto bld::Task::produces(std::string_view output) -> Task &
{
    outputs.emplace_back(output);
    return *this;
}
auto bld::Task::produces_to(std::initializer_list<std::string_view> outs) -> Task &
{
    for (auto s : outs) {
        outputs.emplace_back(s);
    }
    return *this;
}
auto bld::Task::after_dep(std::string_view dep) -> Task &
{
    after.emplace_back(dep);
    return *this;
}

// IMPL SECTION 04 — Execution (bld::run, bld::capture, bld::Task)
auto bld::details::execute(const bld::Cmd &cmd, const Proc_config &cfg, std::source_location loc) -> std::expected<bld::Proc, bld::Err>
{
    auto log_slot = [](std::string_view name, const Io_slot &slot, const bld::Cmd &c) {
        if (std::holds_alternative<std::monostate>(slot)) {
            return;
        }
        if (auto *f = std::get_if<Fd_view>(&slot)) {
            bld::log::d("Routing {} from fd: {} for cmd: {:?}", name, f->val, c);
        } else if (auto *p = std::get_if<std::string>(&slot)) {
            bld::log::d("Routing {} from path: '{}' for cmd: {:?}", name, *p, c);
        } else if (auto *s = std::get_if<Shared_fd>(&slot)) {
            int v = (s && *s) ? (*s)->handle_ : Fd_view::INVALID;
            bld::log::d("Routing {} from owned fd: {} for cmd: {:?}", name, v, c);
        } else if (std::holds_alternative<std::string *>(slot)) {
            bld::log::d("Capturing {} into string for cmd: {:?}", name, c);
        }
    };
    log_slot("input", cfg.io_in, cmd);
    log_slot("output", cfg.io_out, cmd);
    log_slot("error", cfg.io_err, cmd);
    if (cfg.dry_run) {
        // Preview only: no cwd validation, no spawn, no side effects.
        // The dummy Proc reports exited/0 so status checks see success.
        bld::log::i("dry run (would execute {:?})", cmd);
        Exec_spec dry_spec;
        dry_spec.cmd = cmd;
        dry_spec.cfg = cfg;
        if (dry_spec.cfg.label.empty()) {
            dry_spec.cfg.label = cmd.str();
        }
        Proc dry_proc;
        dry_proc.spec = std::move(dry_spec);
        return dry_proc;
    }
    bld::log::i("Executing command: {:?}", cmd);

    Exec_spec spec;
    spec.cmd = cmd;
    spec.cfg = cfg;
    return bld::Proc::spawn(spec)
        .transform_error([&loc](bld::Err err) {
            bld::log::e("at: {}:{}: {}", loc.file_name(), loc.line(), err);
            return err;
        })
        .and_then([&cfg, &loc](bld::Proc proc) -> std::expected<bld::Proc, bld::Err> {
            if (!cfg.async) {
                auto status = proc.wait();
                if (!status) {
                    bld::log::e("at: {}:{}: Wait failed: {}", loc.file_name(), loc.line(), status.error());
                    return std::unexpected(status.error());
                }
                return proc;
            }
            return proc;
        });
}

namespace bld::details {
// One round of stdin feeding for capture_execute's pump loop. Write semantics
// are OS-specific and preserved verbatim; close_in() is invoked when stdin
// reaches EOF, the child closes the pipe, or a fatal error occurs.
template <typename Close_fn>
inline void feed_capture_stdin(int write_fd, std::string_view in_str, std::size_t &written, Close_fn close_in)
{
#ifdef _WIN32
    // CRT pipes are blocking: feed in small chunks so a full pipe
    // stalls for at most one chunk while the child catches up.
    // True non-blocking stdin would need overlapped I/O.
    constexpr unsigned int chunk = 4096;
    std::size_t left = in_str.size() - written;
    unsigned int want = left < chunk ? static_cast<unsigned int>(left) : chunk;
    int n = details::write_fd(write_fd, in_str.data() + written, want);
    if (n > 0) {
        written += static_cast<std::size_t>(n);
    } else if (n == 0) {
        close_in();
    } else if (errno == EPIPE || errno == EINVAL) {
        close_in(); // child closed stdin / exited
    }
    if (written >= in_str.size()) {
        close_in(); // child sees EOF on stdin
    }
#else
    while (written < in_str.size()) {
        unsigned int left = static_cast<unsigned int>(in_str.size() - written);
        int n = details::write_fd(write_fd, in_str.data() + written, left);
        if (n > 0) {
            written += static_cast<std::size_t>(n);
            continue;
        }
        if (n == -1 && errno == EINTR) {
            continue;
        }
        if (n == -1 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
            break; // pipe full: read more output, retry next round
        }
        break; // EPIPE (child exited) or fatal: stop feeding
    }
    if (written >= in_str.size()) {
        close_in(); // child sees EOF on stdin
    }
#endif
}

// Idle-wait slice for capture_execute's pump loop: poll the live fds so the
// parent wakes when the child writes, without busy-spinning.
inline void rest_capture_loop(bool out_eof, bool in_closed, bool child_done, [[maybe_unused]] int out_fd, [[maybe_unused]] int in_fd)
{
#ifndef _WIN32
    if (!out_eof || !in_closed) {
        struct pollfd pfds[2];
        int n = 0;
        if (!out_eof) {
            pfds[n].fd = out_fd;
            pfds[n].events = POLLIN;
            pfds[n].revents = 0;
            ++n;
        }
        if (!in_closed) {
            pfds[n].fd = in_fd;
            pfds[n].events = POLLOUT;
            pfds[n].revents = 0;
            ++n;
        }
        ::poll(pfds, static_cast<nfds_t>(n), 10);
    } else if (!child_done) {
        details::sleep_ms(1);
    }
#else
    if (!out_eof || !in_closed || !child_done) {
        details::sleep_ms(1);
    }
#endif
}
} // namespace bld::details

auto bld::details::capture_execute(const bld::Cmd &cmd, bld::Capture_config &cap_cfg, std::source_location loc)
    -> std::expected<std::string, bld::Err>
{
    bld::log::d("Setting up merged capture for cmd: {:?}", cmd);

    // Only stdin modifiers are allowed (enforced at compile time via
    // validate_capture_configs). Guard hand-built Capture_config too:
    // io_in slot and in_str are mutually exclusive.
    if (!cap_cfg.in_str.empty() && io_is_set(cap_cfg.io_in)) {
        bld::log::e("capture {:?}: conflicting stdin routing (at most one of in_fd/in_file/lazy_in_file/in_str)", cmd);
        return std::unexpected(
            bld::Err::erc(
                std::errc::invalid_argument,
                "Conflicting stdin routing for capture: use at most one of in_fd/in_file/lazy_in_file/in_str"));
    }

    if (cap_cfg.dry_run) {
        // Preview only: no pipes, no spawn, empty output.
        bld::log::i("dry run (would capture {:?}, {} stdin bytes)", cmd, cap_cfg.in_str.size());
        return std::string{};
    }

    int pipe_out[2]{-1, -1}, pipe_in[2]{-1, -1};
    Proc_config run_cfg{};
    run_cfg.label = cap_cfg.label;
    run_cfg.async = true;
    run_cfg.loc = cap_cfg.loc;
    run_cfg.io_in = cap_cfg.io_in;

    if (!cap_cfg.in_str.empty()) {
        if (auto res = details::make_pipe(pipe_in, "stdin"); !res) {
            return std::unexpected(res.error());
        }
        run_cfg.io_in = Io_slot{Fd_view{pipe_in[0]}};
        bld::log::d("Created stdin pipe (read: {}, write: {})", pipe_in[0], pipe_in[1]);
    }

    // Merged capture: stdout goes to a pipe, stderr is merged into it.
    if (auto res = details::make_pipe(pipe_out, "stdout"); !res) {
        if (!cap_cfg.in_str.empty()) {
            details::close_fd(pipe_in[0]);
            details::close_fd(pipe_in[1]);
        }
        return std::unexpected(res.error());
    }
    run_cfg.io_out = Io_slot{Fd_view{pipe_out[1]}};
    run_cfg.merge_err_and_out = true;
    bld::log::d("Created merged stdout+stderr pipe (read: {}, write: {})", pipe_out[0], pipe_out[1]);

    auto proc_res = bld::details::execute(cmd, run_cfg, loc);

    // The parent must immediately close the child's ends of the pipes,
    // otherwise the pump loop below never sees EOF.
    details::close_fd(pipe_out[1]);
    pipe_out[1] = -1;
    if (!cap_cfg.in_str.empty()) {
        details::close_fd(pipe_in[0]);
        pipe_in[0] = -1;
    }

    if (!proc_res) {
        details::close_fd(pipe_out[0]);
        if (!cap_cfg.in_str.empty()) {
            details::close_fd(pipe_in[1]);
        }
        return std::unexpected(proc_res.error());
    }

    // Single-threaded pump: interleave stdin writes, stdout reads and
    // child reaping so large inputs+outputs never deadlock (no threads).
    auto &proc = *proc_res;
    std::string merged;
    const bool have_in = !cap_cfg.in_str.empty();
    std::size_t written = 0;
    bool in_closed = !have_in;
    bool out_eof = false;
    bool child_done = false;
    bld::Proc::Status child_status{};
    details::set_nonblocking(pipe_out[0]);
    if (have_in) {
        details::set_nonblocking(pipe_in[1]);
    }
    auto close_in = [&]() {
        if (pipe_in[1] >= 0) {
            details::close_fd(pipe_in[1]);
            pipe_in[1] = -1;
        }
        in_closed = true;
    };
    auto close_out = [&]() {
        if (pipe_out[0] >= 0) {
            details::close_fd(pipe_out[0]);
            pipe_out[0] = -1;
        }
        out_eof = true;
    };
    while (!out_eof || !in_closed || !child_done) {
        // Drain output first so the child never stalls on a full pipe
        // while we are trying to feed it stdin (matters on Windows where
        // _write can block: pipe_out was just drained, giving room).
        if (!out_eof) {
            if (details::pump_fd_nonblocking(pipe_out[0], merged)) {
                close_out();
            }
        }
        if (!in_closed) {
            details::feed_capture_stdin(pipe_in[1], cap_cfg.in_str, written, close_in);
        }
        if (!child_done) {
            auto st = proc.try_wait();
            if (!st) {
                close_out();
                close_in();
                return std::unexpected(std::move(st.error()).with_payload(std::move(merged)));
            }
            if (!proc.is_running()) {
                child_done = true;
                child_status = *st;
            }
        }
        if (out_eof && in_closed && child_done) {
            break;
        }
        details::rest_capture_loop(out_eof, in_closed, child_done, pipe_out[0], pipe_in[1]);
    }

    if (cap_cfg.normalize_crlf) {
        merged = bld::str::replace_all(merged, "\r\n", "\n");
    }

    auto status = proc.wait();
    if (!status) {
        bld::log::e("capture {:?}: wait failed: {}", cmd, status.error());
        return std::unexpected(std::move(status.error()).with_payload(std::move(merged)));
    }
    (void)child_status;
    if (status->code != 0) {
        bld::log::e("capture {:?}: exited with code {}", cmd, status->code);
        auto err = bld::Err::erc(std::errc::io_error, std::format("command {:?} exited with code {}", cmd, status->code));
        err.payload = std::move(merged);
        return std::unexpected(std::move(err));
    }
    bld::log::d("capture {:?}: {} bytes", cmd, merged.size());
    return merged;
}

// IMPL SECTION 05 — Rebuild helpers (is_outdated, rebuild_this_when_needed)
auto bld::is_outdated(std::string_view target, std::string_view source) -> bool
{
    namespace fs = std::filesystem;
    std::error_code ec;

    if (!fs::exists(target, ec)) {
        bld::log::w("Target '{}' does not exist. Rebuild required.", target);
        return true;
    }

    auto target_time = fs::last_write_time(target, ec);
    if (ec) {
        bld::log::w("Failed to read timestamp for target '{}': {}. Defaulting to rebuild.", target, ec.message());
        return true;
    }

    if (!fs::exists(source, ec)) {
        bld::log::e("Source file missing: '{}'. Forcing rebuild.", source);
        return true;
    }

    auto source_time = fs::last_write_time(source, ec);
    if (ec) {
        bld::log::w("Failed to read timestamp for source '{}': {}. Defaulting to rebuild.", source, ec.message());
        return true;
    }

    bool outdated = target_time < source_time;

    if (outdated) {
        bld::log::i("Target '{}' is outdated relative to '{}'.", target, source);
    } else {
        bld::log::d("Target '{}' is up to date.", target);
    }

    return outdated;
}

auto bld::get_current_cxx_compiler() -> std::string_view
{
    using namespace std::literals::string_view_literals;

#ifdef __clang__
    auto compiler = "clang++"sv;
#elif defined(__GNUC__)
    auto compiler = "g++"sv;
#elif defined(_MSC_VER)
    auto compiler = "cl"sv;
#else
    auto compiler = "c++"sv;
#endif
    return compiler;
}

namespace bld::details {
// Shared body for rebuild_this_when_needed / _ext. Behavior-preserving:
// logs, renames running exe aside, compiles, rolls back on failure,
// then execs the fresh binary. `flags` are appended verbatim (empty for
// the non-ext variant). Only called after the outdated check passed.
inline auto rebuild_and_restart(
    const std::string &target,
    const std::string &source,
    const std::vector<std::string> &flags,
    std::string_view compiler,
    std::span<char *> args_span) -> std::expected<void, bld::Err>
{
    namespace fs = std::filesystem;
    bld::log::i("Rebuilding '{}' from '{}'", target, source);

    std::string old_target = target + ".old";
    std::error_code ec;

    fs::rename(target, old_target, ec);
    if (ec) {
        bld::log::w("Failed to rename currently running executable: {}", ec.message());
    }

    const char *cxx = nullptr;

    if (!compiler.empty()) {
        cxx = compiler.data();
    } else {
        cxx = bld::get_current_cxx_compiler().data();
    }

    bld::Cmd build_cmd{cxx, "-o", target, source, "-std=c++23", "-O3", "-Wall", "-Wextra"};
#ifdef _WIN32
#ifdef __GNUC__
    build_cmd.push("-lstdc++exp");
#endif
#endif

    for (const auto &f : flags) {
        build_cmd.push(f);
    }

    auto proc = bld::run(build_cmd);
    if (!proc || proc->status_.code != 0) {
        fs::rename(old_target, target, ec);
        if (!proc) {
            return std::unexpected(std::move(proc.error()));
        }
        return std::unexpected(
            bld::Err::erc(
                std::errc::operation_canceled,
                std::format("Failed to rebuild build script: compiler exited with code {}", proc->status_.code)));
    }

    bld::log::i("Successfully rebuilt! Restarting...");

    std::vector<char *> c_argv(args_span.begin(), args_span.end());
    c_argv.push_back(nullptr);

#ifdef _WIN32
    ::_execvp(target.c_str(), c_argv.data());
#else
    ::execvp(target.c_str(), c_argv.data());
#endif

    return std::unexpected(bld::Err::erno(errno, "Failed to restart build script after compilation"));
}
} // namespace bld::details

auto bld::rebuild_this_when_needed(int argc, char **argv, std::string_view compiler, std::source_location loc) -> std::expected<void, bld::Err>
{
    if (argv == nullptr || argc <= 0) {
        return {};
    }

    std::span<char *> args_span{argv, static_cast<std::size_t>(argc)};
    std::string target = bld::add_exe_on_win32(args_span[0]);

    std::string source = loc.file_name();

    if (!bld::is_outdated(target, source)) {
        return {};
    }

    return bld::details::rebuild_and_restart(target, source, {}, compiler, args_span);
}

auto bld::rebuild_this_when_needed_ext(int argc, char **argv, std::vector<std::string> flags, std::string_view compiler, std::source_location loc)
    -> std::expected<void, bld::Err>
{
    if (argv == nullptr || argc <= 0) {
        return {};
    }

    std::span<char *> args_span{argv, static_cast<std::size_t>(argc)};
    std::string target = bld::add_exe_on_win32(args_span[0]);

    std::string source = loc.file_name();

    if (!bld::is_outdated(target, std::array<std::string_view, 2>{source, std::string_view(__FILE__)})) {
        return {};
    }

    return bld::details::rebuild_and_restart(target, source, flags, compiler, args_span);
}

auto bld::wait_all(std::span<bld::Proc> procs) -> std::expected<std::size_t, bld::Err>
{
    // Single-threaded pump: round-robin try_wait() (each pumps its own
    // capture pipes) so concurrent out_str/err_str captures never deadlock
    // on a full pipe while another child is being reaped.
#ifdef _WIN32
    std::vector<bld::Proc *> pending;
    for (auto &proc : procs) {
        if (proc.is_running() && proc.pid() != nullptr) {
            pending.push_back(&proc);
        }
    }
    const std::size_t total = pending.size();
    if (total == 0) {
        return 0;
    }
    bld::log::i("Waiting for {} processes, asynchronously", total);
    std::size_t completed = 0;
    bool has_errors = false;
    while (!pending.empty()) {
        bool progressed = false;
        for (auto it = pending.begin(); it != pending.end();) {
            bld::Proc *p = *it;
            auto st = p->try_wait();
            if (!st) {
                return std::unexpected(std::move(st.error()));
            }
            if (!p->is_running()) {
                ++completed;
                int percentage = static_cast<int>((completed * 100) / total);
                if (p->status_code() != 0) {
                    bld::log::e("[{:>3}%] Process '{}' failed: exited with code {}", percentage, p->spec.cfg.label, p->status_code());
                    has_errors = true;
                } else {
                    bld::log::i("[{:>3}%] Process '{}': completed.", percentage, p->spec.cfg.label);
                }
                it = pending.erase(it);
                progressed = true;
            } else {
                ++it;
            }
        }
        if (!pending.empty() && !progressed) {
            details::sleep_ms(1);
        }
    }
    if (has_errors) {
        return std::unexpected(bld::Err::erc(std::errc::operation_canceled, "One or more async processes failed").with_payload(completed));
    }
    return completed;

#else
    std::size_t remaining{0};
    for (const auto &proc : procs) {
        if (proc.is_running()) {
            remaining++;
        }
    }
    const std::size_t total = remaining;
    if (total == 0) {
        return {};
    }
    bld::log::i("Waiting for {} processes, asynchronously", total);
    std::size_t completed{0};
    bool has_errors = false;
    while (remaining > 0) {
        bool progressed = false;
        for (auto &proc : procs) {
            if (!proc.is_running()) {
                continue;
            }
            auto st = proc.try_wait();
            if (!st) {
                return std::unexpected(std::move(st.error()));
            }
            if (!proc.is_running()) {
                remaining--;
                completed = total - remaining;
                int percentage = static_cast<int>((completed * 100) / total);
                if (st->code != 0) {
                    bld::log::e(
                        "[{:>3}%] Process '{}' failed (pid: {}): exited with code {}",
                        percentage,
                        proc.spec.cfg.label,
                        proc.pid(),
                        st->code);
                    has_errors = true;
                } else {
                    bld::log::i("[{:>3}%] Process '{}' (pid: {}): completed.", percentage, proc.spec.cfg.label, proc.pid());
                }
                progressed = true;
            }
        }
        if (remaining > 0 && !progressed) {
            details::sleep_ms(1);
        }
    }
    if (has_errors) {
        return std::unexpected(bld::Err::erc(std::errc::operation_canceled, "One or more async processes failed").with_payload(completed));
    }
    return completed;
#endif
}

namespace bld::details {
namespace {
struct Json_reader
{
    std::string_view text;
    std::size_t pos{0};
    auto ws() -> void
    {
        while (pos < text.size() && (text[pos] == ' ' || text[pos] == '\n' || text[pos] == '\r' || text[pos] == '\t')) {
            ++pos;
        }
    }
    auto take(char c) -> bool
    {
        ws();
        if (pos < text.size() && text[pos] == c) {
            ++pos;
            return true;
        }
        return false;
    }
    auto string() -> std::expected<std::string, bld::Err>
    {
        if (!take('\"')) {
            return std::unexpected(bld::Err::erc(std::errc::invalid_argument, "expected JSON string"));
        }
        std::string out;
        while (pos < text.size()) {
            char c = text[pos++];
            if (c == '\"') {
                return out;
            }
            if (c != '\\') {
                out += c;
                continue;
            }
            if (pos == text.size()) {
                break;
            }
            switch (text[pos++]) {
            case '\"':
                out += '\"';
                break;
            case '\\':
                out += '\\';
                break;
            case '/':
                out += '/';
                break;
            case 'b':
                out += '\b';
                break;
            case 'f':
                out += '\f';
                break;
            case 'n':
                out += '\n';
                break;
            case 'r':
                out += '\r';
                break;
            case 't':
                out += '\t';
                break;
            case 'u':
                if (pos + 4 > text.size()) {
                    return std::unexpected(bld::Err::erc(std::errc::invalid_argument, "truncated JSON unicode escape"));
                }
                // Compile databases are UTF-8. Preserve ASCII escapes and replace other code points safely.
                {
                    unsigned value = 0;
                    for (int i = 0; i != 4; ++i) {
                        char h = text[pos++];
                        value = value * 16
                            + (h >= '0' && h <= '9'       ? h - '0'
                                   : h >= 'a' && h <= 'f' ? h - 'a' + 10
                                   : h >= 'A' && h <= 'F' ? h - 'A' + 10
                                                          : 16);
                        if (value > 0xFFFF) {
                            return std::unexpected(bld::Err::erc(std::errc::invalid_argument, "invalid JSON unicode escape"));
                        }
                    }
                    if (value < 0x80) {
                        out += static_cast<char>(value);
                    } else {
                        out += '?';
                    }
                }
                break;
            default:
                return std::unexpected(bld::Err::erc(std::errc::invalid_argument, "invalid JSON escape"));
            }
        }
        return std::unexpected(bld::Err::erc(std::errc::invalid_argument, "unterminated JSON string"));
    }
    auto skip_value() -> std::expected<void, bld::Err>
    {
        ws();
        if (pos == text.size()) {
            return std::unexpected(bld::Err::erc(std::errc::invalid_argument, "truncated JSON"));
        }
        if (text[pos] == '\"') {
            auto value = string();
            if (!value) {
                return std::unexpected(value.error());
            }
            return {};
        }
        if (text[pos] == '{' || text[pos] == '[') {
            const char open = text[pos++], close = open == '{' ? '}' : ']';
            int depth = 1;
            bool quoted = false;
            while (pos < text.size() && depth) {
                char c = text[pos++];
                if (quoted && c == '\\') {
                    ++pos;
                    continue;
                }
                if (c == '\"') {
                    quoted = !quoted;
                } else if (!quoted && c == open) {
                    ++depth;
                } else if (!quoted && c == close) {
                    --depth;
                }
            }
            return depth ? std::unexpected(bld::Err::erc(std::errc::invalid_argument, "unterminated JSON value")) : std::expected<void, bld::Err>{};
        }
        while (pos < text.size() && text[pos] != ',' && text[pos] != '}' && text[pos] != ']' && text[pos] != ' ' && text[pos] != '\n'
               && text[pos] != '\r' && text[pos] != '\t') {
            ++pos;
        }
        return {};
    }
    auto strings() -> std::expected<std::vector<std::string>, bld::Err>
    {
        if (!take('[')) {
            return std::unexpected(bld::Err::erc(std::errc::invalid_argument, "expected JSON array"));
        }
        std::vector<std::string> out;
        ws();
        if (take(']')) {
            return out;
        }
        do {
            auto value = string();
            if (!value) {
                return std::unexpected(value.error());
            }
            out.push_back(std::move(*value));
            ws();
        } while (take(','));
        if (!take(']')) {
            return std::unexpected(bld::Err::erc(std::errc::invalid_argument, "unterminated JSON array"));
        }
        return out;
    }
};

auto split_command(std::string_view command) -> std::vector<std::string>
{
    std::vector<std::string> args;
    std::string current;
    char quote = 0;
    bool escape = false;
    for (char c : command) {
        if (escape) {
            current += c;
            escape = false;
        } else if (c == '\\' && quote != '\'') {
            escape = true;
        } else if ((c == '\'' || c == '\"') && (quote == 0 || quote == c)) {
            quote = quote == 0 ? c : 0;
        } else if ((c == ' ' || c == '\t') && quote == 0) {
            if (!current.empty()) {
                args.push_back(std::move(current));
                current.clear();
            }
        } else {
            current += c;
        }
    }
    if (!current.empty()) {
        args.push_back(std::move(current));
    }
    return args;
}
auto escaped_json(std::string_view value) -> std::string
{
    std::string out;
    out.reserve(value.size() + 2);
    out += '\"';
    for (char c : value) {
        switch (c) {
        case '\\':
            out += "\\\\";
            break;
        case '\"':
            out += "\\\"";
            break;
        case '\n':
            out += "\\n";
            break;
        case '\r':
            out += "\\r";
            break;
        case '\t':
            out += "\\t";
            break;
        default:
            out += c;
        }
    }
    out += '\"';
    return out;
}
auto path_for(const bld::Task &task, std::string_view path) -> std::string
{
    if (task.spec.cfg.cwd.empty() || std::filesystem::path{path}.is_absolute()) {
        return std::string{path};
    }
    return (std::filesystem::path{task.spec.cfg.cwd} / std::filesystem::path{path}).string();
}
// Shared JSON entry writer for write_database(span) / write_database(Plan).
// Byte-identical output: handles leading comma, directory/file/arguments,
// and optional "output" field when `output` is non-null.
inline void append_compile_entry(
    std::string &json,
    bool &first,
    std::string_view cwd,
    std::string_view source,
    const std::vector<std::string> &args,
    const std::string *output)
{
    if (!first) {
        json += ",\n";
    }
    first = false;
    json += std::format(
        "  {{\"directory\":{},\"file\":{},\"arguments\":[", escaped_json(cwd.empty() ? "." : cwd), escaped_json(source));
    for (std::size_t i = 0; i < args.size(); ++i) {
        if (i) {
            json += ',';
        }
        json += escaped_json(args[i]);
    }
    json += ']';
    if (output != nullptr) {
        json += std::format(",\"output\":{}", escaped_json(*output));
    }
    json += '}';
}
auto write_database(std::span<bld::Task> tasks, std::string_view path) -> std::expected<void, bld::Err>
{
        // For span<Task> (Task carries no dep info), treat all as compile commands (no is_compile_command flag)
    std::string json{"[\n"};
    bool first = true;
    for (const auto &task : tasks) {
        // For span<Task>, inputs/outputs are not in Task, so source/output unknown — use name as file if needed
        append_compile_entry(json, first, task.spec.cfg.cwd, task.name, task.spec.cmd.args_, nullptr);
    }
    json += "\n]\n";
    return bld::fs::write_file(path, json);
}
auto write_database(Plan &plan, std::string_view path) -> std::expected<void, bld::Err>
{
    std::string json{"[\n"};
    bool first = true;
    for (const auto &task : plan.tasks) {
        if (!plan.compile_commands.contains(task.name)) {
            continue;
        }
        auto it_in = plan.task_inputs.find(task.name);
        std::string source = (it_in != plan.task_inputs.end() && !it_in->second.empty()) ? it_in->second.front() : task.name;
        auto it_out = plan.task_outputs.find(task.name);
        const std::string *output = (it_out != plan.task_outputs.end() && !it_out->second.empty()) ? &it_out->second.front() : nullptr;
        append_compile_entry(json, first, task.spec.cfg.cwd, source, task.spec.cmd.args_, output);
    }
    json += "\n]\n";
    return bld::fs::write_file(path, json);
}
} // namespace

auto load_compile_commands(const bld::Compilation_database &database) -> std::expected<std::vector<bld::Task>, bld::Err>
{
    auto file = bld::fs::read_file(database.path);
    if (!file) {
        return std::unexpected(file.error());
    }
    Json_reader reader{*file};
    if (!reader.take('[')) {
        return std::unexpected(bld::Err::erc(std::errc::invalid_argument, "compile_commands.json must contain an array"));
    }
    std::vector<bld::Task> tasks;
    reader.ws();
    while (!reader.take(']')) {
        if (!reader.take('{')) {
            return std::unexpected(bld::Err::erc(std::errc::invalid_argument, "compile_commands entry must be an object"));
        }
        std::string directory, file_name, command, output;
        std::vector<std::string> arguments;
        reader.ws();
        while (!reader.take('}')) {
            auto key = reader.string();
            if (!key || !reader.take(':')) {
                return std::unexpected(bld::Err::erc(std::errc::invalid_argument, "invalid compile_commands entry"));
            }
            if (*key == "arguments") {
                auto value = reader.strings();
                if (!value) {
                    return std::unexpected(value.error());
                }
                arguments = std::move(*value);
            } else if (*key == "directory" || *key == "file" || *key == "command" || *key == "output") {
                auto value = reader.string();
                if (!value) {
                    return std::unexpected(value.error());
                }
                if (*key == "directory") {
                    directory = std::move(*value);
                } else if (*key == "file") {
                    file_name = std::move(*value);
                } else if (*key == "command") {
                    command = std::move(*value);
                } else {
                    output = std::move(*value);
                }
            } else {
                auto ignored = reader.skip_value();
                if (!ignored) {
                    return std::unexpected(ignored.error());
                }
            }
            reader.ws();
            if (!reader.take(',')) {
                if (!reader.take('}')) {
                    return std::unexpected(bld::Err::erc(std::errc::invalid_argument, "unterminated compile_commands object"));
                }
                break;
            }
        }
        if (arguments.empty()) {
            arguments = split_command(command);
        }
        if (arguments.empty() || file_name.empty()) {
            return std::unexpected(bld::Err::erc(std::errc::invalid_argument, "compile_commands entry requires file and arguments or command"));
        }
        bld::Task task;
        task.name = file_name;
        task.spec.cmd.args_ = std::move(arguments);
        task.spec.cfg.cwd = std::move(directory);
        // Record file-level deps so run(tasks, deduce_dependency{}) can order them.
        task.inputs.push_back(file_name);
        if (!output.empty()) {
            task.outputs.push_back(std::move(output));
        } else if (database.infer_outputs) {
            // Infer "-o <file>" from the argument list.
            for (std::size_t i = 0; i + 1 < task.spec.cmd.args_.size(); ++i) {
                if (task.spec.cmd.args_[i] == "-o") {
                    task.outputs.push_back(task.spec.cmd.args_[i + 1]);
                    break;
                }
            }
        }
        tasks.push_back(std::move(task));
        reader.ws();
        if (!reader.take(',')) {
            reader.ws();
            if (!reader.take(']')) {
                return std::unexpected(bld::Err::erc(std::errc::invalid_argument, "unterminated compile_commands array"));
            }
            break;
        }
    }
    return tasks;
}

// Shared scheduler pieces for run_tasks/run_plan. Behavior-preserving:
// identical messages, ordering, and progress format; graph + dirty vectors
// are built by the callers, execution is unified here.
inline void ensure_task_names(std::span<bld::Task> tasks)
{
    std::unordered_set<std::string> used;
    for (auto &t : tasks) {
        if (!t.name.empty()) {
            used.insert(t.name);
        }
    }
    for (auto &t : tasks) {
        if (t.name.empty()) {
            std::string base = !t.spec.cmd.empty() ? t.spec.cmd.str() : "task";
            if (base.empty()) {
                base = "task";
            }
            std::string cand = base;
            int n = 1;
            while (used.find(cand) != used.end()) {
                cand = std::format("{}#{}", base, n++);
            }
            t.name = cand;
            used.insert(cand);
        }
    }
}

inline auto check_empty_commands(std::span<bld::Task> tasks) -> std::expected<void, bld::Err>
{
    for (auto &t : tasks) {
        if (t.spec.cmd.empty()) {
            return std::unexpected(Err::erc(std::errc::invalid_argument, std::format("task '{}' has an empty command", t.name)));
        }
    }
    return {};
}

[[nodiscard]] inline auto resolve_sched_width(const Run_config &cfg) -> std::size_t
{
    std::size_t parallel_budget = resolve_parallel_width(cfg.use_threads);
    std::size_t async_cap = resolve_async_cap(cfg.max_async, parallel_budget);
    std::size_t width = parallel_budget < async_cap ? parallel_budget : async_cap;
    if (width == 0) {
        width = 1;
    }
    return width;
}

inline auto topo_sort(const std::vector<std::vector<std::size_t>> &children, const std::vector<std::size_t> &indegree)
    -> std::expected<std::vector<std::size_t>, bld::Err>
{
    auto pending_deps = indegree;
    std::vector<std::size_t> topo, ready;
    for (std::size_t i = 0; i < indegree.size(); ++i) {
        if (pending_deps[i] == 0) {
            ready.push_back(i);
        }
    }
    for (std::size_t cursor = 0; cursor < ready.size(); ++cursor) {
        auto current = ready[cursor];
        topo.push_back(current);
        for (auto child : children[current]) {
            if (--pending_deps[child] == 0) {
                ready.push_back(child);
            }
        }
    }
    if (topo.size() != indegree.size()) {
        return std::unexpected(bld::Err::erc(std::errc::invalid_argument, "task dependency graph contains a cycle"));
    }
    return topo;
}


// Unified execution loop: queue + Proc_group + progress + cancel tail.
// `result` enters with pre-marked "up to date" skips and accumulates ran/failed.
inline auto run_schedule(
    std::span<bld::Task> tasks,
    const std::vector<std::vector<std::size_t>> &children,
    const std::vector<std::size_t> &indegree,
    const std::vector<bool> &dirty,
    const Run_config &cfg,
    Run_result &result,
    std::size_t width) -> std::expected<Run_result, bld::Err>
{
    std::vector<std::size_t> remaining = indegree, queue;
    for (std::size_t i = 0; i < tasks.size(); ++i) {
        if (remaining[i] == 0) {
            queue.push_back(i);
        }
    }
    // All waiting goes through Proc_group::wait_any, which blocks until any
    // child in the group exits. No polling, no timeouts.
    struct Active
    {
        std::size_t index;
        bld::Proc_id pid;
        std::chrono::steady_clock::time_point started;
    };
    bld::Proc_group group;
    std::vector<Active> active;
    bool stop = false;
    std::size_t cursor = 0;
    auto release = [&](std::size_t i) {
        for (auto child : children[i]) {
            if (--remaining[child] == 0) {
                queue.push_back(child);
            }
        }
    };
    auto find_active = [&](bld::Proc_id pid) -> std::size_t {
        for (std::size_t k = 0; k < active.size(); ++k) {
            if (active[k].pid == pid) {
                return k;
            }
        }
        return active.size();
    };
    // Progress so far: terminal tasks (ran/failed/skipped/cancelled) over all
    // tasks, as a 0-100 percentage in wait_all's "[ 50%]" style.
    std::size_t cancelled = 0;
    auto progress_pct = [&]() -> int {
        if (tasks.empty()) {
            return 100;
        }
        std::size_t settled = result.ran + result.failed + result.skipped + cancelled;
        if (settled > tasks.size()) {
            settled = tasks.size();
        }
        return static_cast<int>((settled * 100) / tasks.size());
    };
    while (cursor < queue.size() || !active.empty()) {
        while (!stop && active.size() < width && cursor < queue.size()) {
            const auto i = queue[cursor++];
            if (!dirty[i]) {
                release(i);
                continue;
            }
            if (cfg.dry_run) {
                result.tasks[i].state = bld::Task_state::skipped;
                result.tasks[i].message = "dry run";
                ++result.skipped;
                bld::log::i("[{:>3}%] Task '{}': dry run (would execute {:?})", progress_pct(), tasks[i].name, tasks[i].spec.cmd);
                release(i);
                continue;
            }
            auto pid = group.run_new(tasks[i]);
            if (!pid) {
                result.tasks[i].state = bld::Task_state::failed;
                result.tasks[i].message = pid.error().msg;
                ++result.failed;
                bld::log::e("[{:>3}%] Task '{}': spawn failed: {}", progress_pct(), tasks[i].name, pid.error().msg);
                if (cfg.failure_policy == bld::Failure_policy::stop) {
                    stop = true;
                }
                continue;
            }
            result.tasks[i].state = bld::Task_state::running;
            bld::log::d("task '{}': spawned {:?}", tasks[i].name, tasks[i].spec.cmd);
            active.push_back(Active{i, *pid, std::chrono::steady_clock::now()});
        }
        if (active.empty()) {
            break;
        }
        auto done = group.wait_any();
        if (!done) {
            bld::log::e("scheduler: wait failed: {}", done.error().msg);
            for (auto &entry : active) {
                result.tasks[entry.index].state = bld::Task_state::failed;
                result.tasks[entry.index].message = done.error().msg;
                ++result.failed;
                bld::log::e("[{:>3}%] Task '{}' failed: {}", progress_pct(), tasks[entry.index].name, done.error().msg);
                group.remove(entry.pid);
            }
            active.clear();
            break;
        }
        const auto k = find_active(*done);
        if (k == active.size()) {
            group.remove(*done);
            continue;
        }
        auto entry = active[k];
        active.erase(active.begin() + static_cast<std::ptrdiff_t>(k));
        auto got = group.get(entry.pid);
        if (!got) {
            result.tasks[entry.index].state = bld::Task_state::failed;
            result.tasks[entry.index].message = got.error().msg;
            ++result.failed;
            bld::log::e("[{:>3}%] Task '{}': lost proc: {}", progress_pct(), tasks[entry.index].name, got.error().msg);
            stop = cfg.failure_policy == bld::Failure_policy::stop;
            continue;
        }
        // wait_any already reaped; wait() returns the cached status (and joins drains).
        auto status = got->get().wait();
        group.remove(entry.pid);
        if (!status) {
            result.tasks[entry.index].state = bld::Task_state::failed;
            result.tasks[entry.index].message = status.error().msg;
            ++result.failed;
            bld::log::e("[{:>3}%] Task '{}': wait failed: {}", progress_pct(), tasks[entry.index].name, status.error().msg);
            stop = cfg.failure_policy == bld::Failure_policy::stop;
            continue;
        }
        auto &item = result.tasks[entry.index];
        item.status = *status;
        item.elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - entry.started);
        if (status->state != bld::Proc::State::exited || status->code != 0) {
            item.state = bld::Task_state::failed;
            item.message = std::format("exited with status {}", status->code);
            ++result.failed;
            bld::log::e("[{:>3}%] Task '{}' failed: {}", progress_pct(), tasks[entry.index].name, item.message);
            if (cfg.failure_policy == bld::Failure_policy::stop) {
                stop = true;
            }
        } else {
            item.state = bld::Task_state::succeeded;
            ++result.ran;
            bld::log::i("[{:>3}%] Task '{}': completed in {}ms.", progress_pct(), tasks[entry.index].name, item.elapsed.count());
            release(entry.index);
        }
    }
    for (std::size_t ci = 0; ci < result.tasks.size(); ++ci) {
        auto &item = result.tasks[ci];
        if (item.state == bld::Task_state::pending) {
            item.state = bld::Task_state::cancelled;
            item.message = "not scheduled after an earlier failure";
            ++cancelled;
            bld::log::w("[{:>3}%] Task '{}': cancelled (not scheduled after an earlier failure)", progress_pct(), tasks[ci].name);
        }
    }
    if (!result.ok()) {
        bld::log::e("run: {} ran, {} skipped, {} failed", result.ran, result.skipped, result.failed);
        return std::unexpected(
            bld::Err::erc(std::errc::operation_canceled, std::format("{} task(s) failed", result.failed)).with_payload(std::move(result)));
    }
    bld::log::i("run: {} ran, {} skipped, {} failed", result.ran, result.skipped, result.failed);
    return result;
}

auto run_tasks(std::span<bld::Task> tasks, const bld::Run_config &cfg) -> std::expected<bld::Run_result, bld::Err>
{
    // span<Task>: run all of them by default (independent, no graph).
    // With deduce_dependency, build a DAG from Task.inputs/outputs/after.
    // Waiting is done via Proc_group (procs owned by the group).
    if (!cfg.write_compile_commands.empty()) {
        auto written = write_database(tasks, cfg.write_compile_commands);
        if (!written) {
            return std::unexpected(written.error());
        }
    }
    bld::Run_result result{};
    result.tasks.resize(tasks.size());
    if (tasks.empty()) {
        return result;
    }
    // ensure names (output -> cmd) already done in Plan::add/run_tasks for span, but do here too for direct span
    ensure_task_names(tasks);
    // Every task needs a runnable command — name the culprit, not just the index.
    if (auto ok = check_empty_commands(tasks); !ok) {
        return std::unexpected(std::move(ok.error()));
    }
    // Single-threaded scheduler: up to <width> child processes live at once.
    // Effective width = min(resolved parallel width, resolved async cap).
    std::size_t width = resolve_sched_width(cfg);
    std::vector<std::vector<std::size_t>> children(tasks.size());
    std::vector<std::size_t> indegree(tasks.size(), 0);
    if (cfg.deduce_dependency) {
        std::unordered_map<std::string, std::size_t> producers, names;
        for (std::size_t i = 0; i < tasks.size(); ++i) {
            if (!names.emplace(tasks[i].name, i).second) {
                return std::unexpected(Err::erc(std::errc::invalid_argument, std::format("duplicate task name '{}'", tasks[i].name)));
            }
            for (auto &out : tasks[i].outputs) {
                if (!producers.emplace(out, i).second) {
                    return std::unexpected(Err::erc(std::errc::invalid_argument, std::format("multiple tasks produce '{}'", out)));
                }
            }
        }
        auto edge = [&](std::size_t from, std::size_t to) {
            for (auto c : children[from]) {
                if (c == to) {
                    return;
                }
            }
            children[from].push_back(to);
            ++indegree[to];
        };
        for (std::size_t i = 0; i < tasks.size(); ++i) {
            for (auto &in : tasks[i].inputs) {
                if (auto it = producers.find(in); it != producers.end()) {
                    if (it->second == i) {
                        return std::unexpected(Err::erc(
                            std::errc::invalid_argument,
                            std::format("task '{}' depends on its own output '{}'", tasks[i].name, in)));
                    }
                    edge(it->second, i);
                }
            }
            for (auto &dep : tasks[i].after) {
                auto it = names.find(dep);
                if (it == names.end()) {
                    return std::unexpected(
                        Err::erc(std::errc::invalid_argument, std::format("task '{}' depends on unknown task '{}'", tasks[i].name, dep)));
                }
                if (it->second == i) {
                    return std::unexpected(
                        Err::erc(std::errc::invalid_argument, std::format("task '{}' depends on itself", tasks[i].name)));
                }
                edge(it->second, i);
            }
        }
    } else {
        // else: all independent, no edges — but declared deps would be silently
        // ignored (and mis-ordered), which is almost certainly a bug.
        std::unordered_map<std::string, std::size_t> producers, names;
        for (std::size_t i = 0; i < tasks.size(); ++i) {
            names.emplace(tasks[i].name, i);
            for (auto &out : tasks[i].outputs) {
                producers.emplace(out, i);
            }
        }
        for (std::size_t i = 0; i < tasks.size(); ++i) {
            for (auto &in : tasks[i].inputs) {
                if (auto it = producers.find(in); it != producers.end() && it->second != i) {
                    return std::unexpected(Err::erc(
                        std::errc::invalid_argument,
                        std::format(
                            "task '{}' needs '{}' produced by '{}', but deduce_dependency is not set "
                            "(pass bld::deduce_dependency{{}} or use bld::Plan)",
                            tasks[i].name,
                            in,
                            tasks[it->second].name)));
                }
            }
            if (!tasks[i].after.empty()) {
                return std::unexpected(Err::erc(
                    std::errc::invalid_argument,
                    std::format(
                        "task '{}' declares after-dependencies, but deduce_dependency is not set "
                        "(pass bld::deduce_dependency{{}} or use bld::Plan)",
                        tasks[i].name)));
            }
        }
    }
    auto topo_res = topo_sort(children, indegree);
    if (!topo_res) {
        return std::unexpected(std::move(topo_res.error()));
    }
    auto topo = std::move(*topo_res);
    // Dirty: run-all mode always dirty; deduce mode honours is_outdated + force.
    std::vector<bool> dirty(tasks.size(), false);
    if (!cfg.deduce_dependency) {
        std::fill(dirty.begin(), dirty.end(), true);
    } else if (cfg.force) {
        std::fill(dirty.begin(), dirty.end(), true);
    } else {
        for (auto i : topo) {
            bool needs_run = false;
            if (tasks[i].outputs.empty()) {
                needs_run = true;
            } else {
                for (auto &out : tasks[i].outputs) {
                    if (!std::filesystem::exists(out)) {
                        needs_run = true;
                        break;
                    }
                    for (auto &in : tasks[i].inputs) {
                        if (is_outdated(out, in)) {
                            needs_run = true;
                            break;
                        }
                    }
                    if (needs_run) {
                        break;
                    }
                }
            }
            // A dirty producer forces consumers dirty.
            if (!needs_run) {
                for (std::size_t p = 0; p < tasks.size(); ++p) {
                    for (auto ch : children[p]) {
                        if (ch == i && dirty[p]) {
                            needs_run = true;
                            break;
                        }
                    }
                    if (needs_run) {
                        break;
                    }
                }
            }
            dirty[i] = needs_run;
            if (!needs_run) {
                result.tasks[i].state = bld::Task_state::skipped;
                result.tasks[i].message = "up to date";
                ++result.skipped;
                bld::log::d("task '{}': skipped (up to date)", tasks[i].name);
            }
        }
    }
    return run_schedule(tasks, children, indegree, dirty, cfg, result, width);
}

auto run_plan(Plan &plan, const Run_config &cfg) -> std::expected<Run_result, Err>
{
    // Plan always builds its own graph from needs/produces/after —
    // deduce_dependency is a span<Task>-only flag and passing it here is a bug.
    if (cfg.deduce_dependency) {
        return std::unexpected(Err::erc(
            std::errc::invalid_argument, "deduce_dependency applies to run(span<Task>) only; bld::Plan always uses its own dependency graph"));
    }
    auto &tasks = plan.tasks;
    if (!cfg.write_compile_commands.empty()) {
        auto written = write_database(plan, cfg.write_compile_commands);
        if (!written) {
            return std::unexpected(written.error());
        }
    }
    Run_result result{};
    result.tasks.resize(tasks.size());
    if (tasks.empty()) {
        return result;
    }
    // ensure names already done in Plan::add, but also for direct span
    ensure_task_names(std::span<bld::Task>{tasks});
    if (auto ok = check_empty_commands(std::span<bld::Task>{tasks}); !ok) {
        return std::unexpected(std::move(ok.error()));
    }
    std::size_t width = resolve_sched_width(cfg);
    std::unordered_map<std::string, std::size_t> producers, names;
    for (std::size_t i = 0; i < tasks.size(); ++i) {
        if (!tasks[i].name.empty() && !names.emplace(tasks[i].name, i).second) {
            return std::unexpected(Err::erc(std::errc::invalid_argument, std::format("duplicate task name '{}'", tasks[i].name)));
        }
        auto it_out = plan.task_outputs.find(tasks[i].name);
        if (it_out != plan.task_outputs.end()) {
            for (auto &out : it_out->second) {
                if (!producers.emplace(out, i).second) {
                    return std::unexpected(Err::erc(std::errc::invalid_argument, std::format("multiple tasks produce '{}'", out)));
                }
            }
        }
    }
    std::vector<std::vector<std::size_t>> children(tasks.size());
    std::vector<std::size_t> indegree(tasks.size(), 0);
    auto edge = [&](std::size_t from, std::size_t to) {
        for (auto c : children[from]) {
            if (c == to) {
                return;
            }
        }
        children[from].push_back(to);
        ++indegree[to];
    };
    for (std::size_t i = 0; i < tasks.size(); ++i) {
        auto it_in = plan.task_inputs.find(tasks[i].name);
        if (it_in != plan.task_inputs.end()) {
            for (auto &in : it_in->second) {
                if (auto it = producers.find(in); it != producers.end()) {
                    if (it->second == i) {
                        return std::unexpected(Err::erc(
                            std::errc::invalid_argument,
                            std::format("task '{}' depends on its own output '{}'", tasks[i].name, in)));
                    }
                    edge(it->second, i);
                }
            }
        }
        auto it_after = plan.task_after.find(tasks[i].name);
        if (it_after != plan.task_after.end()) {
            for (auto &dep : it_after->second) {
                auto it = names.find(dep);
                if (it == names.end()) {
                    return std::unexpected(
                        Err::erc(std::errc::invalid_argument, std::format("task '{}' depends on unknown task '{}'", tasks[i].name, dep)));
                }
                if (it->second == i) {
                    return std::unexpected(
                        Err::erc(std::errc::invalid_argument, std::format("task '{}' depends on itself", tasks[i].name)));
                }
                edge(it->second, i);
            }
        }
    }
    auto topo_res = topo_sort(children, indegree);
    if (!topo_res) {
        return std::unexpected(std::move(topo_res.error()));
    }
    auto topo = std::move(*topo_res);
    std::vector<bool> dirty(tasks.size(), cfg.force);
    for (auto i : topo) {
        if (!dirty[i]) {
            auto it_out = plan.task_outputs.find(tasks[i].name);
            if (it_out == plan.task_outputs.end() || it_out->second.empty()) {
                dirty[i] = true;
            } else {
                for (auto &out : it_out->second) {
                    auto target = path_for(tasks[i], out);
                    if (!std::filesystem::exists(target)) {
                        dirty[i] = true;
                        break;
                    }
                    auto it_in = plan.task_inputs.find(tasks[i].name);
                    if (it_in != plan.task_inputs.end()) {
                        for (auto &in : it_in->second) {
                            if (is_outdated(target, path_for(tasks[i], in))) {
                                dirty[i] = true;
                                break;
                            }
                        }
                    }
                    if (dirty[i]) {
                        break;
                    }
                }
            }
        }
        for (std::size_t p = 0; p < tasks.size() && !dirty[i]; ++p) {
            for (auto ch : children[p]) {
                if (ch == i && dirty[p]) {
                    dirty[i] = true;
                    break;
                }
            }
        }
        if (!dirty[i]) {
            result.tasks[i].state = Task_state::skipped;
            result.tasks[i].message = "up to date";
            ++result.skipped;
            bld::log::d("task '{}': skipped (up to date)", tasks[i].name);
        }
    }
    return run_schedule(std::span<bld::Task>{tasks}, children, indegree, dirty, cfg, result, width);
}
} // namespace bld::details

// IMPL SECTION 06 — Config (bld::Config)
auto bld::Config::normalize_key(std::string_view key) -> std::string
{
    std::size_t i = 0;
    while (i < key.size() && key[i] == '-') {
        ++i;
    }
    return std::string{key.substr(i)};
}

auto bld::Config::get() -> Config &
{
    static Config instance;
    return instance;
}

auto bld::Config::operator[](std::string_view key) const -> Proxy
{
    return Proxy{this, normalize_key(key)};
}

auto bld::Config::add_option(std::string_view flag, val_t type, std::string_view desc, value_type def, std::vector<std::string> valid_choices)
    -> Config &
{
    std::string nkey = normalize_key(flag);
    // Coerce a mismatched default (e.g. omitted def defaults to bool false)
    // into a sensible default for the declared type instead of storing a
    // value that later reads would reject as a type mismatch.
    bool matches = false;
    switch (type) {
    case Bool:
        matches = std::holds_alternative<bool>(def);
        break;
    case Int:
        matches = std::holds_alternative<int>(def);
        break;
    case Double:
        matches = std::holds_alternative<double>(def);
        break;
    case String:
        matches = std::holds_alternative<std::string>(def);
        break;
    case String_arr:
        matches = std::holds_alternative<std::vector<std::string>>(def);
        break;
    }
    if (!matches) {
        switch (type) {
        case Bool:
            def = false;
            break;
        case Int:
            def = 0;
            break;
        case Double:
            def = 0.0;
            break;
        case String:
            def = std::string{};
            break;
        case String_arr:
            def = std::vector<std::string>{};
            break;
        }
        bld::log::w("Config: default for option '{}' mismatched declared type; using type default.", nkey);
    }
    options[nkey] = Option{type, std::string(desc), std::move(def), std::move(valid_choices)};
    bld::log::d("Config: registered option '{}'.", nkey);
    return *this;
}

namespace bld::details {
// Shared val_t -> name mapping for Config::print_help. Extracted from two
// identical switches; behavior unchanged.
[[nodiscard]] inline auto config_type_name(bld::Config::val_t type) noexcept -> std::string_view
{
    switch (type) {
    case bld::Config::Bool:
        return "bool";
    case bld::Config::Int:
        return "int";
    case bld::Config::Double:
        return "double";
    case bld::Config::String:
        return "string";
    case bld::Config::String_arr:
        return "string[]";
    }
    return "unknown";
}

// Trims ASCII whitespace (used for numeric/bool CLI values).
[[nodiscard]] inline auto config_trim(std::string_view s) noexcept -> std::string_view
{
    std::size_t b = 0;
    while (b < s.size() && (s[b] == ' ' || s[b] == '\t' || s[b] == '\n' || s[b] == '\r' || s[b] == '\f' || s[b] == '\v')) {
        ++b;
    }
    std::size_t e = s.size();
    while (e > b && (s[e - 1] == ' ' || s[e - 1] == '\t' || s[e - 1] == '\n' || s[e - 1] == '\r' || s[e - 1] == '\f' || s[e - 1] == '\v')) {
        --e;
    }
    return s.substr(b, e - b);
}

// Case-insensitive bool parsing: true/1/yes/y <-> false/0/no/n.
[[nodiscard]] inline auto config_parse_bool(std::string_view s) -> std::optional<bool>
{
    auto t = config_trim(s);
    // Lowercase into a small buffer (bool words are short).
    char buf[8]{};
    if (t.size() >= sizeof(buf)) {
        return std::nullopt;
    }
    for (std::size_t i = 0; i < t.size(); ++i) {
        char c = t[i];
        buf[i] = static_cast<char>(c >= 'A' && c <= 'Z' ? c - 'A' + 'a' : c);
    }
    std::string_view l{buf, t.size()};
    if (l == "true" || l == "1" || l == "yes" || l == "y") {
        return true;
    }
    if (l == "false" || l == "0" || l == "no" || l == "n") {
        return false;
    }
    return std::nullopt;
}

// True when `s` looks like a number (allows leading -/+, digits, '.') so a
// space-separated value like "-5" isn't mistaken for a flag.
[[nodiscard]] inline auto config_looks_like_number(std::string_view s) noexcept -> bool
{
    auto t = config_trim(s);
    if (t.empty()) {
        return false;
    }
    std::size_t i = 0;
    if (t[0] == '-' || t[0] == '+') {
        i = 1;
    }
    bool any_digit = false;
    bool any_dot = false;
    for (; i < t.size(); ++i) {
        char c = t[i];
        if (c >= '0' && c <= '9') {
            any_digit = true;
        } else if (c == '.' && !any_dot) {
            any_dot = true;
        } else {
            return false;
        }
    }
    return any_digit;
}
} // namespace bld::details

auto bld::Config::print_help(std::string_view prog_name, std::string_view specific_opt) const -> void
{
    std::string help_text;
    std::string nspec = specific_opt.empty() ? std::string{} : normalize_key(specific_opt);

    if (!nspec.empty() && options.find(nspec) != options.end()) {
        const auto &opt = options.at(nspec);
        std::string_view type_str = bld::details::config_type_name(opt.type);
        std::format_to(std::back_inserter(help_text), "Option: {}\n", nspec);
        std::format_to(std::back_inserter(help_text), "  Type: {}\n", type_str);
        std::format_to(std::back_inserter(help_text), "  Desc: {}", opt.description);
        if (!opt.choices.empty()) {
            auto joined = opt.choices | std::views::join_with(std::string_view{"|"}) | std::ranges::to<std::string>();
            std::format_to(std::back_inserter(help_text), "\n  Choices: [{}]", joined);
        }
        std::println(std::cout, "{}", help_text);
        return;
    }

    if (!specific_opt.empty()) {
        bld::log::w("Option '{}' is not registered. Showing general help.", nspec.empty() ? specific_opt : nspec);
    }

    std::format_to(std::back_inserter(help_text), "Usage: {} [options]\nOptions:", prog_name);

    for (const auto &[flag, opt] : options) {
        std::string_view type_str = bld::details::config_type_name(opt.type);

        std::string def_str = "null";
        if (auto *p = std::get_if<int>(&opt.default_val); p) {
            def_str = std::format("{}", *p);
        } else if (auto *p = std::get_if<bool>(&opt.default_val); p) {
            def_str = *p ? "true" : "false";
        } else if (auto *p = std::get_if<double>(&opt.default_val); p) {
            def_str = std::format("{}", *p);
        } else if (auto *p = std::get_if<std::string>(&opt.default_val); p) {
            def_str = std::format("\"{}\"", *p);
        } else if (auto *p = std::get_if<std::vector<std::string>>(&opt.default_val); p) {
            if (p->empty()) {
                def_str = "[]";
            } else {
                def_str = std::format("[{}]", bld::str::join(*p, ", "));
            }
        }

        std::format_to(std::back_inserter(help_text), "\n  {:<15} [{:<8}] : {}", flag, type_str, opt.description);

        if (!opt.choices.empty()) {
            auto joined = opt.choices | std::views::join_with(std::string_view{"|"}) | std::ranges::to<std::string>();
            std::format_to(std::back_inserter(help_text), " [{}]", joined);
        }

        std::format_to(std::back_inserter(help_text), " (default: {})", def_str);
    }

    std::println(std::cout, "{}", help_text);
}

auto bld::Config::parse(int argc, char *argv[]) -> std::expected<Parse_outcome, bld::Err>
{
    if (argc < 0) {
        return std::unexpected(bld::Err::erc(std::errc::invalid_argument, "Config::parse received negative argc"));
    }
    std::size_t nargs = argc <= 0 ? 0 : static_cast<std::size_t>(argc);
    std::span<char *> args{argv, nargs};
    std::string_view prog_name = args.empty() || args[0] == nullptr ? "bld" : std::string_view{args[0]};

    auto arg_view = [&](std::size_t i) -> std::string_view {
        return args[i] == nullptr ? std::string_view{} : std::string_view{args[i]};
    };

    // Help detection first (never errors): -h/--help/help, with optional
    // `=opt` or trailing/leading option name for focused help.
    for (std::size_t i = 1; i < nargs; ++i) {
        std::string_view curr = arg_view(i);
        auto eq = curr.find('=');
        std::string_view head = eq == std::string_view::npos ? curr : curr.substr(0, eq);
        std::string nhead = normalize_key(head);
        if (nhead == "h" || nhead == "help") {
            std::string specific;
            if (eq != std::string_view::npos) {
                specific = normalize_key(curr.substr(eq + 1));
            } else if (i + 1 < nargs) {
                std::string_view nxt = arg_view(i + 1);
                if (!nxt.empty() && nxt[0] != '-') {
                    specific = normalize_key(nxt);
                }
            }
            if (specific.empty() && i > 1) {
                std::string_view prv = arg_view(i - 1);
                if (!prv.empty() && prv[0] != '-' && prv.find('=') == std::string_view::npos) {
                    std::string cand = normalize_key(prv);
                    if (options.find(cand) != options.end()) {
                        specific = std::move(cand);
                    }
                }
            }
            print_help(prog_name, specific);
            bld::log::d("Config: help requested (specific='{}').", specific);
            return Parse_outcome{.help_requested = true};
        }
    }

    data.clear();
    for (const auto &[flag, opt] : options) {
        data[flag] = opt.default_val;
    }
    // Reset accumulated String_arr defaults so repeated parses don't append
    // to leftovers (data was just rebuilt from defaults, but be explicit).
    for (const auto &[flag, opt] : options) {
        if (opt.type == String_arr) {
            data[flag] = std::vector<std::string>{};
            if (auto *p = std::get_if<std::vector<std::string>>(&opt.default_val); p && !p->empty()) {
                data[flag] = *p;
            }
        }
    }

    auto assign_typed = [&](const std::string &nkey, std::string_view val) -> std::expected<void, bld::Err> {
        auto oit = options.find(nkey);
        if (oit == options.end()) {
            return std::unexpected(
                bld::Err::erc(std::errc::invalid_argument, std::format("Unknown option '{}'.", nkey)));
        }
        val_t expected_type = oit->second.type;

        auto assign_num = [&]<typename T>(std::string_view noun) -> std::expected<void, bld::Err> {
            std::string_view t = bld::details::config_trim(val);
            T v{};
            auto [p, ec] = std::from_chars(t.data(), t.data() + t.size(), v);
            if (ec == std::errc{} && p == t.data() + t.size()) {
                data[nkey] = v;
                bld::log::d("Config: '{}' = {} ({}).", nkey, val, noun);
                return {};
            }
            return std::unexpected(
                bld::Err::erc(std::errc::invalid_argument, std::format("Option '{}' expects {}, got '{}'", nkey, noun, val)));
        };

        if (expected_type == Bool) {
            if (auto b = bld::details::config_parse_bool(val)) {
                data[nkey] = *b;
                bld::log::d("Config: '{}' = {}.", nkey, *b ? "true" : "false");
                return {};
            }
            return std::unexpected(
                bld::Err::erc(std::errc::invalid_argument, std::format("Option '{}' expects a bool, got '{}'", nkey, val)));
        } else if (expected_type == Int) {
            return assign_num.template operator()<int>("an integer");
        } else if (expected_type == Double) {
            return assign_num.template operator()<double>("a double");
        } else if (expected_type == String) {
            std::string sval(val);
            if (!oit->second.choices.empty()) {
                const auto &ch = oit->second.choices;
                if (std::find(ch.begin(), ch.end(), sval) == ch.end()) {
                    return std::unexpected(
                        bld::Err::erc(std::errc::invalid_argument, std::format("Invalid choice '{}' for option '{}'.", sval, nkey)));
                }
            }
            data[nkey] = sval;
            bld::log::d("Config: '{}' = '{}'.", nkey, sval);
            return {};
        } else if (expected_type == String_arr) {
            if (!std::holds_alternative<std::vector<std::string>>(data[nkey])) {
                data[nkey] = std::vector<std::string>{};
            }
            std::get<std::vector<std::string>>(data[nkey]).push_back(std::string(val));
            bld::log::d("Config: '{}' += '{}'.", nkey, val);
            return {};
        }
        return std::unexpected(bld::Err::erc(std::errc::invalid_argument, std::format("Option '{}' has unknown type", nkey)));
    };

    for (std::size_t i = 1; i < nargs; ++i) {
        std::string_view curr = arg_view(i);
        if (curr.empty()) {
            continue;
        }
        if (curr == "--") {
            continue;
        }
        auto eq_idx = curr.find('=');

        if (eq_idx != std::string_view::npos) {
            std::string nkey = normalize_key(curr.substr(0, eq_idx));
            std::string_view val = curr.substr(eq_idx + 1);
            if (nkey.empty()) {
                return std::unexpected(bld::Err::erc(std::errc::invalid_argument, std::format("Malformed option '{}'.", curr)));
            }
            if (options.find(nkey) == options.end()) {
                return std::unexpected(bld::Err::erc(std::errc::invalid_argument, std::format("Unknown option '{}'.", nkey)));
            }
            if (auto r = assign_typed(nkey, val); !r) {
                return std::unexpected(std::move(r.error()));
            }
            continue;
        }

        // No '=': either a bare bool flag or `key value` for value options.
        std::string nkey = normalize_key(curr);
        if (nkey.empty()) {
            return std::unexpected(bld::Err::erc(std::errc::invalid_argument, std::format("Malformed option '{}'.", curr)));
        }
        auto oit = options.find(nkey);
        if (oit == options.end()) {
            return std::unexpected(bld::Err::erc(std::errc::invalid_argument, std::format("Unknown option '{}'.", nkey)));
        }
        if (oit->second.type == Bool) {
            data[nkey] = true;
            bld::log::d("Config: '{}' = true.", nkey);
            continue;
        }
        // Value option with space-separated value: `jobs 8`, `--mode release`.
        if (i + 1 >= nargs) {
            return std::unexpected(
                bld::Err::erc(std::errc::invalid_argument, std::format("Option '{}' expects a value, got nothing.", nkey)));
        }
        std::string_view nxt = arg_view(i + 1);
        bool nxt_is_flag = false;
        if (!nxt.empty() && nxt[0] == '-') {
            // A leading '-' still counts as a value when it parses as a
            // number (e.g. `jobs -5`); otherwise it is the next flag.
            auto neq = nxt.find('=');
            std::string_view nhead = neq == std::string_view::npos ? nxt : nxt.substr(0, neq);
            std::string nn = normalize_key(nhead);
            if (nn == "h" || nn == "help") {
                nxt_is_flag = true;
            } else if (options.find(nn) != options.end()) {
                nxt_is_flag = true;
            } else if (!bld::details::config_looks_like_number(nxt)) {
                nxt_is_flag = true;
            }
        }
        if (nxt_is_flag) {
            return std::unexpected(
                bld::Err::erc(std::errc::invalid_argument, std::format("Option '{}' expects a value, got '{}'.", nkey, nxt)));
        }
        ++i;
        if (auto r = assign_typed(nkey, nxt); !r) {
            return std::unexpected(std::move(r.error()));
        }
    }

    return Parse_outcome{};
}

bld::Config::Proxy::operator bool() const
{
    if (auto res = cfg->get_val<bool>(key)) {
        return *res;
    } else {
        throw std::runtime_error(std::format("Config error: {}", res.error().msg));
    }
}

bld::Config::Proxy::operator std::string() const
{
    if (auto res = cfg->get_val<std::string>(key)) {
        return *res;
    } else {
        throw std::runtime_error(std::format("Config error: {}", res.error().msg));
    }
}

bld::Config::Proxy::operator int() const
{
    if (auto res = cfg->get_val<int>(key)) {
        return *res;
    } else {
        throw std::runtime_error(std::format("Config error: {}", res.error().msg));
    }
}

bld::Config::Proxy::operator double() const
{
    if (auto res = cfg->get_val<double>(key)) {
        return *res;
    } else {
        throw std::runtime_error(std::format("Config error: {}", res.error().msg));
    }
}

bld::Config::Proxy::operator std::vector<std::string>() const
{
    if (auto res = cfg->get_val<std::vector<std::string>>(key)) {
        return *res;
    } else {
        throw std::runtime_error(std::format("Config error: {}", res.error().msg));
    }
}

auto std::formatter<std::unordered_map<std::string, bld::Config::value_type>>::format(
    const std::unordered_map<std::string, bld::Config::value_type> &m, std::format_context &ctx) const -> std::format_context::iterator
{
    auto out = ctx.out();
    for (const auto &[k, v] : m) {
        std::format_to(out, "{}: ", k);
        if (auto *p = std::get_if<int>(&v); p) {
            std::format_to(out, "(i){}", *p);
        } else if (auto *p = std::get_if<bool>(&v); p) {
            std::format_to(out, "(b){}", *p);
        } else if (auto *p = std::get_if<double>(&v); p) {
            std::format_to(out, "(d){}", *p);
        } else if (auto *p = std::get_if<std::string>(&v); p) {
            std::format_to(out, "(s){}", *p);
        } else if (auto *p = std::get_if<std::vector<std::string>>(&v); p) {
            std::format_to(out, "(s[]){}", *p);
        } else {
            std::format_to(out, "unknown");
        }
    }
    return out;
}

// IMPL SECTION 07 — Test helpers (bld::test)
bld::test::Frontier::Frontier(std::ptrdiff_t max_d) : data(2 * max_d + 1, 0), offset(max_d)
{}

bld::test::compute_diff_op::operator bool() const
{
    return same;
}

auto bld::test::Frontier::operator[](std::ptrdiff_t k) -> std::ptrdiff_t &
{
    return data[k + offset];
}

auto bld::test::Frontier::operator[](std::ptrdiff_t k) const -> std::ptrdiff_t
{
    return data[k + offset];
}

auto bld::test::compute_diff(std::span<const std::string_view> original, std::span<const std::string_view> updated) -> compute_diff_op
{
    const std::ptrdiff_t n = original.size();
    const std::ptrdiff_t m = updated.size();
    const std::ptrdiff_t max_edits = n + m;

    if (max_edits == 0) {
        return compute_diff_op{.same = true, .edits = {}};
    }

    std::vector<Frontier> history;
    history.reserve(max_edits);

    Frontier current_frontier{max_edits};
    bool reached_end = false;
    std::ptrdiff_t end_x = 0;
    std::ptrdiff_t end_y = 0;

    for (std::ptrdiff_t d = 0; d <= max_edits; ++d) {
        history.push_back(current_frontier);

        for (std::ptrdiff_t k = -d; k <= d; k += 2) {
            std::ptrdiff_t x = 0;

            bool moving_down = (k == -d) || (k != d && current_frontier[k - 1] <= current_frontier[k + 1]);

            if (moving_down) {
                x = current_frontier[k + 1];
            } else {
                x = current_frontier[k - 1] + 1;
            }

            std::ptrdiff_t y = x - k;

            while (x < n && y < m && original[x] == updated[y]) {
                x++;
                y++;
            }

            current_frontier[k] = x;

            if (x >= n && y >= m) {
                reached_end = true;
                end_x = x;
                end_y = y;
                break;
            }
        }
        if (reached_end) {
            break;
        }
    }

    std::vector<Edit> script;
    std::ptrdiff_t x = end_x;
    std::ptrdiff_t y = end_y;

    for (std::ptrdiff_t d = history.size() - 1; d > 0; --d) {
        const Frontier &past = history[d];
        std::ptrdiff_t k = x - y;

        bool moving_down = (k == -d) || (k != d && past[k - 1] <= past[k + 1]);
        std::ptrdiff_t prev_k = moving_down ? k + 1 : k - 1;

        std::ptrdiff_t prev_x = past[prev_k];
        if (!moving_down) {
            prev_x++;
        }
        std::ptrdiff_t prev_y = prev_x - k;

        while (x > prev_x && y > prev_y) {
            script.push_back({Edit_type::keep, original[x - 1]});
            x--;
            y--;
        }

        if (moving_down) {
            script.push_back({Edit_type::insert, updated[y - 1]});
            y--;
        } else {
            script.push_back({Edit_type::remove, original[x - 1]});
            x--;
        }
    }

    while (x > 0 && y > 0) {
        script.push_back({Edit_type::keep, original[x - 1]});
        x--;
        y--;
    }

    std::ranges::reverse(script);

    bool is_same = (script.size() == static_cast<std::size_t>(n)) && (n == m);

    return compute_diff_op{.same = is_same, .edits = script};
}

auto bld::test::split_lines(std::string_view text) -> std::vector<std::string_view>
{
    std::vector<std::string_view> lines;
    std::size_t start = 0;
    while (start < text.size()) {
        auto end = text.find('\n', start);
        if (end == std::string_view::npos) {
            lines.push_back(text.substr(start));
            break;
        }
        lines.push_back(text.substr(start, end - start));
        start = end + 1;
    }
    return lines;
}

auto bld::test::compute_diff(std::initializer_list<std::string_view> original, std::initializer_list<std::string_view> updated) -> compute_diff_op
{
    return compute_diff(std::vector<std::string_view>{original}, std::vector<std::string_view>{updated});
}

auto bld::test::compute_diff(std::string_view original, std::string_view updated) -> compute_diff_op
{
    return compute_diff(split_lines(original), split_lines(updated));
}

auto std::formatter<bld::test::compute_diff_op>::format(const bld::test::compute_diff_op &diff, std::format_context &ctx) const
    -> std::format_context::iterator
{
    auto out = ctx.out();

    if (diff.same) {
        return std::format_to(out, "Files match.\n");
    }

    for (const auto &e : diff.edits) {
        switch (e.type) {
        case bld::test::Edit_type::keep:
            out = std::format_to(out, "  {}\n", e.content);
            break;
        case bld::test::Edit_type::insert:
            if (use_color) {
                out = std::format_to(out, "\033[32m+ {}\033[0m\n", e.content);
            } else {
                out = std::format_to(out, "+ {}\n", e.content);
            }
            break;
        case bld::test::Edit_type::remove:
            if (use_color) {
                out = std::format_to(out, "\033[31m- {}\033[0m\n", e.content);
            } else {
                out = std::format_to(out, "- {}\n", e.content);
            }
            break;
        }
    }
    return out;
}

// IMPL SECTION 08 — Filesystem (bld::fs)
auto bld::fs::make_dir_if_not_exists(std::string_view path, bool create_parents, std::source_location loc) noexcept -> bool
{
    namespace fs = std::filesystem;

    if (path.empty()) {
        bld::log::w("({}:{}) No directory was created because of empty path.", loc.file_name(), loc.line());
        return false;
    }

    std::error_code ec;
    const bool created = create_parents ? fs::create_directories(fs::path{path}, ec) : fs::create_directory(fs::path{path}, ec);

    if (ec) {
        bld::log::w("fs: make_dir_if_not_exists('{}') failed: {}", path, ec.message());
        return false;
    }

    if (created) {
        bld::log::i("fs: created dir '{}'.", path);
    } else {
        bld::log::d("fs: dir '{}' already exists.", path);
    }

    return created;
}

namespace bld::fs {
namespace detail {
// Wraps a std::filesystem call taking a trailing std::error_code&, turning ec
// failures into bld::Err with a lazily formatted message (formatted only on
// error, as before). Extracted from the repetitive is_empty/file_size/...
// wrappers; messages preserved verbatim.
template <typename Op, typename... Args>
inline auto wrap_value(Op &&op, std::format_string<Args...> fmt, Args &&...args) noexcept
    -> std::expected<std::invoke_result_t<Op, std::error_code &>, bld::Err>
{
    std::error_code ec;
    auto value = op(ec);
    if (ec) {
        return std::unexpected(bld::Err{.err = ec, .msg = std::format(fmt, std::forward<Args>(args)...)});
    }
    return value;
}
template <typename Op, typename... Args>
inline auto wrap_void(Op &&op, std::format_string<Args...> fmt, Args &&...args) noexcept -> std::expected<void, bld::Err>
{
    std::error_code ec;
    op(ec);
    if (ec) {
        return std::unexpected(bld::Err{.err = ec, .msg = std::format(fmt, std::forward<Args>(args)...)});
    }
    return {};
}

// Shared ofstream body for write_file/append_file (differ only in open mode
// and message verb). Messages preserved verbatim via `verb`.
inline auto write_content(std::string_view path, std::string_view content, std::ios::openmode mode, std::string_view verb) noexcept
    -> std::expected<void, bld::Err>
{
    try {
        std::ofstream file(std::filesystem::path(path), std::ios::out | std::ios::binary | mode);
        if (!file) {
            bld::log::e("fs: failed to open file for {}: '{}'.", verb, path);
            return std::unexpected(bld::Err::erc(std::errc::io_error, std::format("Failed to open file for {}: '{}'", verb, path)));
        }
        file.write(content.data(), static_cast<std::streamsize>(content.size()));
        if (!file) {
            bld::log::e("fs: failed to write {} bytes while {} '{}'.", content.size(), verb, path);
            return std::unexpected(bld::Err::erc(std::errc::io_error, std::format("Failed to write file '{}'", path)));
        }
        bld::log::i("fs: wrote {} byte(s) to '{}' ({}).", content.size(), path, verb);
        return {};
    } catch (const std::exception &e) {
        bld::log::e("fs: failed {} '{}': {}.", verb, path, e.what());
        return std::unexpected(bld::Err::erc(std::errc::io_error, e.what()));
    }
}
} // namespace detail

bool Dir_entry::is_file() const noexcept
{
    return type == std::filesystem::file_type::regular;
}

bool Dir_entry::is_dir() const noexcept
{
    return type == std::filesystem::file_type::directory;
}

bool Dir_entry::is_symlink() const noexcept
{
    return type == std::filesystem::file_type::symlink;
}

bool Dir_entry::is_hidden() const noexcept
{
    const auto n = filename();
    if (!n.empty() && n.front() == '.') {
        return true;
    }
#ifdef _WIN32
    const DWORD attrs = ::GetFileAttributesW(path.c_str());
    if (attrs != INVALID_FILE_ATTRIBUTES) {
        return (attrs & FILE_ATTRIBUTE_HIDDEN) != 0;
    }
#endif
    return false;
}

std::string Dir_entry::extension() const noexcept
{
    return path.extension().string();
}

std::string Dir_entry::stem() const noexcept
{
    return path.stem().string();
}

std::string Dir_entry::filename() const noexcept
{
    return path.filename().string();
}

std::string Dir_entry::parent() const noexcept
{
    return path.parent_path().string();
}

bool Walk_error::is_fs_error() const noexcept
{
    return kind == Kind::fs;
}

bool Walk_error::is_visitor_error() const noexcept
{
    return kind == Kind::visitor;
}

std::string Walk_error::message() const
{
    return code.message();
}

Dir_walker::Dir_walker(std::string_view root) : root_{root}
{}

Dir_walker::Dir_walker(std::filesystem::path root) : root_{std::move(root)}
{}

Dir_walker::Dir_walker(const std::string &root) : root_{root}
{}

Dir_walker::Dir_walker(const char *root) : root_{root}
{}

auto Dir_walker::recursive(bool v) noexcept -> Dir_walker &
{
    recursive_ = v;
    return *this;
}

auto Dir_walker::flat() noexcept -> Dir_walker &
{
    recursive_ = false;
    return *this;
}

auto Dir_walker::exclude_dirs(bool v) noexcept -> Dir_walker &
{
    include_dirs_ = !v;
    return *this;
}

auto Dir_walker::files_only(bool v) noexcept -> Dir_walker &
{
    include_dirs_ = !v;
    return *this;
}

auto Dir_walker::include_hidden(bool v) noexcept -> Dir_walker &
{
    include_hidden_ = v;
    return *this;
}

auto Dir_walker::follow_symlinks(bool v) noexcept -> Dir_walker &
{
    follow_symlinks_ = v;
    return *this;
}

auto Dir_walker::max_depth(int v) noexcept -> Dir_walker &
{
    max_depth_ = v;
    return *this;
}

auto Dir_walker::ext(std::string e) -> Dir_walker &
{
    if (!e.empty() && e.front() != '.') {
        e = '.' + e;
    }
    return where([e = std::move(e)](const Dir_entry &entry) { return entry.extension() == e; });
}

auto Dir_walker::ext(std::initializer_list<std::string_view> exts) -> Dir_walker &
{
    return where([exts = std::vector<std::string>{exts.begin(), exts.end()}](const Dir_entry &entry) {
        return std::ranges::any_of(exts, [&](const auto &e) { return entry.extension() == e; });
    });
}

auto Dir_walker::named(std::string name) -> Dir_walker &
{
    return where([n = std::move(name)](const Dir_entry &entry) { return entry.filename() == n; });
}

auto Dir_walker::skip(std::string dir_name) -> Dir_walker &
{
    skips_.emplace_back(std::move(dir_name));
    return *this;
}

auto Dir_walker::skip(std::initializer_list<std::string_view> dir_names) -> Dir_walker &
{
    for (auto n : dir_names) {
        skips_.emplace_back(n);
    }
    return *this;
}

auto Dir_walker::collect(std::source_location caller) const noexcept -> Walk_result_t<std::vector<Dir_entry>>
{
    std::vector<Dir_entry> out;
    return run(
               [&](const Dir_entry &e) -> Walk_result {
                   out.push_back(e);
                   return Walk_action::next;
               },
               caller)
        .transform([&] { return std::move(out); });
}

auto Dir_walker::collect_paths(std::source_location caller) const noexcept -> Walk_result_t<std::vector<std::filesystem::path>>
{
    return collect(caller).transform(
        [](auto &&entries) { return entries | std::views::transform(&Dir_entry::path) | std::ranges::to<std::vector>(); });
}

auto Dir_walker::count(std::source_location caller) const noexcept -> Walk_result_t<std::size_t>
{
    std::size_t n = 0;
    return run(
               [&](const Dir_entry &) -> Walk_result {
                   ++n;
                   return Walk_action::next;
               },
               caller)
        .transform([&] { return n; });
}

auto Dir_walker::any(std::source_location caller) const noexcept -> Walk_result_t<bool>
{
    bool found = false;
    return run(
               [&](const Dir_entry &) -> Walk_result {
                   found = true;
                   return Walk_action::stop;
               },
               caller)
        .transform([&] { return found; });
}

auto Dir_walker::none(std::source_location caller) const noexcept -> Walk_result_t<bool>
{
    return any(caller).transform([](bool v) { return !v; });
}

auto Dir_walker::first(std::source_location caller) const noexcept -> Walk_result_t<std::optional<Dir_entry>>
{
    std::optional<Dir_entry> found;
    return run(
               [&](const Dir_entry &e) -> Walk_result {
                   found = e;
                   return Walk_action::stop;
               },
               caller)
        .transform([&] { return std::move(found); });
}

auto Dir_walker::last(std::source_location caller) const noexcept -> Walk_result_t<std::optional<Dir_entry>>
{
    std::optional<Dir_entry> found;
    return run(
               [&](const Dir_entry &e) -> Walk_result {
                   found = e;
                   return Walk_action::next;
               },
               caller)
        .transform([&] { return std::move(found); });
}

auto Dir_walker::subdir(std::string_view sub) const -> Dir_walker
{
    Dir_walker w{root_ / sub};
    w.recursive_ = recursive_;
    w.include_dirs_ = include_dirs_;
    w.include_hidden_ = include_hidden_;
    w.follow_symlinks_ = follow_symlinks_;
    w.max_depth_ = max_depth_;
    w.filters_ = filters_;
    w.skips_ = skips_;
    return w;
}

bool Dir_walker::passes_filters(const Dir_entry &e) const
{
    return std::ranges::all_of(filters_, [&](const auto &f) { return f(e); });
}

bool Dir_walker::is_skipped(const Dir_entry &e) const
{
    return e.is_dir() && std::ranges::any_of(skips_, [&](const auto &s) { return e.filename() == s; });
}

bool Dir_walker::is_visible(const Dir_entry &e) const
{
    if (!include_hidden_ && e.is_hidden()) {
        return false;
    }
    if (!include_dirs_ && e.is_dir()) {
        return false;
    }
    return true;
}

auto Dir_walker::make_entry(const std::filesystem::directory_entry &raw, int d) -> Dir_entry
{
    std::error_code ignored;
    return Dir_entry{
        .path = raw.path(),
        .type = raw.symlink_status(ignored).type(),
        .depth = d,
    };
}

auto collect(std::string_view root, std::source_location caller) noexcept -> Walk_result_t<std::vector<Dir_entry>>
{
    return Dir_walker{root}.collect(caller);
}

auto stem(std::string_view path) noexcept -> std::string
{
    return std::filesystem::path{path}.stem().string();
}

auto name(std::string_view path) noexcept -> std::string
{
    return std::filesystem::path{path}.filename().string();
}

auto extension(std::string_view path) noexcept -> std::string
{
    return std::filesystem::path{path}.extension().string();
}

auto parent_dir(std::string_view path) noexcept -> std::string
{
    return std::filesystem::path{path}.parent_path().string();
}

auto is_absolute(std::string_view path) noexcept -> bool
{
    return std::filesystem::path{path}.is_absolute();
}

auto is_relative(std::string_view path) noexcept -> bool
{
    return std::filesystem::path{path}.is_relative();
}

auto exists(std::string_view path) noexcept -> bool
{
    std::error_code ec;
    return std::filesystem::exists(std::filesystem::path{path}, ec);
}

auto is_dir(std::string_view path) noexcept -> bool
{
    std::error_code ec;
    return std::filesystem::is_directory(std::filesystem::path{path}, ec);
}

auto is_file(std::string_view path) noexcept -> bool
{
    std::error_code ec;
    return std::filesystem::is_regular_file(std::filesystem::path{path}, ec);
}

auto is_symlink(std::string_view path) noexcept -> bool
{
    std::error_code ec;
    return std::filesystem::is_symlink(std::filesystem::path{path}, ec);
}

auto same_file(std::string_view a, std::string_view b) noexcept -> bool
{
    std::error_code ec;
    return std::filesystem::equivalent(std::filesystem::path{a}, std::filesystem::path{b}, ec);
}

auto is_empty(std::string_view path) noexcept -> std::expected<bool, bld::Err>
{
    return detail::wrap_value(
        [&](std::error_code &ec) { return std::filesystem::is_empty(std::filesystem::path{path}, ec); },
        "Failed to check if '{}' is empty",
        path);
}

auto file_size(std::string_view path) noexcept -> std::expected<std::uintmax_t, bld::Err>
{
    return detail::wrap_value(
        [&](std::error_code &ec) { return std::filesystem::file_size(std::filesystem::path{path}, ec); },
        "Failed to get size of '{}'",
        path);
}

auto last_write_time(std::string_view path) noexcept -> std::expected<std::filesystem::file_time_type, bld::Err>
{
    return detail::wrap_value(
        [&](std::error_code &ec) { return std::filesystem::last_write_time(std::filesystem::path{path}, ec); },
        "Failed to get last write time of '{}'",
        path);
}

auto copy_file(std::string_view from, std::string_view to, bool overwrite) noexcept -> std::expected<void, bld::Err>
{
    auto options = overwrite ? std::filesystem::copy_options::overwrite_existing : std::filesystem::copy_options::none;
    auto r = detail::wrap_void(
        [&](std::error_code &ec) { std::filesystem::copy_file(std::filesystem::path{from}, std::filesystem::path{to}, options, ec); },
        "Failed to copy file from '{}' to '{}'",
        from,
        to);
    if (!r) {
        bld::log::e("fs: copy_file('{}' -> '{}') failed: {}.", from, to, r.error().msg);
    } else {
        bld::log::i("fs: copied '{}' -> '{}'{}.", from, to, overwrite ? " (overwrite)" : "");
    }
    return r;
}

auto rename(std::string_view from, std::string_view to) noexcept -> std::expected<void, bld::Err>
{
    auto r = detail::wrap_void(
        [&](std::error_code &ec) { std::filesystem::rename(std::filesystem::path{from}, std::filesystem::path{to}, ec); },
        "Failed to rename '{}' to '{}'",
        from,
        to);
    if (!r) {
        bld::log::e("fs: rename('{}' -> '{}') failed: {}.", from, to, r.error().msg);
    } else {
        bld::log::i("fs: renamed '{}' -> '{}'.", from, to);
    }
    return r;
}

auto create_symlink(std::string_view target, std::string_view link) noexcept -> std::expected<void, bld::Err>
{
    auto r = detail::wrap_void(
        [&](std::error_code &ec) { std::filesystem::create_symlink(std::filesystem::path{target}, std::filesystem::path{link}, ec); },
        "Failed to create symlink at '{}'",
        link);
    if (!r) {
        bld::log::e("fs: create_symlink('{}' -> '{}') failed: {}.", target, link, r.error().msg);
    } else {
        bld::log::i("fs: created symlink '{}' -> '{}'.", link, target);
    }
    return r;
}

auto create_hard_link(std::string_view target, std::string_view link) noexcept -> std::expected<void, bld::Err>
{
    auto r = detail::wrap_void(
        [&](std::error_code &ec) {
            std::filesystem::create_hard_link(std::filesystem::path{target}, std::filesystem::path{link}, ec);
        },
        "Failed to create hard link at '{}'",
        link);
    if (!r) {
        bld::log::e("fs: create_hard_link('{}' -> '{}') failed: {}.", target, link, r.error().msg);
    } else {
        bld::log::i("fs: created hard link '{}' -> '{}'.", link, target);
    }
    return r;
}

auto read_symlink(std::string_view path) noexcept -> std::expected<std::string, bld::Err>
{
    auto r = detail::wrap_value(
        [&](std::error_code &ec) { return std::filesystem::read_symlink(std::filesystem::path{path}, ec); },
        "Failed to read symlink '{}'",
        path);
    if (!r) {
        return std::unexpected(std::move(r.error()));
    }
    return r->string();
}

auto current_path() noexcept -> std::expected<std::string, bld::Err>
{
    auto r = detail::wrap_value(
        [](std::error_code &ec) { return std::filesystem::current_path(ec); }, "Failed to get current working directory");
    if (!r) {
        return std::unexpected(std::move(r.error()));
    }
    return r->string();
}

auto set_current_path(std::string_view path) noexcept -> std::expected<void, bld::Err>
{
    auto r = detail::wrap_void(
        [&](std::error_code &ec) { std::filesystem::current_path(std::filesystem::path{path}, ec); },
        "Failed to set current path to '{}'",
        path);
    if (!r) {
        bld::log::e("fs: set_current_path('{}') failed: {}.", path, r.error().msg);
    } else {
        bld::log::i("fs: set current path to '{}'.", path);
    }
    return r;
}

auto absolute(std::string_view path) noexcept -> std::expected<std::string, bld::Err>
{
    auto r = detail::wrap_value(
        [&](std::error_code &ec) { return std::filesystem::absolute(std::filesystem::path{path}, ec); },
        "Failed to get absolute path for '{}'",
        path);
    if (!r) {
        return std::unexpected(std::move(r.error()));
    }
    return r->string();
}

auto canonical(std::string_view path) noexcept -> std::expected<std::string, bld::Err>
{
    auto r = detail::wrap_value(
        [&](std::error_code &ec) { return std::filesystem::canonical(std::filesystem::path{path}, ec); },
        "Failed to resolve canonical path for '{}'",
        path);
    if (!r) {
        return std::unexpected(std::move(r.error()));
    }
    return r->string();
}

auto relative(std::string_view path, std::string_view base) noexcept -> std::expected<std::string, bld::Err>
{
    auto r = detail::wrap_value(
        [&](std::error_code &ec) {
            return std::filesystem::relative(std::filesystem::path{path}, std::filesystem::path{base}, ec);
        },
        "Failed to resolve relative path for '{}'",
        path);
    if (!r) {
        return std::unexpected(std::move(r.error()));
    }
    return r->string();
}

auto read_file(std::string_view path) noexcept -> std::expected<std::string, bld::Err>
{
    try {
        std::error_code ec;
        if (!std::filesystem::is_regular_file(path, ec)) {
            if (ec) {
                return std::unexpected(bld::Err{.err = ec, .msg = std::format("Failed to read '{}': {}", path, ec.message())});
            }
            return std::unexpected(bld::Err::erc(std::errc::io_error, std::format("'{}' is not a regular file", path)));
        }
        std::ifstream file(std::filesystem::path(path), std::ios::in | std::ios::binary | std::ios::ate);
        if (!file) {
            return std::unexpected(bld::Err::erc(std::errc::io_error, std::format("Failed to open file for reading: '{}'", path)));
        }

        auto size = file.tellg();
        file.seekg(0, std::ios::beg);

        std::string buffer;
        if (size > 0) {
            buffer.resize(static_cast<std::size_t>(size));
            file.read(buffer.data(), size);
        }
        return buffer;
    } catch (const std::exception &e) {
        return std::unexpected(bld::Err::erc(std::errc::io_error, e.what()));
    }
}

auto write_file(std::string_view path, std::string_view content) noexcept -> std::expected<void, bld::Err>
{
    return detail::write_content(path, content, std::ios::trunc, "writing");
}

auto append_file(std::string_view path, std::string_view content) noexcept -> std::expected<void, bld::Err>
{
    return detail::write_content(path, content, std::ios::app, "appending");
}

template <typename... Paths>
    requires(std::convertible_to<Paths, std::string_view> && ...)
[[nodiscard]] auto join(Paths &&...paths) -> std::string
{
    std::filesystem::path result;
    (..., (result /= std::filesystem::path{std::forward<Paths>(paths)}));
    return result.string();
}

template <typename... Paths>
    requires(std::convertible_to<Paths, std::string_view> && ...)
auto remove(Paths &&...paths) noexcept -> std::expected<void, bld::Err>
{
    std::error_code ec;
    bool ok = true;
    std::string failed;
    auto try_remove = [&](std::string_view p) {
        std::filesystem::remove_all(std::filesystem::path{p}, ec);
        if (ec) {
            ok = false;
            failed = std::string{p};
            bld::log::e("fs: remove('{}') failed: {}.", p, ec.message());
        } else {
            bld::log::i("fs: removed '{}'.", p);
        }
    };
    (try_remove(std::string_view{std::forward<Paths>(paths)}), ...);
    if (!ok) {
        return std::unexpected(bld::Err{.err = ec, .msg = std::format("Failed to remove '{}'", failed)});
    }
    return {};
}

template <typename... Paths>
    requires(std::convertible_to<Paths, std::string_view> && ...)
auto make_dirs(Paths &&...paths) noexcept -> std::expected<void, bld::Err>
{
    std::error_code ec;
    bool ok = true;
    std::string failed;
    auto try_mkdir = [&](std::string_view p) {
        std::filesystem::create_directories(std::filesystem::path{p}, ec);
        if (ec) {
            ok = false;
            failed = std::string{p};
            bld::log::e("fs: make_dirs('{}') failed: {}.", p, ec.message());
        } else {
            bld::log::i("fs: make_dirs ensured dir '{}'.", p);
        }
    };
    (try_mkdir(std::string_view{std::forward<Paths>(paths)}), ...);
    if (!ok) {
        return std::unexpected(bld::Err{.err = ec, .msg = std::format("Failed to create directory '{}'", failed)});
    }
    return {};
}

inline auto find_all_files(std::string_view root) noexcept -> Walk_result_t<std::vector<std::string>>
{
    return Dir_walker{root}.collect_paths().transform([](const auto &paths) {
        std::vector<std::string> res;
        res.reserve(paths.size());
        for (const auto &p : paths) {
            res.push_back(p.string());
        }
        return res;
    });
}

template <typename... Exts>
    requires(std::convertible_to<Exts, std::string_view> && ...)
auto find_by_ext(std::string_view root, Exts &&...exts) noexcept -> Walk_result_t<std::vector<std::string>>
{
    return Dir_walker{root}.ext({std::string_view(exts)...}).collect_paths().transform([](const auto &paths) {
        std::vector<std::string> res;
        res.reserve(paths.size());
        for (const auto &p : paths) {
            res.push_back(p.string());
        }
        return res;
    });
}

template <typename... Names>
    requires(std::convertible_to<Names, std::string_view> && ...)
auto find_by_name(std::string_view root, Names &&...names) noexcept -> Walk_result_t<std::vector<std::string>>
{
    std::vector<std::string_view> targets{std::forward<Names>(names)...};
    return Dir_walker{root}
        .where([targets](const Dir_entry &e) { return std::ranges::find(targets, e.filename()) != targets.end(); })
        .collect_paths()
        .transform([](const auto &paths) {
            std::vector<std::string> res;
            res.reserve(paths.size());
            for (const auto &p : paths) {
                res.push_back(p.string());
            }
            return res;
        });
}

std::vector<Cpp_module> scan_modules(const std::string &path, std::vector<std::string> extensions)
{
    std::vector<bld::fs::Cpp_module> modules;

    if (extensions.empty()) {
        bld::log::e("Extesions cannot be empty: {}", std::source_location::current().function_name());
        return {};
    }

    for (auto &e : extensions) {
        if (!e.empty() && e.front() != '.') {
            e = '.' + e;
        }
    }

    auto walk_res = bld::fs::Dir_walker{path}
                        .recursive()
                        .where([&extensions](const bld::fs::Dir_entry &entry) {
                            return std::ranges::any_of(extensions, [&](const std::string &ext) { return entry.extension() == ext; });
                        })
                        .for_each([&](const bld::fs::Dir_entry &entry) {
                            std::string content;
                            if (auto res = bld::fs::read_file(entry.path.string()); res) {
                                content = *res;
                            } else {
                                bld::log::w("Failed to read module file '{}': {}", entry.path.string(), res.error().msg);
                                return;
                            }
                            if (content.empty()) {
                                return;
                            }

                            // Tokenize
                            std::vector<std::string> tokens;
                            std::string token;
                            for (char c : content) {
                                if (std::isspace(static_cast<unsigned char>(c)) || c == ';') {
                                    if (!token.empty()) {
                                        tokens.push_back(token);
                                        token.clear();
                                    }
                                    if (c == ';') {
                                        tokens.push_back(";");
                                    }
                                } else {
                                    token += c;
                                }
                            }

                            bld::fs::Cpp_module mod;
                            mod.file = entry.path;
                            bool found_name = false;
                            std::string primary_name;
                            std::unordered_set<std::string> seen_imports;

                            for (size_t i = 0; i < tokens.size(); ++i) {
                                if (tokens[i] == "export" && i + 2 < tokens.size() && tokens[i + 1] == "module") {
                                    mod.name = tokens[i + 2];
                                    found_name = true;
                                    size_t colon = mod.name.find(':');
                                    primary_name = (colon != std::string::npos) ? mod.name.substr(0, colon) : mod.name;
                                } else if (tokens[i] == "import") {
                                    if (i > 0 && tokens[i - 1] == "export") {
                                        continue;
                                    }
                                    if (i + 1 < tokens.size()) {
                                        std::string dep = tokens[i + 1];
                                        if (!dep.starts_with("std") && dep != ";" && !dep.empty()) {
                                            if (dep.starts_with(":") && !primary_name.empty()) {
                                                dep = primary_name + dep;
                                            }
                                            if (seen_imports.insert(dep).second) {
                                                mod.imports.push_back(dep);
                                            }
                                        }
                                    }
                                } else if (tokens[i] == "export" && i + 2 < tokens.size() && tokens[i + 1] == "import") {
                                    std::string dep = tokens[i + 2];
                                    if (!dep.starts_with("std") && dep != ";" && !dep.empty()) {
                                        if (dep.starts_with(":") && !primary_name.empty()) {
                                            dep = primary_name + dep;
                                        }
                                        if (seen_imports.insert(dep).second) {
                                            mod.imports.push_back(dep);
                                        }
                                    }
                                }
                            }

                            if (found_name) {
                                modules.push_back(std::move(mod));
                            } else {
                                bld::log::w("Skipped file (no module decl found): {}", entry.path.string());
                            }
                        });

    if (!walk_res) {
        bld::log::e("Failed to scan modules in '{}': {}", path, walk_res.error().message());
    }

    return modules;
}

} // namespace bld::fs

// IMPL SECTION 09 — String utilities (bld::str)
namespace bld::str {

auto trim_left(std::string_view s) noexcept -> std::string_view
{
    auto it = std::ranges::find_if_not(s, [](unsigned char c) { return std::isspace(c); });
    return s.substr(static_cast<std::size_t>(std::distance(s.begin(), it)));
}

auto trim_right(std::string_view s) noexcept -> std::string_view
{
    auto it = std::ranges::find_if_not(s | std::views::reverse, [](unsigned char c) { return std::isspace(c); });
    return s.substr(0, s.size() - static_cast<std::size_t>(std::distance(s.rbegin(), it)));
}

auto trim(std::string_view s) noexcept -> std::string_view
{
    return trim_right(trim_left(s));
}

auto split(std::string_view s, char delimiter) -> std::vector<std::string_view>
{
    std::vector<std::string_view> result;
    std::size_t start = 0;
    std::size_t end = s.find(delimiter);

    while (end != std::string_view::npos) {
        result.push_back(s.substr(start, end - start));
        start = end + 1;
        end = s.find(delimiter, start);
    }
    result.push_back(s.substr(start));
    return result;
}

auto split(std::string_view s, std::string_view delimiter) -> std::vector<std::string_view>
{
    std::vector<std::string_view> result;
    if (delimiter.empty()) {
        result.push_back(s);
        return result;
    }

    std::size_t start = 0;
    std::size_t end = s.find(delimiter);

    while (end != std::string_view::npos) {
        result.push_back(s.substr(start, end - start));
        start = end + delimiter.size();
        end = s.find(delimiter, start);
    }
    result.push_back(s.substr(start));
    return result;
}

auto to_lower(std::string_view s) -> std::string
{
    std::string result(s);
    std::ranges::transform(result, result.begin(), [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    return result;
}

auto to_upper(std::string_view s) -> std::string
{
    std::string result(s);
    std::ranges::transform(result, result.begin(), [](unsigned char c) { return static_cast<char>(std::toupper(c)); });
    return result;
}

auto replace_all(std::string_view s, std::string_view from, std::string_view to) -> std::string
{
    if (from.empty()) {
        return std::string(s);
    }

    std::string result;
    std::size_t pos = 0;
    std::size_t last = 0;

    while ((pos = s.find(from, last)) != std::string_view::npos) {
        result.append(s.data() + last, pos - last);
        result.append(to);
        last = pos + from.size();
    }
    result.append(s.data() + last, s.size() - last);
    return result;
}

auto parse_int(std::string_view s, int base) noexcept -> std::expected<int, bld::Err>
{
    int value = 0;
    auto trimmed = trim(s);
    auto [ptr, ec] = std::from_chars(trimmed.data(), trimmed.data() + trimmed.size(), value, base);

    if (ec == std::errc() && ptr == trimmed.data() + trimmed.size()) {
        return value;
    }
    return std::unexpected(bld::Err::erc(std::errc::invalid_argument, std::format("Failed to parse int from '{}'", s)));
}

auto parse_double(std::string_view s) noexcept -> std::expected<double, bld::Err>
{
    double value = 0.0;
    auto trimmed = trim(s);
    auto [ptr, ec] = std::from_chars(trimmed.data(), trimmed.data() + trimmed.size(), value);

    if (ec == std::errc() && ptr == trimmed.data() + trimmed.size()) {
        return value;
    }
    return std::unexpected(bld::Err::erc(std::errc::invalid_argument, std::format("Failed to parse double from '{}'", s)));
}

auto parse_bool(std::string_view s) noexcept -> std::expected<bool, bld::Err>
{
    auto t = to_lower(trim(s));
    if (t == "true" || t == "1" || t == "yes" || t == "y") {
        return true;
    }
    if (t == "false" || t == "0" || t == "no" || t == "n") {
        return false;
    }
    return std::unexpected(bld::Err::erc(std::errc::invalid_argument, std::format("Failed to parse bool from '{}'", s)));
}

template <std::ranges::range Range>
    requires std::convertible_to<std::ranges::range_value_t<Range>, std::string_view>
[[nodiscard]] auto join(const Range &range, std::string_view delimiter) -> std::string
{
    std::string result;
    bool first = true;
    for (const auto &item : range) {
        if (!first) {
            result.append(delimiter);
        }
        first = false;
        result.append(std::string_view{item});
    }
    return result;
}

} // namespace bld::str

// IMPL SECTION 10 — Time (bld::time)
namespace bld::time {

auto stamp::reset() noexcept -> std::chrono::nanoseconds
{
    auto now = clock_t::now();
    auto diff = now - tp_;
    tp_ = now;
    return std::chrono::duration_cast<std::chrono::nanoseconds>(diff);
}

auto stamp::elapsed() const noexcept -> std::chrono::nanoseconds
{
    return std::chrono::duration_cast<std::chrono::nanoseconds>(clock_t::now() - tp_);
}

auto stamp::since(const stamp &baseline) const noexcept -> std::chrono::nanoseconds
{
    return std::chrono::duration_cast<std::chrono::nanoseconds>(tp_ - baseline.tp_);
}

auto now() noexcept -> stamp
{
    return stamp{};
}

auto since(const stamp &baseline) noexcept -> std::chrono::nanoseconds
{
    return baseline.elapsed();
}

auto format(std::chrono::nanoseconds ns) -> std::string
{
    if (ns.count() >= 1'000'000'000) {
        double s = static_cast<double>(ns.count()) / 1'000'000'000.0;
        return std::format("{:.3f}s", s);
    }
    if (ns.count() >= 1'000'000) {
        double ms = static_cast<double>(ns.count()) / 1'000'000.0;
        return std::format("{:.2f}ms", ms);
    }
    if (ns.count() >= 1'000) {
        double us = static_cast<double>(ns.count()) / 1'000.0;
        return std::format("{:.1f}us", us);
    }
    return std::format("{}ns", ns.count());
}

} // namespace bld::time

namespace bld::fs {

constexpr Walk_result::Walk_result(Walk_action a) noexcept : action{a}
{}
constexpr Walk_result::Walk_result(std::error_code ec) noexcept : action{ec ? Walk_action::stop : Walk_action::next}, error{ec}
{}

namespace detail {
template <Valid_visitor V>
auto invoke_visitor(V &&v, const Dir_entry &e) -> Walk_result
{
    if constexpr (Void_visitor<V>) {
        std::invoke(std::forward<V>(v), e);
        return {};
    } else {
        return std::invoke(std::forward<V>(v), e);
    }
}
} // namespace detail

template <typename Pred>
    requires std::predicate<Pred, const Dir_entry &>
auto Dir_walker::where(Pred pred) -> Dir_walker &
{
    filters_.emplace_back(std::move(pred));
    return *this;
}

template <detail::Valid_visitor V>
auto Dir_walker::walk(V &&visitor, std::source_location caller) const noexcept -> Walk_result_t<void>
{
    return run([&](const Dir_entry &e) { return detail::invoke_visitor(std::forward<V>(visitor), e); }, caller);
}

template <std::invocable<const Dir_entry &> F>
auto Dir_walker::for_each(F &&fn, std::source_location caller) const noexcept -> Walk_result_t<void>
{
    return run(
        [&](const Dir_entry &e) -> Walk_result {
            std::invoke(std::forward<F>(fn), e);
            return Walk_action::next;
        },
        caller);
}

template <typename Pred>
    requires std::predicate<Pred, const Dir_entry &>
auto Dir_walker::partition(Pred pred, std::source_location caller) const noexcept
    -> Walk_result_t<std::pair<std::vector<Dir_entry>, std::vector<Dir_entry>>>
{
    std::vector<Dir_entry> yes, no;
    return run(
               [&](const Dir_entry &e) -> Walk_result {
                   (std::invoke(pred, e) ? yes : no).push_back(e);
                   return Walk_action::next;
               },
               caller)
        .transform([&] { return std::pair{std::move(yes), std::move(no)}; });
}

template <typename T, std::invocable<T, const Dir_entry &> F>
auto Dir_walker::fold(T init, F &&fn, std::source_location caller) const noexcept -> Walk_result_t<T>
{
    T acc = std::move(init);
    return run(
               [&](const Dir_entry &e) -> Walk_result {
                   acc = std::invoke(std::forward<F>(fn), std::move(acc), e);
                   return Walk_action::next;
               },
               caller)
        .transform([&] { return std::move(acc); });
}

template <std::invocable<Dir_walker &> F>
auto Dir_walker::apply(F &&fn) -> Dir_walker &
{
    std::invoke(std::forward<F>(fn), *this);
    return *this;
}

template <std::invocable<const Dir_entry &> F>
auto Dir_walker::run(F &&fn, std::source_location caller) const noexcept -> Walk_result_t<void>
{
    namespace fs = std::filesystem;

    if (root_.empty()) {
        bld::log::w("Dir_walker: empty root ({})", caller.function_name());
        return std::unexpected{Walk_error{.kind = Walk_error::Kind::fs, .code = std::make_error_code(std::errc::invalid_argument)}};
    }

    std::error_code ec;
    const auto iter_opts = follow_symlinks_ ? fs::directory_options::skip_permission_denied | fs::directory_options::follow_directory_symlink
                                            : fs::directory_options::skip_permission_denied;

    auto handle = [&](const Dir_entry &e) -> Walk_result {
        if (is_skipped(e)) {
            return Walk_action::skip_dir;
        }
        if (!is_visible(e)) {
            return Walk_action::next;
        }
        if (!passes_filters(e)) {
            return Walk_action::next;
        }
        return std::invoke(fn, e);
    };

    if (!recursive_) {
        fs::directory_iterator it{root_, iter_opts, ec};
        if (ec) {
            return std::unexpected{Walk_error{.kind = Walk_error::Kind::fs, .code = ec}};
        }

        for (const auto &raw : it) {
            const auto [action, err] = handle(make_entry(raw, 0));
            if (err) {
                return std::unexpected{Walk_error{.kind = Walk_error::Kind::visitor, .code = err}};
            }
            if (action == Walk_action::stop) {
                return {};
            }
        }
        return {};
    }

    fs::recursive_directory_iterator it{root_, iter_opts, ec};
    if (ec) {
        return std::unexpected{Walk_error{.kind = Walk_error::Kind::fs, .code = ec}};
    }

    for (const auto &raw : it) {
        const int d = it.depth();
        if (d >= max_depth_) {
            it.disable_recursion_pending();
        }

        const auto [action, err] = handle(make_entry(raw, d));

        if (err) {
            return std::unexpected{Walk_error{.kind = Walk_error::Kind::visitor, .code = err}};
        }

        switch (action) {
        case Walk_action::stop:
            it = {};
            return {};
        case Walk_action::skip_dir:
            it.disable_recursion_pending();
            break;
        case Walk_action::next:
            break;
        }
    }

    return {};
}

template <detail::Valid_visitor V>
inline auto walk(std::string_view root, V &&visitor, std::source_location caller) noexcept
{
    return Dir_walker{root}.walk(std::forward<V>(visitor), caller);
}

} // namespace bld::fs

// IMPL SECTION 11 — Runtime environment (thread-safe, bld::env::get/set)
//   Meyers singleton — no init() needed, first use constructs mutex thread-safely.
//   Every has/get/set/unset/get_all locks the same mutex, so read+write is safe.
namespace bld::env::detail {
inline std::mutex &mtx()
{
    static std::mutex m;
    return m;
}
} // namespace bld::env::detail

[[nodiscard]] auto bld::env::has(std::string_view key) -> bool
{
    if (key.empty() || key.find('=') != std::string_view::npos || key.find('\0') != std::string_view::npos) {
        return false;
    }
    std::string k{key};
    std::lock_guard<std::mutex> lk(detail::mtx());
    const char *v = std::getenv(k.c_str());
    return v != nullptr;
}

[[nodiscard]] auto bld::env::get(std::string_view key) -> std::optional<std::string>
{
    if (key.empty() || key.find('=') != std::string_view::npos || key.find('\0') != std::string_view::npos) {
        return std::nullopt;
    }
    std::string k{key};
    std::lock_guard<std::mutex> lk(detail::mtx());
    const char *v = std::getenv(k.c_str());
    if (!v) {
        return std::nullopt;
    }
    return std::string{v};
}

[[nodiscard]] auto bld::env::get_or(std::string_view key, std::string_view fallback) -> std::string
{
    if (auto v = get(key)) {
        return *v;
    }
    return std::string{fallback};
}

[[nodiscard]] auto bld::env::get_all() -> std::unordered_map<std::string, std::string>
{
    std::unordered_map<std::string, std::string> out;
    std::lock_guard<std::mutex> lk(detail::mtx());
#ifdef _WIN32
    // Windows: GetEnvironmentStringsA returns double-null terminated block.
    char *block = ::GetEnvironmentStringsA();
    if (!block) {
        return out;
    }
    for (char *p = block; *p;) {
        std::string_view entry{p};
        auto eq = entry.find('=');
        // Skip the per-drive entries like "=C:=C:\\" and entries without '='
        if (eq != std::string_view::npos && eq != 0 && entry[0] != '=') {
            out.emplace(std::string{entry.substr(0, eq)}, std::string{entry.substr(eq + 1)});
        }
        p += entry.size() + 1;
    }
    ::FreeEnvironmentStringsA(block);
#else
    if (!::environ) {
        return out;
    }
    for (char **e = ::environ; *e; ++e) {
        std::string_view entry{*e};
        auto eq = entry.find('=');
        if (eq == std::string_view::npos) {
            continue;
        }
        out.emplace(std::string{entry.substr(0, eq)}, std::string{entry.substr(eq + 1)});
    }
#endif
    return out;
}

auto bld::env::set(std::string_view key, std::string_view value, bool overwrite) -> std::expected<void, Err>
{
    if (key.empty() || key.find('=') != std::string_view::npos || key.find('\0') != std::string_view::npos
        || value.find('\0') != std::string_view::npos) {
        bld::log::e("env: set failed: invalid key '{}'.", key);
        return std::unexpected(Err::erc(std::errc::invalid_argument, std::format("invalid env key/value: '{}'", key)));
    }
    std::string k{key};
    std::string v{value};
    std::lock_guard<std::mutex> lk(detail::mtx());
    if (!overwrite) {
        if (std::getenv(k.c_str()) != nullptr) {
            bld::log::d("env: set('{}') skipped (already set, overwrite=false).", k);
            return {};
        }
    }
#ifdef _WIN32
    errno_t err = ::_putenv_s(k.c_str(), v.c_str());
    if (err != 0) {
        bld::log::e("env: set('{}') failed: {}.", k, std::strerror(err));
        return std::unexpected(Err::erno(err, std::format("setenv failed for '{}'", k)));
    }
#else
    int rc = ::setenv(k.c_str(), v.c_str(), 1);
    if (rc != 0) {
        bld::log::e("env: set('{}') failed: {}.", k, std::strerror(errno));
        return std::unexpected(Err::erno(errno, std::format("setenv failed for '{}'", k)));
    }
#endif
    bld::log::i("env: set '{}'.", k);
    return {};
}

auto bld::env::unset(std::string_view key) -> std::expected<void, Err>
{
    if (key.empty() || key.find('=') != std::string_view::npos || key.find('\0') != std::string_view::npos) {
        bld::log::e("env: unset failed: invalid key '{}'.", key);
        return std::unexpected(Err::erc(std::errc::invalid_argument, std::format("invalid env key: '{}'", key)));
    }
    std::string k{key};
    std::lock_guard<std::mutex> lk(detail::mtx());
#ifdef _WIN32
    // _putenv_s with empty value deletes; also try SetEnvironmentVariable for completeness.
    errno_t err = ::_putenv_s(k.c_str(), "");
    if (err != 0) {
        bld::log::e("env: unset('{}') failed.", k);
        return std::unexpected(Err::erno(err, std::format("unsetenv failed for '{}'", k)));
    }
    // Ensure OS block is cleared too
    ::SetEnvironmentVariableA(k.c_str(), nullptr);
#else
    int rc = ::unsetenv(k.c_str());
    if (rc != 0) {
        bld::log::e("env: unset('{}') failed: {}.", k, std::strerror(errno));
        return std::unexpected(Err::erno(errno, std::format("unsetenv failed for '{}'", k)));
    }
#endif
    bld::log::i("env: unset '{}'.", k);
    return {};
}

// END OF IMPLEMENTATION

#endif // B_LDR_IMPLEMENTATION_ONCE
#endif // B_LDR_IMPLEMENTATION
