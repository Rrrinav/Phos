#ifndef CL_HPP_
#define CL_HPP_

#include <sys/types.h>
#include <expected>
#include <algorithm>
#include <array>
#include <cassert>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <expected>
#include <format>
#include <functional>
#include <iostream>
#include <iterator>
#include <memory>
#include <new>
#include <optional>
#include <source_location>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>
#include <variant>

// -----------------------------------------------------------------------------
//  CONFIGURATION & MACROS
// -----------------------------------------------------------------------------

namespace cl::debug
{
[[noreturn]]
auto todo(std::string &&s, std::source_location loc = std::source_location::current()) -> void;
[[noreturn]]
inline auto todo(std::source_location loc = std::source_location::current()) -> void { todo("", loc); }
}  // namespace cl::debug

namespace cl::asrt {
template <typename... Args>
constexpr auto t(bool cond, std::format_string<Args...> fmt, Args &&...args) -> void;
template <typename... Args>
constexpr auto f(bool cond, std::format_string<Args...> fmt, Args &&...args) -> void;
template <typename... Args>
constexpr auto tloc(std::source_location loc, bool cond, std::format_string<Args...> fmt, Args &&...args) -> void;
template <typename... Args>
constexpr auto floc(std::source_location loc, bool cond, std::format_string<Args...> fmt, Args &&...args) -> void;

}  // namespace cl::asrt

// Trying to follow semver
namespace cl::vrsn
{

constexpr inline int major = 0;
constexpr inline int minor = 0;
constexpr inline int patch = 0;

namespace detail
{

consteval std::array<char, 16> make_version()
{
    std::array<char, 16> buf{};
    int pos = 0;

    auto append_int = [&](int v)
    {
        char tmp[10]{};
        int n = 0;

        do
        {
            tmp[n++] = char('0' + (v % 10));
            v /= 10;
        } while (v);

        while (n--) buf[pos++] = tmp[n];
    };

    append_int(major);
    buf[pos++] = '.';
    append_int(minor);
    buf[pos++] = '.';
    append_int(patch);
    buf[pos] = '\0';

    return buf;
}

constexpr auto storage = detail::make_version();
}  // namespace detail
constexpr const char *val = detail::storage.data();
consteval const char *get() { return val; }
}  // namespace cl::vrsn

namespace cl::impl
{
inline std::optional<std::function<void(std::string s)>> panic_handler = std::nullopt;
}

namespace cl
{
// -----------------------------------------------------------------------------
// UTILITIES & CONCEPTS
// -----------------------------------------------------------------------------
using Num           = long long;
using Fp_Num        = double;
using Text          = std::string;
using Flag          = bool;
using Opt_id        = uint32_t;
using Subcommand_id = uint32_t;
inline constexpr Subcommand_id global_command = 0;

template <typename T>
concept Supported_Scalar_C = std::is_same_v<T, bool> || std::is_same_v<T, cl::Num> || std::is_same_v<T, cl::Fp_Num> || std::is_same_v<T, std::string> || std::is_same_v<T, std::string_view>;
template <typename T, std::size_t N>
requires Supported_Scalar_C<T>
using Fix_list = std::array<T, N>;

template <typename T>
requires Supported_Scalar_C<T>
using List = std::vector<T>;

template <typename T>
struct is_std_array : std::false_type { };
template <typename U, std::size_t N>
struct is_std_array<std::array<U, N>> : std::true_type { };
template <typename T>
inline constexpr bool is_std_array_v = is_std_array<T>::value;

template <typename T>
struct is_std_vector : std::false_type { };
template <typename U, typename A>
struct is_std_vector<std::vector<U, A>> : std::true_type { };
template <typename T>
inline constexpr bool is_std_vector_v = is_std_vector<T>::value;

// Types we can PARSE (Scalars and Arrays)
template <typename T>
concept Parsable_Type_C = Supported_Scalar_C<T> || (is_std_array_v<T> && Supported_Scalar_C<typename T::value_type>);

template <typename T>
concept Gettable_Scalar_C = std::is_same_v<T, Num> || std::is_same_v<T, Fp_Num> || std::is_same_v<T, std::string> || std::is_same_v<T, bool>;

template <typename T>
concept Gettable_Type_C = Gettable_Scalar_C<T> || (is_std_vector_v<T> && Gettable_Scalar_C<typename T::value_type>) || (is_std_array_v<T> && Gettable_Scalar_C<typename T::value_type>);

template <typename V, typename T>
concept Validator_C = requires(const V &v, const T &val) {
    { v(val) } -> std::convertible_to<std::expected<void, std::string>>;
    { v.help() } -> std::convertible_to<std::string>;
};
// -----------------------------------------------------------------------------
// MEMORY MANAGEMENT
// -----------------------------------------------------------------------------

namespace detail
{
template <typename T>
struct get_inner_type
{
    using type = T;
};

// Specialization for vector to extract inner type
template <typename T, typename A>
struct get_inner_type<std::vector<T, A>>
{
    using type = T;
};

class Arena
{
public:
    explicit Arena(std::size_t block_size = 64 * 1024) : block_size_(block_size) { add_block(); }
    Arena(const Arena &) = delete;
    Arena &operator=(const Arena &) = delete;
    Arena(Arena &&) noexcept = default;
    Arena &operator=(Arena &&) noexcept = default;

    ~Arena()
    {
        for (auto it = destructors_.rbegin(); it != destructors_.rend(); ++it) (*it)();
        for (auto &b : blocks_) ::operator delete(b.data);
    }

    template <typename... Args>
    constexpr auto deflt(Args... values);

    [[nodiscard]]
    void *alloc(std::size_t n, std::size_t align = alignof(std::max_align_t));

    template <typename T, typename... Args>
    [[nodiscard]]
    T *make(Args &&...args);

    [[nodiscard]]
    std::string_view str(std::string_view s);

    [[nodiscard]]
    std::string_view str(const char *s);

    [[nodiscard]]
    std::size_t blocks() const noexcept;

private:
    void add_block(std::size_t size = 0);
    static std::byte *align_ptr(std::byte *p, std::size_t align);

    struct Block
    {
        std::byte *data;
        std::byte *cur;
        std::byte *end;
    };
    std::size_t block_size_;
    std::vector<Block> blocks_;
    std::vector<std::function<void()>> destructors_;
};

}  // namespace detail
// -----------------------------------------------------------------------------
// OPTIONS & VALIDATORS
// -----------------------------------------------------------------------------

template <typename T>
requires Supported_Scalar_C<T>
class Range
{
    T l, h;

public:
    Range(T l, T h) : l(l), h(h) {}
    auto operator()(const T &val) const -> std::expected<void, std::string>
    {
        if (l <= val && val <= h)
            return {};
        return std::unexpected(std::format("Value {} out of range [{}, {}]", val, l, h));
    }
    std::string help() const { return std::format("Range[{}, {}]", l, h); }
};

enum class List_type    : uint8_t { Consecutive, Delimited };
enum class Multi_type   : uint8_t { Repeat, Delimited };
enum class Bool_type    : uint8_t { Flag, Explicit };
enum class Binding_type : uint8_t { Equal, Next, Both };

enum class Flags : uint16_t
{
    Multi = 1,
    Required = 1 << 1,
    Hidden = 1 << 2,
    Env = 1 << 3,
    Default = 1 << 4,
    Explicit_Bool = 1 << 5,
    Exclusive = 1 << 6
};
constexpr auto operator*(Flags f) -> const uint16_t { return static_cast<uint16_t>(f); }
constexpr Flags operator|(Flags a, Flags b) noexcept { return static_cast<Flags>(std::to_underlying(a) | std::to_underlying(b)); }
constexpr Flags operator&(Flags a, Flags b) noexcept { return static_cast<Flags>(std::to_underlying(a) & std::to_underlying(b)); }
constexpr Flags operator^(Flags a, Flags b) noexcept { return static_cast<Flags>(std::to_underlying(a) ^ std::to_underlying(b)); }
constexpr Flags operator~(Flags f) noexcept { return static_cast<Flags>(~std::to_underlying(f)); }
constexpr Flags &operator|=(Flags &a, Flags b) noexcept { return a = a | b; }
constexpr Flags &operator&=(Flags &a, Flags b) noexcept { return a = a & b; }

struct List_cfg
{
    List_type type;
    std::string delimiter;
};

struct Multi_cfg
{
    Multi_type type;
    std::string delimiter;
};

template <typename T>
struct Opt
{
    using value_type = T;
    std::array<std::string, 2> args{};
    std::string desc{};
    T default_val{};
    std::string meta_{""};
    std::string env_{""};
    uint16_t flags = 0;
    List_cfg list_cfg{List_type::Consecutive, ","};
    Multi_cfg multi_cfg{Multi_type::Repeat, ","};
    Subcommand_id sub_cmd_id{global_command};
    struct Validator_entry
    {
        std::function<std::expected<void, std::string>(const T &)> func;
        std::string help;
    };
    std::vector<Validator_entry> validators_;
    std::source_location loc{};
};

struct Name_config
{
    std::string_view short_name;
    std::string_view long_name;
    std::source_location loc;
};

struct Single_name_cfg
{
    std::string_view long_name;
    std::source_location loc;
};

struct Subcommand
{
    Subcommand_id id;
    std::string_view name;
    std::string_view description;
    uint16_t flags;
    std::vector<Opt_id> child_options;
    std::vector<Subcommand_id> child_subcommands;

    Subcommand_id parent_id;

    std::unordered_map<std::string_view, Subcommand_id> child_to_id;
    std::unordered_map<std::string_view, Opt_id> long_arg_to_id_;
    std::unordered_map<std::string_view, Opt_id> short_arg_to_id_;
};

constexpr auto name(std::string_view l, std::source_location loc = std::source_location::current()) { return Single_name_cfg{l, loc}; }

constexpr auto name(std::string_view s, std::string_view l, std::source_location loc = std::source_location::current())
{
    return Name_config{s, l, loc};
}

constexpr auto desc(std::string_view sv)
{
    return [sv]<typename Opt>(Opt &o) constexpr { o.desc = sv; };
}

constexpr auto required()
{
    return []<typename Opt>(Opt &o) constexpr { o.flags |= *Flags::Required; };
}

constexpr auto explicit_bool()
{
    return []<typename Opt>(Opt &o) constexpr
    {
        static_assert(std::is_same_v<typename Opt::value_type, bool>, "Non bools/flags can't have be explicit bools");
        o.flags |= *Flags::Explicit_Bool;
    };
}

constexpr auto multi(Multi_type type = Multi_type::Repeat, std::string_view d = ",")
{
    cl::asrt::t((!d.empty()), "Delimiter cannot be empty.");
    return [type, d]<typename Opt>(Opt &o) constexpr
    {
        static_assert(!is_std_array_v<typename Opt::value_type>, "Fixed-size arrays cannot be multi-value. Use vector.");
        o.multi_cfg.type = type;
        o.multi_cfg.delimiter = d;
        o.flags |= *Flags::Multi;
    };
}

constexpr auto array(List_type t = List_type::Consecutive, std::string_view d = ",")
{
    cl::asrt::t((!d.empty()), "delimiter cannot be empty.");
    return [t, d]<typename Opt>(Opt &o) constexpr
    {
        static_assert(is_std_array_v<typename Opt::value_type>, "Array configuration can only be done on arrays");
        o.list_cfg.type = t;
        o.list_cfg.delimiter = d;
    };
}

template <typename T>
constexpr auto deflt(T &&val)
{
    return [val = std::forward<T>(val)]<typename Opt>(Opt &o) constexpr
    {
        o.default_val = std::move(val);
        o.flags |= *Flags::Default;
    };
}

template <typename T>
constexpr auto flags(uint16_t f)
{
    return [f]<typename Opt>(Opt &o) constexpr { o.flags |= f; };
}

template <typename... Args>
constexpr auto deflt(Args... values)
{
    return [=]<typename Opt>(Opt &o) constexpr
    {
        using T = typename Opt::value_type;
        static_assert(is_std_array_v<T>, "Variadic deflt() can only be used with std::array<T, N>");
        constexpr std::size_t N = std::tuple_size_v<T>;
        static_assert(sizeof...(Args) == N, "Number of default values must match array size");
        o.default_val = T{static_cast<typename T::value_type>(values)...};
        o.flags |= *Flags::Default;
    };
}

template <typename... Vs>
constexpr auto validators(Vs &&...vals)
{
    return [=]<typename Opt>(Opt &o)
    {
        using T = typename Opt::value_type;
        (
            [&]
            {
                static_assert(Validator_C<Vs, T>,
                    "Argument is not a valid Validator, must have operator() -> expected<void, std::string>; and help() -> std::string;");
                o.validators_.push_back({.func = [v = vals](const T &x) { return v(x); }, .help = vals.help()});
            }(),
            ...);
    };
}

constexpr auto env(std::string_view e)
{
    return [e]<typename Opt>(Opt &o) constexpr
    {
        o.flags |= (*Flags::Env);
        o.env_ = e;
    };
}

constexpr auto meta(std::string_view m)
{
    return [m]<typename Opt>(Opt &o) constexpr { o.meta_ = m; };
}

constexpr auto sub_cmd(Subcommand_id id)
{
    return [id]<typename Opt>(Opt &o) constexpr { o.sub_cmd_id = id; };
}

template <typename Config, typename T>
concept Configurer = requires(Config &&c, Opt<T> &opt) {
    { std::forward<Config>(c)(opt) } -> std::convertible_to<void>;
};

enum Opt_type : uint8_t { Int, Bool, Str, Float };
enum Storage_kind : uint8_t { Scalar, Array, Vector };

// Get Opt_type from Parsable types (used in add())
template <typename T>
requires Parsable_Type_C<T>
[[nodiscard]]
consteval Opt_type parsable_to_opt_type()
{
    if constexpr (is_std_array_v<T>)
        return parsable_to_opt_type<typename T::value_type>();
    if constexpr (std::is_same_v<T, bool>)
        return Opt_type::Bool;
    if constexpr (std::integral<T>)
        return Opt_type::Int;
    if constexpr (std::floating_point<T>)
        return Opt_type::Float;
    if constexpr (std::is_convertible_v<T, std::string>)
        return Opt_type::Str;
    if constexpr (std::is_convertible_v<T, std::string_view>)
        return Opt_type::Str;
}

// Get Opt_type from Gettable types (used in get())
template <typename T>
requires Gettable_Type_C<T>
[[nodiscard]]
consteval Opt_type gettable_to_opt_type()
{
    if constexpr (is_std_array_v<T>)
        return gettable_to_opt_type<typename T::value_type>();
    if constexpr (is_std_vector_v<T>)
        return gettable_to_opt_type<typename T::value_type>();
    if constexpr (std::is_same_v<T, bool>)
        return Opt_type::Bool;
    if constexpr (std::integral<T>)
        return Opt_type::Int;
    if constexpr (std::floating_point<T>)
        return Opt_type::Float;
    if constexpr (std::is_convertible_v<T, std::string>)
        return Opt_type::Str;
    if constexpr (std::is_convertible_v<T, std::string_view>)
        return Opt_type::Str;
}

// Get Storage_kind from any Gettable type
template <typename T>
requires Gettable_Type_C<T>
[[nodiscard]]
consteval Storage_kind type_to_storage_kind()
{
    if constexpr (is_std_vector_v<T>)
        return Storage_kind::Vector;
    if constexpr (is_std_array_v<T>)
        return Storage_kind::Array;
    return Storage_kind::Scalar;
}

// Map enum to canonical storage types
template <Opt_type T>
struct _opt_type_to_canonical_type_t_ { };
template <> struct _opt_type_to_canonical_type_t_<Opt_type::Int> { using value = long long; };
template <> struct _opt_type_to_canonical_type_t_<Opt_type::Bool> { using value = bool; };
template <> struct _opt_type_to_canonical_type_t_<Opt_type::Str> { using value = std::string; };
template <> struct _opt_type_to_canonical_type_t_<Opt_type::Float> { using value = double; };

// -----------------------------------------------------------------------------
// PARSER CONFIG & STREAM
// -----------------------------------------------------------------------------

enum class Repeated_scalar_policy : uint8_t { REJECT, FIRST, LAST };

struct Parser_config
{
    Binding_type value_binding = Binding_type::Both;  // '=' binding not allowed in case of list type consecutive
    bool allow_combined_short_flags = true;
    bool allow_short_value_concat = true;
    bool stop_on_double_dash = true;
    Repeated_scalar_policy repeated_scalar = Repeated_scalar_policy::REJECT;
    bool allow_empty_arrays = false;
};

struct Parse_err
{
    struct err
    {
        std::string option{};
        std::string message{};
    };
    std::vector<err> errors{};

    template <typename... Args>
    auto push_err(const std::string &option, std::format_string<Args...> fmt, Args &&...a)
    {
        this->errors.push_back({option, std::format(fmt, std::forward<Args>(a)...)});
    }

    template <typename... Args>
    auto push_err(std::string_view option, std::format_string<Args...> fmt, Args &&...args)
    {
        return this->push_err(std::string(option), fmt, std::forward<Args>(args)...);
    }

private:
};

using Runtime_value = std::variant<cl::Text, cl::Num, cl::Flag, cl::Fp_Num, std::vector<cl::Text>, std::vector<cl::Num>,
    std::vector<cl::Flag>, std::vector<cl::Fp_Num>, std::monostate>;

struct Runtime
{
    Runtime_value runtime_value{std::monostate()};
    bool parsed{false};
    std::size_t count{0};

    Runtime &operator=(const Runtime &r) = default;
};

namespace detail {

    class Arg_stream
    {
        std::span<char *> args_;
        size_t cur_ = 1;
    public:
        Arg_stream(int argc, char **argv) : args_(argv, static_cast<size_t>(argc)) {}

        [[nodiscard]] bool empty() const { return cur_ >= args_.size(); }
        [[nodiscard]] std::optional<std::string_view> peek() const;
        std::string_view pop();
        std::size_t size();
        void rewind();
    };
    struct Option
    {
        Opt_id id;
        std::array<std::string_view, 2> names;
        std::string_view desc;
        Opt_type type;
        Storage_kind storage;
        uint16_t flags;
        List_cfg list_cfg;
        Multi_cfg multi_cfg;
        std::size_t arity;
        std::string_view meta;
        std::string_view env;
        std::vector<std::string_view> validator_helps;
        std::string_view default_hints;
        Runtime_value default_value;
        Subcommand_id sub_id;

        std::function<std::expected<void, std::string>(const Runtime_value &)> validate;
    };

    struct Commands_schema
    {
        Arena arena_ = Arena();
        std::vector<Subcommand *> sub_cmds_{};
        std::vector<detail::Option *> options_{};
    };
}

// We dont need to write copy cosntructors because we want to do shallow copy since we want to maintain a shared state
// and the lifetimes are managed by shared pointer;
struct Parse_res
{
    std::shared_ptr<detail::Commands_schema> _schema = nullptr;
    std::shared_ptr<std::vector<Runtime>> _runtime = nullptr;
    std::unordered_set<Subcommand_id> _visited_subcommands{};
    Subcommand_id curr_subcmd = global_command;

    template <typename T>
    requires Gettable_Type_C<T>
    [[nodiscard]]
    auto get(Opt_id id) const -> std::optional<T>;

    template <typename T>
    requires Gettable_Type_C<T>
    [[nodiscard]]
    auto get(Opt_id id, T &val) const -> std::expected<void, std::string>;

    template <typename T>
    requires Gettable_Type_C<T>
    [[nodiscard]]
    auto get(std::string_view key) const -> std::optional<T>;

    auto get_subcmd(Subcommand_id id) const -> std::optional<Parse_res>;

    auto get_subcmd(std::string_view key) const -> std::optional<Parse_res>;

    template <typename T, typename... Args>
    requires Gettable_Type_C<T> && (sizeof...(Args) > 0)
    [[nodiscard]]
    auto get(std::string_view subcmd_name, Args &&...rest) const -> std::optional<T>;

    [[nodiscard]]
    auto is_seen(Opt_id id) -> bool;

    [[nodiscard]]
    auto is_subcmd_chosen(Subcommand_id id) -> bool;

    [[nodiscard]]
    auto is_seen(std::string_view key) -> bool;

    template <typename T, typename... Args>
    requires cl::Gettable_Type_C<T> && (sizeof...(Args) > 0)
    [[nodiscard]]
    auto get(T* val, std::string_view subcmd_name, Args &&...rest) const -> std::expected<void, std::string>;

    template <typename T>
    requires Gettable_Type_C<T>
    [[nodiscard]]
    auto get(T* val, std::string_view key) const -> std::expected<void, std::string>;

private:
    auto get_id_in_curr_scope(std::string_view key) const -> std::optional<Opt_id>;
};

inline std::ostream &operator<<(std::ostream &os, const cl::Parse_err &err);

namespace detail {
    struct Parse_ctx
    {
        Arg_stream &_args;
        Parse_res &_res;
        Parse_err &_err;
        const Parser_config &_cfg;
        bool _stop_flags;
        std::string_view _curr_key{};
        std::size_t _positional_ind{0};
        Subcommand_id _active_sub_id{global_command};
        Subcommand *_active_subcomamnd;

        Parse_ctx(Arg_stream &a, Parse_err &e, Parse_res &r, Parser_config &c) : _args(a), _res(r), _err(e), _cfg(c), _stop_flags(false) {}
        Parse_ctx(const Parse_ctx &p) = delete;
        Parse_ctx(const Parse_ctx &&p) = delete;
        Parse_ctx operator=(const Parse_ctx &p) = delete;
        Parse_ctx operator=(const Parse_ctx &&p) = delete;

        // Helper to push errors easily
        template <typename... Args>
        void error(std::format_string<Args...> fmt, Args &&...a);
    };
};

class Parser
{
public:
    std::vector<std::string> truthy_strs = {"y", "true", "yes", "t"};
    std::vector<std::string> falsy_strs = {"n", "false", "no", "f"};
    Parser_config cfg_;

    explicit Parser(std::string s = "", std::string des = "", std::size_t reserve = 15);
    void add_explicit_bool_strs(const std::vector<std::string> &truthy, const std::vector<std::string> &falsy);
    template <typename T, typename... Configs>
    requires((Configurer<Configs, T> && ...) && Parsable_Type_C<T>)
    auto add(Name_config name_cfg, Configs &&...confs) -> Opt_id;
    auto add_sub_cmd(const std::string &name, const std::string &desc, uint16_t flags, Subcommand_id parent_id = global_command, int reserve = 4) -> Subcommand_id;
    auto parse(int argc, char *argv[]) -> std::expected<Parse_res, Parse_err>;
    auto print_help(std::ostream &os = std::cout, Subcommand_id sub_id = global_command, std::optional<Opt_id> opt_id = std::nullopt) -> void;
    template <typename T, typename... Configs>
    requires((Configurer<Configs, T> && ...) && Supported_Scalar_C<T>)
    auto positional(Single_name_cfg name, Configs &&...confs) -> Opt_id;

private:
    template <typename T>
    requires Parsable_Type_C<T>
    inline auto add_impl(Opt<T> opt) -> Opt_id;

    template <typename T>
    requires cl::Supported_Scalar_C<T>
    inline auto add_positional_impl(Opt<T> opt) -> Opt_id;

    inline auto assign_id() -> Opt_id { return this->_next_id++; }

    template <typename Dest>
    requires Supported_Scalar_C<Dest>
    auto string_to_value(std::string_view s) -> std::expected<Dest, std::string>;

    void handle_long_token(detail::Parse_ctx &ctx, std::string_view body);
    void handle_short_token(detail::Parse_ctx &ctx, std::string_view body);
    bool add_short_combined(detail::Parse_ctx &ctx, std::string_view body);
    bool acquire_value(detail::Parse_ctx &ctx, detail::Option *opt, std::optional<std::string_view> explicit_val);
    void inject_value(detail::Parse_ctx &ctx, detail::Option *opt, std::span<const std::string_view> raw_values);
    void assign_true(Runtime &rt);
    void handle_positional_and_subcmds(detail::Parse_ctx &ctx, std::string_view value);

    static constexpr int _sn_index = 0;
    static constexpr int _ln_index = 1;

    std::string _name;
    std::string _description;
    Opt_id _next_id = 0;

    std::vector<Runtime> _runtime;
    std::shared_ptr<detail::Commands_schema> _schema;
    std::vector<Opt_id> _positional_ids;
};

}  // namespace cl

template <>
struct std::formatter<cl::Opt_type> : std::formatter<std::string_view>
{
    auto format(cl::Opt_type t, format_context &ctx) const;
};

template <>
struct std::formatter<cl::Storage_kind> : std::formatter<std::string_view>
{
    auto format(cl::Storage_kind k, format_context &ctx) const;
};

template <>
struct std::formatter<cl::detail::Option, char>
{
    constexpr auto parse(std::format_parse_context &ctx);
    auto format(const cl::detail::Option &o, std::format_context &ctx) const;
};

template <>
struct std::formatter<cl::Parse_err>
{
    constexpr auto parse(std::format_parse_context &ctx);
    template <typename FormatContext>
    auto format(const cl::Parse_err &pe, FormatContext &ctx) const;
};

#ifdef CL_IMPLEMENTATION
#include <print>
#include <charconv>
#include <cctype>
#include <stdexcept>
#include <ranges>
#include <iostream>
#include <iomanip>

namespace cl::debug {
auto todo(std::string &&s, std::source_location loc) -> void
{
    std::println("{}:{}:{}: [TODO]: {}", loc.file_name(), loc.line(), loc.column(), loc.function_name());
    std::println("         {}", s);
    std::exit(EXIT_FAILURE);
}
} // cl::debuc>

namespace cl::asrt {

template <typename... Args>
constexpr auto t(bool cond, std::format_string<Args...> fmt, Args &&...args) -> void
{
    if (!cond)
    {
        std::println(stderr, "[Assert ERR]: {}", std::format(fmt, std::forward<Args>(args)...));
        std::exit(EXIT_FAILURE);
    }
}

template <typename... Args>
constexpr auto tloc(std::source_location loc, bool cond, std::format_string<Args...> fmt, Args &&...args) -> void
{
    if (!cond)
    {
        std::println(stderr, "{}:{}: [Assert ERR]: {}", loc.file_name(), loc.line(), std::format(fmt, std::forward<Args>(args)...));
        std::exit(EXIT_FAILURE);
    }
}

template <typename... Args>
constexpr auto floc(std::source_location loc, bool cond, std::format_string<Args...> fmt, Args &&...args) -> void
{
    if (cond)
    {
        std::println(stderr, "{}:{}: [Assert ERR]: {}", loc.file_name(), loc.line(), std::format(fmt, std::forward<Args>(args)...));
        std::exit(EXIT_FAILURE);
    }
}

template <typename... Args>
constexpr auto f(bool cond, std::format_string<Args...> fmt, Args &&...args) -> void
{
    if (cond)
    {
        std::println(stderr, "[Assert ERR]: {}", std::format(fmt, std::forward<Args>(args)...));
        std::exit(EXIT_FAILURE);
    }
}

}  // namespace cl::asrt


namespace cl::detail {
template <typename... Args>
constexpr auto Arena::deflt(Args... values)
{
    return [=]<typename Opt>(Opt &o) constexpr
    {
        using T = typename Opt::value_type;
        static_assert(is_std_array_v<T>, "Variadic deflt() can only be used with std::array<T, N>");
        constexpr std::size_t N = std::tuple_size_v<T>;
        static_assert(sizeof...(Args) == N, "Number of default values must match array size");
        o.default_val_ = T{static_cast<typename T::value_type>(values)...};
        o.has_default_ = true;
    };
}

void *Arena::alloc(std::size_t n, std::size_t align)
{
    Block &b = blocks_.back();
    std::byte *p = align_ptr(b.cur, align);

    if (p + n > b.end)
    {
        add_block(std::max(block_size_, n + align));
        return alloc(n, align);
    }

    b.cur = p + n;
    return p;
}

template <typename T, typename... Args>
T *Arena::make(Args &&...args)
{
    void *mem = alloc(sizeof(T), alignof(T));
    T *obj = ::new (mem) T(std::forward<Args>(args)...);

    if constexpr (!std::is_trivially_destructible_v<T>)
        destructors_.push_back([obj]() { obj->~T(); });

    return obj;
}

std::string_view Arena::str(std::string_view s)
{
    if (s.empty())
        return {};
    char *mem = static_cast<char *>(alloc(s.size() + 1, alignof(char)));
    std::memcpy(mem, s.data(), s.size());
    mem[s.size()] = '\0';
    return {mem, s.size()};
}

[[nodiscard]]
std::string_view Arena::str(const char *s)
{
    if (!s)
        return {};
    return str(std::string_view{s});
}

[[nodiscard]]
std::size_t Arena::blocks() const noexcept
{
    return blocks_.size();
}

void Arena::add_block(std::size_t size)
{
    std::size_t sz = size ? size : block_size_;
    std::byte *mem = static_cast<std::byte *>(::operator new(sz));
    blocks_.push_back(Block{mem, mem, mem + sz});
}

std::byte *Arena::align_ptr(std::byte *p, std::size_t align)
{
    auto ip = reinterpret_cast<std::uintptr_t>(p);
    auto aligned = (ip + align - 1) & ~(align - 1);
    return reinterpret_cast<std::byte *>(aligned);
}

}


namespace cl::detail {
    struct Help_entry
    {
        std::string left;
        std::string right;
        bool is_header = false;
    };

    inline std::string format_option_flags(const detail::Option *opt)
    {
        std::string s;
        // Short name
        if (!opt->names[0].empty())
            s += std::format("-{}", opt->names[0]);

        // Comma if both exist
        if (!opt->names[0].empty() && !opt->names[1].empty())
            s += ", ";

        // Long name
        if (!opt->names[1].empty())
            s += std::format("--{}", opt->names[1]);

        // Value hint
        if (opt->arity > 0)
        {
            std::string type_hint = "VAL";
            if (opt->type == Opt_type::Int)
                type_hint = "INT";
            else if (opt->type == Opt_type::Float)
                type_hint = "FLOAT";
            else if (opt->type == Opt_type::Str)
                type_hint = "STR";

            if (!opt->meta.empty())
                type_hint = opt->meta;

            if (opt->arity > 1 || (opt->flags & *Flags::Multi))
                s += std::format(" <{}...>", type_hint);
            else
                s += std::format(" <{}>", type_hint);
        }
        return s;
    }
}  // namespace detail

template <typename T>
constexpr std::string_view type_name()
{
#if defined(__clang__) || defined(__GNUC__)
    std::string_view p = __PRETTY_FUNCTION__;
    auto start = p.find("T = ") + 4;
    auto end = p.find(']', start);
    return p.substr(start, end - start);
#elif defined(_MSC_VER)
    return typeid(T).name();
    //std::string_view p = __FUNCSIG__;
    //auto start = p.find("type_name<") + 10;
    //auto end   = p.find(">(void)");
    //return p.substr(start, end - start);
#else
    return typeid(T).name();
    // return "unsupported compiler: send patches :)";
#endif
}

cl::Parser::Parser(std::string s, std::string des, std::size_t reserve) : _name(std::move(s)), _description(std::move(des)), _schema(std::make_shared<detail::Commands_schema>())
{
    this->_schema->options_.reserve(reserve);
    _runtime.reserve(reserve);
    this->add_sub_cmd("GLOBAL", "Global Context", 0);
}

void cl::Parser::add_explicit_bool_strs(const std::vector<std::string> &truthy, const std::vector<std::string> &falsy)
{
    truthy_strs = truthy;
    falsy_strs = falsy;
}

template <typename T, typename... Configs>
requires((cl::Configurer<Configs, T> && ...) && cl::Parsable_Type_C<T>)
auto cl::Parser::add(Name_config name_cfg, Configs &&...confs) -> Opt_id
{
    Opt<T> opt;
    opt.args[0] = name_cfg.short_name;
    opt.args[1] = name_cfg.long_name;
    opt.loc = name_cfg.loc;

    if (opt.args[0].empty() && opt.args[1].empty())
        throw std::invalid_argument("Option must have at least one name (short or long)");

    (confs(opt), ...);
    try {
        return this->add_impl(opt);
    } catch (...) {
        throw;
    }
}

template <typename T>
requires cl::Parsable_Type_C<T>
inline auto cl::Parser::add_impl(Opt<T> opt) -> Opt_id
{
    // ==================================================================================
    // 1. Helper Lambdas (Name & Env Validation)
    // ==================================================================================
    auto validate_long_name = [&](std::string_view __name) {
        bool valid_chars = std::ranges::all_of(__name, [](char c) { return std::isalnum(c) || c == '-' || c == '_'; });
        cl::asrt::t( valid_chars && !__name.starts_with("-"), "Long option '{}' invalid. Must be alphanumeric/_/-, cannot start with '-'.", __name);
        cl::asrt::t(__name.size() > 1, "Long option '{}' must be > 1 char.", __name);
        if (this->_schema->sub_cmds_[opt.sub_cmd_id]->long_arg_to_id_.contains(__name))
            throw std::logic_error(std::format("Name: {} already exists in {} scope", __name, (this->_schema->sub_cmds_[opt.sub_cmd_id]->name)));
    };

    auto validate_short_name = [&](std::string_view __name)
    {
        cl::asrt::t(__name.size() == 1, "Short option '{}' must be 1 char.", __name);
        cl::asrt::t(std::isalpha(__name[0]), "Short option '{}' must be a letter.", __name);
        if (this->_schema->sub_cmds_[opt.sub_cmd_id]->long_arg_to_id_.contains(__name))
            throw std::logic_error(std::format("Name: {} already exists in {} scope", __name, (this->_schema->sub_cmds_[opt.sub_cmd_id]->name)));
    };

    //auto validate_env_name = [&](std::string_view name)
    //{
    //    if (name.empty() || std::isdigit(name[0]))
    //        return false;
    //    return std::ranges::all_of(name, [](char c) { return std::isalnum(c) || c == '_'; });
    //};

    // ==================================================================================
    // 2. Type Analysis & Storage Configuration
    // ==================================================================================
    constexpr Opt_type target_enum = parsable_to_opt_type<T>();
    using CanonElem = typename _opt_type_to_canonical_type_t_<target_enum>::value;

    const bool is_multi = opt.flags & *Flags::Multi;

    // Safety Checks
    cl::asrt::t(!(is_std_array_v<T> && is_multi), "Arrays cannot be multi. Use vector (scalar + F_MULTI).");
    if ((opt.flags & *Flags::Env) && (target_enum != Opt_type::Str || is_multi))
        throw std::invalid_argument("Environment variable fallback only applies to scalar strings.");

    // Determine Storage Kind and Arity
    Storage_kind storage_kind = Storage_kind::Scalar;
    std::size_t arity = 1;

    if (is_multi)
    {
        storage_kind = Storage_kind::Vector;
        if (opt.multi_cfg.type == Multi_type::Delimited && opt.multi_cfg.delimiter.empty())
            throw std::invalid_argument("DELIMITED multi mode requires non-empty delimiter");
    }
    else if constexpr (is_std_array_v<T>)
    {
        storage_kind = Storage_kind::Array;
        arity = std::tuple_size_v<T>;

        if (opt.list_cfg.type == List_type::Consecutive && cfg_.value_binding == Binding_type::Equal)
            throw std::invalid_argument("CONSECUTIVE arrays incompatible with Binding_type::Equal.");
    }
    else if constexpr (std::is_same_v<T, bool>)
    {
        if (!(opt.flags & *Flags::Explicit_Bool))
            arity = 0;
    }

    // ==================================================================================
    // 3. Register Names & Env
    // ==================================================================================
    auto id = assign_id();

    if (!opt.args[this->_ln_index].empty())
        validate_long_name(opt.args[1]);

    if (!opt.args[this->_sn_index].empty())
        validate_short_name(opt.args[0]);

    // ==================================================================================
    // 4. Default Value Construction (FIXED LOGIC)
    // ==================================================================================
    Runtime_value default_rt_val;
    std::string_view default_str_hint;

    // Helper to fill vector/array from a container or single value
    auto make_vector_storage = [&](const auto &source)
    {
        std::vector<CanonElem> vec;
        if constexpr (is_std_array_v<T>)
        {
            vec.reserve(std::tuple_size_v<T>);
            for (const auto &item : source) vec.push_back(static_cast<CanonElem>(item));
        }
        else if constexpr (is_std_vector_v<T>)
        {
            for (const auto &item : source) vec.push_back(static_cast<CanonElem>(item));
        }
        else
        {
            // Multi-scalar
            vec.push_back(static_cast<CanonElem>(source));
        }
        return vec;
    };

    if (opt.flags & *Flags::Default)
    {
        default_str_hint = this->_schema->arena_.str(std::format("{}", opt.default_val));

        if (storage_kind == Storage_kind::Scalar)
        {
            if constexpr (!is_std_array_v<T> && !is_std_vector_v<T>)
                default_rt_val = static_cast<CanonElem>(opt.default_val);
            else
                throw std::logic_error("Internal Error: Storage kind Scalar mismatch with Container Type.");
        }
        else
        {
            // Vector or Array storage
            default_rt_val = make_vector_storage(opt.default_val);
        }
    }
    else
    {
        // Initialize Empty
        if (storage_kind == Storage_kind::Scalar)
            default_rt_val = CanonElem{};
        else if (storage_kind == Storage_kind::Array)
            default_rt_val = std::vector<CanonElem>(arity, CanonElem{});
        else
            default_rt_val = std::vector<CanonElem>{};
    }

    // ==================================================================================
    // 5. Validator Construction
    // ==================================================================================
    std::vector<std::string_view> v_helps;
    v_helps.reserve(opt.validators_.size());
    for (const auto &v : opt.validators_) v_helps.push_back(this->_schema->arena_.str(v.help));

    std::function<std::expected<void, std::string>(const Runtime_value &)> val_fn;

    if (opt.validators_.empty())
    {
        val_fn = [](const auto &) -> std::expected<void, std::string> { return {}; };
    }
    else
    {
        val_fn = [entries = opt.validators_, storage_kind](const Runtime_value &rv) -> std::expected<void, std::string>
        {
            auto check_instance = [&](const T &instance) -> std::expected<void, std::string>
            {
                for (const auto &e : entries)
                    if (auto r = e.func(instance); !r)
                        return r;
                return {};
            };

            if (storage_kind == Storage_kind::Vector || storage_kind == Storage_kind::Array)
            {
                const auto &raw_vec = std::get<std::vector<CanonElem>>(rv);

                if constexpr (is_std_vector_v<T> || is_std_array_v<T>)
                {
                    T container;
                    using ValType = typename T::value_type;

                    if constexpr (is_std_vector_v<T>)
                    {
                        container.reserve(raw_vec.size());
                        for (const auto &e : raw_vec) container.push_back(static_cast<ValType>(e));
                    }
                    else
                    {
                        for (size_t i = 0; i < raw_vec.size() && i < std::tuple_size_v<T>; ++i)
                            container[i] = static_cast<ValType>(raw_vec[i]);
                    }
                    return check_instance(container);
                }
                else
                {
                    for (size_t i = 0; i < raw_vec.size(); ++i)
                        if (auto r = check_instance(static_cast<T>(raw_vec[i])); !r)
                            return std::unexpected(std::format("Item {}: {}", i, r.error()));
                    return {};
                }
            }
            else
            {
                if constexpr (!is_std_array_v<T>)
                    return check_instance(static_cast<T>(std::get<CanonElem>(rv)));
                return {};
            }
        };
    }

    // ==================================================================================
    // 6. Object Registration (FIXED ORDER)
    // ==================================================================================

    detail::Option *o = this->_schema->arena_.make<detail::Option>(detail::Option{
        .id = id,
        .names = {this->_schema->arena_.str(opt.args[0]), this->_schema->arena_.str(opt.args[1])},
        .desc = this->_schema->arena_.str(opt.desc),
        .type = target_enum,
        .storage = storage_kind,
        .flags = opt.flags,
        .list_cfg = opt.list_cfg,
        .multi_cfg = opt.multi_cfg,
        .arity = arity,
        .meta = this->_schema->arena_.str(opt.meta_),
        .env = this->_schema->arena_.str(opt.env_),
        .validator_helps = v_helps,
        .default_hints = default_str_hint,
        .default_value = default_rt_val,
        .sub_id = opt.sub_cmd_id,
        .validate = val_fn
    });

    Runtime rt{};
    rt.runtime_value = default_rt_val;

    this->_schema->options_.push_back(o);
    _runtime.push_back(rt);

    // Register to subcommand
    if (opt.sub_cmd_id >= this->_schema->sub_cmds_.size())
        throw std::invalid_argument(std::format("Invalid subcommand id: {}.", opt.sub_cmd_id));

    Subcommand* sub_command = this->_schema->sub_cmds_[opt.sub_cmd_id];

    sub_command->child_options.push_back(id);
    sub_command->long_arg_to_id_.emplace(o->names[1], id);
    sub_command->short_arg_to_id_.emplace(o->names[0], id);

    return id;
}

// -----------------------------------------------------------------------------
// IMPLEMENTATION: PARSER::PARSE
// -----------------------------------------------------------------------------
// TODO: Make subcommand context switch smarter when going from local to global when they share an option and it appears.
//       E.g: ./main device wifi --add "wifi" --add "global"
//       Here first add must be local inside device and wifi and second to nearest scope containing it, if it is not multi
auto cl::Parser::parse(int argc, char *argv[]) -> std::expected<Parse_res, Parse_err>
{
    detail::Arg_stream args(argc, argv);
    Parse_err err{};
    Parse_res res{};
    cl::detail::Parse_ctx ctx(args, err, res, this->cfg_);
    res._schema  = this->_schema;
    res._runtime = std::make_shared<std::vector<Runtime>>(this->_runtime);
    res._visited_subcommands.insert(global_command);
    ctx._active_subcomamnd = this->_schema->sub_cmds_[global_command];

    while (!args.empty())
    {
        std::string_view tok = args.pop();

        if (tok.starts_with("--"))
            this->handle_long_token(ctx, tok.substr(2));
        else if (tok.starts_with("-"))
            this->handle_short_token(ctx, tok.substr(1));
        else
            this->handle_positional_and_subcmds(ctx, tok);
    }

    if (!err.errors.empty())
        return std::unexpected(err);

    for (size_t i = 0; i < this->_schema->options_.size(); ++i)
    {
        detail::Option *opt = this->_schema->options_[i];
        Runtime &rt = (*res._runtime)[i];
        // 1. Try Environment Variable (If not parsed from CLI)
        if (!rt.parsed && (opt->flags & *Flags::Env) && !opt->env.empty())
        {
            if (const char *env_val = std::getenv(opt->env.data()))
            {
                // We treat Env var as a single string input.
                // If it's an array/multi, we assume it might be comma-delimited.
                // Reusing the injection logic requires a tiny bit of setup:
                std::string_view sv{env_val};
                std::vector<std::string_view> env_inputs;
                env_inputs.push_back(sv);
                ctx._curr_key = opt->env;
                this->inject_value(ctx, opt, env_inputs);
            }
        }

        // 2. Apply Default (If still not parsed)
        if (!rt.parsed && (opt->flags & *Flags::Default))
        {
            rt.runtime_value = opt->default_value;
            rt.parsed = true;
            // Note: Defaults are trusted, we assume they are valid types.
        }

        // 3. Check Required (Final Check)
        if (!rt.parsed && (opt->flags & *Flags::Required))
        {
            // NEW: Only enforce requirement if the option is Global OR belongs to the Active Subcommand
            if (opt->sub_id == global_command || opt->sub_id == ctx._active_sub_id)
                err.push_err(opt->names[0], "Required option is missing.");
        }
        if (rt.parsed)
            continue;

        if ((opt->flags & (*Flags::Default)))
            rt.runtime_value = opt->default_value;
    }

    if (!err.errors.empty())
        return std::unexpected(err);
    else
        return res;
}

template <typename Dest>
requires cl::Supported_Scalar_C<Dest>
inline auto cl::Parser::string_to_value(std::string_view s) -> std::expected<Dest, std::string>
{
    if constexpr (std::is_same_v<Dest, std::string> || std::is_same_v<Dest, std::string_view>)
    {
        return std::string(s);
    }
    else if constexpr (std::is_same_v<Dest, bool>)
    {
        for (auto &t : truthy_strs)
            if (s == t)
                return true;
        for (auto &f : falsy_strs)
            if (s == f)
                return false;

        return std::unexpected(std::format("Invalid boolean value: '{}'", s));
    }
    else if constexpr (std::is_integral_v<Dest>)
    {
        Dest v{};
        auto [p, ec] = std::from_chars(s.data(), s.data() + s.size(), v);
        if (ec != std::errc{})
            return std::unexpected(std::format("Invalid integer: '{}'", s));
        return v;
    }
    else if constexpr (std::is_floating_point_v<Dest>)
    {
        Dest v{};
        auto [p, ec] = std::from_chars(s.data(), s.data() + s.size(), v);
        if (ec != std::errc{})
            return std::unexpected(std::format("Invalid float: '{}'", s));
        return v;
    }
}

void cl::Parser::handle_long_token(cl::detail::Parse_ctx &ctx, std::string_view body)
{
    std::string_view key = body;
    std::optional<std::string_view> explicit_val = std::nullopt;
    ctx._curr_key = body;

    if (size_t eq_pos = body.find('='); eq_pos != std::string_view::npos)
    {
        if (this->cfg_.value_binding == Binding_type::Next)
            return ctx.error("Equals '=' binding is disabled by configuration.");
        key = body.substr(0, eq_pos);
        ctx._curr_key = key;
        explicit_val = body.substr(eq_pos + 1);
    }

    auto it = ctx._active_subcomamnd->long_arg_to_id_.find(key);
    if (it == ctx._active_subcomamnd->long_arg_to_id_.end())
        return ctx.error("Unknown option");

    detail::Option *opt = this->_schema->options_[it->second];

    if (opt->sub_id != global_command && opt->sub_id != ctx._active_sub_id)
        return ctx.error("Option '--{}' is not valid in the current context.", key);

    acquire_value(ctx, opt, explicit_val);
}

void cl::Parser::handle_short_token(cl::detail::Parse_ctx &ctx, std::string_view body)
{
    if (body.empty())
        return;
    std::string_view key = body.substr(0, 1);
    ctx._curr_key = key;

    auto it = ctx._active_subcomamnd->short_arg_to_id_.find(key);
    if (it == ctx._active_subcomamnd->short_arg_to_id_.end())
        return ctx.error("Unknown flag -{}", key);

    detail::Option *opt = this->_schema->options_[it->second];

    if (opt->sub_id != global_command && opt->sub_id != ctx._active_sub_id)
        return ctx.error("Flag '-{}' is not valid in the current context.", key);

    if (opt->arity == 0)
    {
        this->assign_true(ctx._res._runtime->at(opt->id));
        // If characters remain, handle them recursively
        if (body.size() > 1)
        {
            if (!ctx._cfg.allow_combined_short_flags)
                return ctx.error("Combined short flags are disabled.");

            this->add_short_combined(ctx, body.substr(1));
        }
        return;
    }

    std::optional<std::string_view> attached_val = std::nullopt;

    if (body.size() > 1)
    {
        // Check if user is trying to attach a value
        if (!ctx._cfg.allow_short_value_concat)
            return ctx.error("Concatenated values disabled.");

        // Handle edge case: -j=4 vs -j4
        // Only strip '=' if the binding config allows it (Equal or Both)
        if (body[1] == '=' && ctx._cfg.value_binding != Binding_type::Next)
            attached_val = body.substr(2);  // Skip flag and '='
        else
            attached_val = body.substr(1);  // Skip just the flag, rest is value
    }

    // Delegate to value acquisition (handles parsing/storage)
    acquire_value(ctx, opt, attached_val);
}

bool cl::Parser::add_short_combined(cl::detail::Parse_ctx &ctx, std::string_view body)
{
    for (auto x : body)
    {
        std::string_view curr_key{&x, 1};
        auto it = ctx._active_subcomamnd->short_arg_to_id_.find(curr_key);
        ctx._curr_key = curr_key;
        if (it == ctx._active_subcomamnd->short_arg_to_id_.end())
        {
            ctx.error("Unknown flag");
            return false;
        }
        detail::Option *curr_opt = this->_schema->options_[it->second];

        if (curr_opt->arity > 0)
        {
            ctx.error("airty greater than zero, therefore can't be concatenated");
            return false;
        }

        this->assign_true((*ctx._res._runtime)[curr_opt->id]);
    }
    return true;
}

bool cl::Parser::acquire_value(cl::detail::Parse_ctx &ctx, detail::Option *opt, std::optional<std::string_view> explicit_val)
{
    auto &rt = (*ctx._res._runtime)[opt->id];

    if (rt.parsed && !(opt->flags & *Flags::Multi) && (opt->arity == 1))
    {
        switch (this->cfg_.repeated_scalar)
        {
            case cl::Repeated_scalar_policy::REJECT:
            {
                return false;
            }
            break;
            case Repeated_scalar_policy::FIRST:
            {
                return true;
            }
            break;
            case cl::Repeated_scalar_policy::LAST:
                break;
        }
    }

    if (opt->arity == 0)
    {
        if (explicit_val)
        {
            ctx.error("Flag '{}' cannot accept a value.", opt->names[0]);
            return false;
        }
        this->assign_true(rt);
        return true;
    }

    std::vector<std::string_view> raw_inputs{};
    if (explicit_val)
    {
        if ((opt->arity > 1) && (opt->list_cfg.type == List_type::Consecutive))
        {
            ctx.error("Consecutive arrays cannot use attached values.");
            return false;
        }
        // List type can only be Delimited now.
        // Split the input
        if (opt->arity > 1 || ((opt->flags & (*Flags::Multi)) && (opt->multi_cfg.type == Multi_type::Delimited)))
        {
            std::string_view delim = (opt->arity > 1) ? opt->list_cfg.delimiter : opt->multi_cfg.delimiter;

            for (auto &&part : std::views::split(*explicit_val, delim)) raw_inputs.emplace_back(std::string_view(part));
        }
        else
        {
            // Not array, not multi delimited
            // Only options: Scalar/Multi repeat
            raw_inputs.push_back(*explicit_val);
        }
    }
    else
    {
        // Fetch values from a stream.
        if (this->cfg_.value_binding != Binding_type::Next && this->cfg_.value_binding != Binding_type::Both)
        {
            ctx.error("No value for option, use '<key>='<value>'' to bind value");
            return false;
        }

        if (opt->arity > 1 && opt->list_cfg.type == List_type::Consecutive)
        {
            // Fetch N items
            for (size_t i{}; i < opt->arity; ++i)
            {
                if (ctx._args.empty())
                {
                    ctx.error("Not enough arguments for array.");
                    return false;
                }
                raw_inputs.push_back(ctx._args.pop());
            }
        }
        else
        {
            // Fetch 1 item (might be delimited string)
            if (ctx._args.empty())
            {
                ctx.error("Value not provided.");
                return false;
            }
            std::string_view val = ctx._args.pop();

            // Check for delimiter splitting again
            bool split_needed = (opt->arity > 1 && opt->list_cfg.type == List_type::Delimited) ||
                                ((opt->flags & *Flags::Multi) && opt->multi_cfg.type == Multi_type::Delimited);

            if (split_needed)
            {
                std::string_view delim = (opt->arity > 1) ? opt->list_cfg.delimiter : opt->multi_cfg.delimiter;
                for (auto &&part : std::views::split(val, delim)) raw_inputs.emplace_back(std::string_view(part));
            }
            else
            {
                // Either scalar or multi repeat
                raw_inputs.push_back(val);
            }
        }
    }
    this->inject_value(ctx, opt, raw_inputs);
    return true;
}

void cl::Parser::inject_value(cl::detail::Parse_ctx &ctx, detail::Option *opt, std::span<const std::string_view> raw_values)
{
    auto &rt = (*ctx._res._runtime)[opt->id];

    // Ensure error reporting knows context (optional, but safe)
    ctx._curr_key = opt->names[0];
    bool result = false;

    std::visit(
        [&](auto &&storage)
        {
            using StorageT = std::decay_t<decltype(storage)>;

            // 1. Skip Monostate
            if constexpr (std::is_same_v<StorageT, std::monostate>)
                return;
            else
            {
                // 2. Safe Type Extraction
                using ElemT = typename cl::detail::get_inner_type<StorageT>::type;

                // 3. Process Values
                for (size_t i = 0; i < raw_values.size(); ++i)
                {
                    // Convert string to value
                    auto val_res = this->string_to_value<ElemT>(raw_values[i]);

                    if (!val_res)
                    {
                        ctx.error("{}", val_res.error());
                        return;
                    }

                    // 4. Storage Logic (Guarded by if constexpr)
                    if constexpr (is_std_vector_v<StorageT>)
                    {
                        // VECTOR / ARRAY LOGIC
                        bool is_fixed_array = (opt->storage == Storage_kind::Array);

                        if (is_fixed_array)
                        {
                            // Validate Array Size safely here
                            if (i >= storage.size())
                            {
                                if (!ctx._cfg.allow_empty_arrays)
                                    ctx.error("Internal Error: Target array size {} too small for input index {}.", storage.size(), i);
                                return;
                            }
                            storage[i] = *val_res;
                        }
                        else
                        {
                            storage.push_back(*val_res);
                            result = true;
                        }
                    }
                    else
                    {
                        // SCALAR LOGIC (int, bool, etc.)
                        storage = *val_res;
                        result = true;
                    }
                }
            }
        },
        rt.runtime_value);

    if (result)
    {
        if (auto valid = opt->validate(rt.runtime_value); !valid)
        {
            ctx.error("Validation failed: {}", valid.error());
            return;
        }
    }

    rt.parsed = true;
    rt.count++;
}

void cl::Parser::assign_true(Runtime &rt)
{
    rt.runtime_value = true;
    rt.count++;
    rt.parsed = true;
}

template <typename T, typename... Configs>
requires((cl::Configurer<Configs, T> && ...) && cl::Supported_Scalar_C<T>)
auto cl::Parser::positional(Single_name_cfg name, Configs &&...confs) -> Opt_id
{
    static_assert(!is_std_vector_v<T> && !is_std_array_v<T>, "Positionals restricted to Scalars only (int, float, string, bool).");

    Opt<T> opt;
    opt.args[1] = name.long_name;
    opt.loc = name.loc;
    (confs(opt), ...);

    return this->add_positional_impl(opt);
}

template <typename T>
requires cl::Supported_Scalar_C<T>
inline auto cl::Parser::add_positional_impl(Opt<T> opt) -> Opt_id
{
    auto id = assign_id();

    // 1. Basic Setup (Scalar Only)
    constexpr Opt_type target_enum = parsable_to_opt_type<T>();
    using CanonElem = typename _opt_type_to_canonical_type_t_<target_enum>::value;

    // Safety check against configurers trying to sneak in Multi/Array
    if ((opt.flags & *Flags::Multi) || (opt.list_cfg.type != List_type::Consecutive))
        throw std::invalid_argument("Positionals must be scalar (no Multi/Array flags allowed).");

    // 2. Prepare Storage
    Runtime_value val = CanonElem{};

    // 3. Create Option
    std::vector<std::string_view> v_helps;
    for (const auto &v : opt.validators_) v_helps.push_back(this->_schema->arena_.str(v.help));

    detail::Option *o = this->_schema->arena_.make<detail::Option>(detail::Option{.id = id,
        .names = {"", this->_schema->arena_.str(opt.args[1])},  // [1] is Display Name, [0] empty
        .desc = this->_schema->arena_.str(opt.desc),
        .type = target_enum,
        .storage = Storage_kind::Scalar,  // Forced Scalar
        .flags = opt.flags,
        .list_cfg = opt.list_cfg,
        .multi_cfg = opt.multi_cfg,
        .arity = 1,  // Forced Arity 1
        .meta = this->_schema->arena_.str(opt.meta_),
        .env = this->_schema->arena_.str(opt.env_),
        .validator_helps = v_helps,
        .default_value = val});

    // 4. Default Values
    if (opt.flags & (*Flags::Default))
    {
        o->default_value = opt.default_val;
        o->default_hints = this->_schema->arena_.str(std::format("{}", opt.default_val));
    }

    // 5. Validator
    if (!opt.validators_.empty())
    {
        o->validate = [entries = opt.validators_](const Runtime_value &rv) -> std::expected<void, std::string>
        {
            auto check_one = [&](const T &val) -> std::expected<void, std::string>
            {
                for (const auto &e : entries)
                    if (auto r = e.func(val); !r)
                        return r;
                return {};
            };
            // It's always scalar per your restriction
            const auto &canon_val = std::get<CanonElem>(rv);
            return check_one(static_cast<T>(canon_val));
        };
    }
    else
    {
        o->validate = [](const auto &) -> std::expected<void, std::string> { return {}; };
    }

    // 6. Register
    this->_schema->options_.push_back(o);
    _runtime.push_back(Runtime{.runtime_value = o->default_value});

    // IMPORTANT: Track this as a positional
    this->_positional_ids.push_back(id);

    return id;
}

inline void cl::Parser::handle_positional_and_subcmds(cl::detail::Parse_ctx &ctx, std::string_view value)
{
    if (auto it = ctx._active_subcomamnd->child_to_id.find(value); it != ctx._active_subcomamnd->child_to_id.end())
    {
        ctx._active_sub_id = it->second;
        ctx._active_subcomamnd = this->_schema->sub_cmds_[ctx._active_sub_id];
        ctx._res._visited_subcommands.insert(ctx._active_sub_id);
        return;
    }
    else if (auto it = this->_schema->sub_cmds_[global_command]->child_to_id.find(value); it != ctx._active_subcomamnd->child_to_id.end())
    {
        ctx._active_sub_id = it->second;
        ctx._active_subcomamnd = this->_schema->sub_cmds_[ctx._active_sub_id];
        ctx._res._visited_subcommands.insert(ctx._active_sub_id);
        return;
    }
    // Check if we have any positionals left to fill for the current command
    if (ctx._positional_ind >= _positional_ids.size())
    {
        ctx.error("Unexpected positional argument: '{}'", value);
        return;
    }

    Opt_id id = _positional_ids[ctx._positional_ind];
    detail::Option *opt = this->_schema->options_[id];

    // Inject the value
    // We treat it as a single element span
    std::string_view arr_input[] = {value};
    this->inject_value(ctx, opt, std::span(arr_input));

    // Since it's strictly scalar, we ALWAYS move to the next positional
    ctx._positional_ind++;
}


inline auto cl::Parser::add_sub_cmd(const std::string &name, const std::string &desc, uint16_t flags, Subcommand_id parent_id, int reserve) -> Subcommand_id
{
    // 1. Validation & Parent Lookup
    Subcommand_id id = static_cast<Subcommand_id>(this->_schema->sub_cmds_.size());
    bool is_root = this->_schema->sub_cmds_.empty();

    if (!is_root)
    {
        if (parent_id >= this->_schema->sub_cmds_.size())
            throw std::invalid_argument(std::format("Attempted to add subcommand '{}' to invalid parent ID: {}", name, parent_id));

        Subcommand *parent = this->_schema->sub_cmds_[parent_id];
        if (parent->child_to_id.contains(name))
            throw std::invalid_argument(std::format("Duplicate subcommand name: '{}' in context '{}'", name, parent->name));
    }

    cl::asrt::t(!name.empty(), "Subcommand name cannot be empty.");

    // 2. Allocation
    Subcommand *g = this->_schema->arena_.make<Subcommand>(Subcommand{
        .id = id,
        .name = this->_schema->arena_.str(name),
        .description = this->_schema->arena_.str(desc),
        .flags = flags,
        .child_options = {},
        .child_subcommands = {},
        .parent_id = is_root ? id : parent_id,
        .child_to_id = {},
        .long_arg_to_id_ = {},
        .short_arg_to_id_ = {}
    });

    g->long_arg_to_id_.reserve(reserve);
    g->short_arg_to_id_.reserve(reserve);
    g->child_options.reserve(reserve);
    g->child_subcommands.reserve(reserve);

    // 3. Registration
    this->_schema->sub_cmds_.push_back(g);

    if (!is_root)
    {
        // Add to parent's child map using the persistent arena string view
        this->_schema->sub_cmds_[parent_id]->child_to_id.emplace(g->name, id);
        this->_schema->sub_cmds_[parent_id]->child_subcommands.push_back(id);
    }

    return id;
}


template <typename... Args>
void cl::detail::Parse_ctx::error(std::format_string<Args...> fmt, Args &&...a)
{
    _err.push_err(this->_curr_key, fmt, std::forward<Args>(a)...);

    while (!_args.empty())
    {
        auto t = _args.peek();
        // Stop if we see something that looks like a flag
        if (t)
        {
            if (t->starts_with("--"))
                return;
            if (t->starts_with("-") && t->size() == 2 && std::isalpha((*t)[1]))
                return;
        }
        _args.pop();
    }
}

auto cl::Parser::print_help(std::ostream &os, Subcommand_id sub_id, std::optional<Opt_id> opt_id) -> void
{
    // 1. Validate
    if (sub_id >= this->_schema->sub_cmds_.size())
        return;
    Subcommand *sub = this->_schema->sub_cmds_[sub_id];

    // 2. Option Drill-Down (Detailed Technical View)
    if (opt_id.has_value())
    {
        Opt_id oid = *opt_id;
        if (oid >= this->_schema->options_.size())
            return;
        detail::Option *opt = this->_schema->options_[oid];

        os << "\nOPTION: " << detail::format_option_flags(opt) << "\n";
        os << std::string(60, '-') << "\n";
        os << " " << (opt->desc.empty() ? "No description." : opt->desc) << "\n\n";
        os << " Type:    " << opt->type << "\n";
        os << " Arity:   " << opt->arity << "\n";
        if (opt->flags & *Flags::Required)
            os << " Status:  Required\n";
        if (opt->flags & *Flags::Env)
            os << " Env:     " << opt->env << "\n";
        if (opt->flags & *Flags::Default)
            os << " Default: " << opt->default_hints << "\n";
        if (!opt->validator_helps.empty())
        {
            os << " Checks:\n";
            for (auto &h : opt->validator_helps) os << "   - " << h << "\n";
        }
        os << "\n";
        return;
    }

    // 3. Layout Utilities
    std::vector<detail::Help_entry> rows;
    size_t max_left = 0;

    auto add_row = [&](std::string l, std::string r)
    {
        if (l.size() > max_left)
            max_left = l.size();
        rows.push_back({l, r, false});
    };
    auto add_header = [&](std::string t) { rows.push_back({t, "", true}); };

    // --- USAGE ---
    // Build path: main -> device -> list
    std::vector<std::string_view> path;
    Subcommand *curr = sub;
    while (true)
    {
        if (curr->id != global_command)
            path.push_back(curr->name);
        if (curr->id == curr->parent_id)
            break;
        curr = this->_schema->sub_cmds_[curr->parent_id];
    }

    os << "USAGE: " << this->_name;
    for (auto it = path.rbegin(); it != path.rend(); ++it) os << " " << *it;

    // Logic: If children exist -> [COMMAND]. Always show [OPTIONS].
    if (!sub->child_subcommands.empty())
        os << " [COMMAND]";
    os << " [OPTIONS]\n";

    // --- DESCRIPTION ---
    // Fix: Don't print description if it's the root (Global Context)
    if (sub_id != global_command && !sub->description.empty())
        os << "\n" << sub->description << "\n";

    // --- COMMANDS ---
    if (!sub->child_subcommands.empty())
    {
        add_header("\nCOMMANDS:");

        // Simple recursion for indentation
        std::function<void(Subcommand_id, int)> print_subs;
        print_subs = [&](Subcommand_id sid, int depth)
        {
            Subcommand *s = this->_schema->sub_cmds_[sid];
            // 2 spaces per level
            std::string indent(depth * 2, ' ');
            add_row(indent + std::string(s->name), std::string(s->description));

            // Recurse
            for (auto child_id : s->child_subcommands) print_subs(child_id, depth + 1);
        };

        for (auto child_id : sub->child_subcommands) print_subs(child_id, 0);
    }

    // --- ARGUMENTS (Positionals) ---
    bool has_pos = false;
    for (auto pid : this->_positional_ids)
    {
        bool belongs = false;
        for (auto co : sub->child_options)
            if (co == pid)
            {
                belongs = true;
                break;
            }

        if (belongs)
        {
            if (!has_pos)
            {
                add_header("\nARGUMENTS:");
                has_pos = true;
            }
            detail::Option *p = this->_schema->options_[pid];
            add_row(std::format("<{}>", p->names[1]), std::string(p->desc));
        }
    }

    // --- OPTIONS ---
    // Only show options explicitly attached to THIS subcommand ID.
    bool has_opts = false;
    for (auto oid : sub->child_options)
    {
        // Skip positionals
        bool is_pos = false;
        for (auto pid : this->_positional_ids)
            if (pid == oid)
                is_pos = true;
        if (is_pos)
            continue;

        detail::Option *opt = this->_schema->options_[oid];
        if (opt->flags & *Flags::Hidden)
            continue;

        if (!has_opts)
        {
            add_header("\nOPTIONS:");
            has_opts = true;
        }

        std::string right = std::string(opt->desc);

        if (opt->flags & *Flags::Required)
            right += " [Required]";
        if (opt->flags & *Flags::Env)
            right += " [Env: " + std::string(opt->env) + "]";
        if ((opt->flags & *Flags::Default) && !opt->default_hints.empty())
            right += " [Def: " + std::string(opt->default_hints) + "]";

        add_row(detail::format_option_flags(opt), right);
    }

    // --- RENDER ---
    size_t pad = max_left + 4;
    for (const auto &r : rows)
        if (r.is_header)
            os << r.left << "\n";
        else
            os << "  " << std::left << std::setw(pad) << r.left << r.right << "\n";
    os << "\n";
}

auto std::formatter<cl::Opt_type>::format(cl::Opt_type t, format_context &ctx) const
{
    std::string_view name = "unknown";
    switch (t)
    {
        case cl::Opt_type::Int:
            name = "Int";
            break;
        case cl::Opt_type::Bool:
            name = "Bool";
            break;
        case cl::Opt_type::Str:
            name = "Str";
            break;
        case cl::Opt_type::Float:
            name = "Float";
            break;
    }
    return std::formatter<std::string_view>::format(name, ctx);
}

auto std::formatter<cl::Storage_kind>::format(cl::Storage_kind k, format_context &ctx) const
{
    std::string_view name = "unknown";
    switch (k)
    {
        case cl::Storage_kind::Scalar:
            name = "Scalar";
            break;
        case cl::Storage_kind::Array:
            name = "Array";
            break;
        case cl::Storage_kind::Vector:
            name = "Vector";
            break;
    }
    return std::formatter<std::string_view>::format(name, ctx);
}


constexpr auto std::formatter<cl::detail::Option, char>::parse(std::format_parse_context &ctx) { return ctx.begin(); }

auto std::formatter<cl::detail::Option, char>::format(const cl::detail::Option &o, std::format_context &ctx) const
{
    auto out = ctx.out();
    std::format_to(out, "Option{{ id={}, type={}, storage={}, arity={}, flags=0b{:b}, default_hints: {}}}", o.id, o.type, o.storage,
                   o.arity, o.flags, o.default_hints);
    return out;
}


constexpr auto std::formatter<cl::Parse_err>::parse(std::format_parse_context &ctx) { return ctx.begin(); }

template <typename FormatContext>
auto std::formatter<cl::Parse_err>::format(const cl::Parse_err &pe, FormatContext &ctx) const
{
    auto out = ctx.out();

    for (std::size_t i = 0; i < pe.errors.size(); ++i)
    {
        const auto &e = pe.errors[i];

        out = std::format_to(out, "error: {}", e.message);

        if (!e.option.empty())
            out = std::format_to(out, "\n      option: {}", e.option);

        if (i + 1 < pe.errors.size())
            out = std::format_to(out, "\n");
    }

    return out;
}

template <typename T>
requires cl::Gettable_Type_C<T>
[[nodiscard]]
auto cl::Parse_res::get(cl::Opt_id id) const -> std::optional<T>
{
    if (id >= _runtime->size())
        return std::nullopt;

    if (!(*_runtime)[id].parsed) return std::nullopt;

    const auto &val_variant = (*_runtime)[id].runtime_value;

    if constexpr (is_std_array_v<T>)
    {
        using InnerT = typename T::value_type;
        const auto &vec = std::get<std::vector<InnerT>>(val_variant);
        T arr_result;
        std::copy(vec.begin(), vec.end(), arr_result.begin());
        return arr_result;
    }
    else
    {
        try {
            return std::get<T>(val_variant);
        } catch(...) {
            throw;
        }
    }
}


template <typename T>
requires cl::Gettable_Type_C<T>
[[nodiscard]]
auto cl::Parse_res::get(Opt_id id, T &val) const -> std::expected<void, std::string>
{
    if (id >= _runtime->size())
        return std::unexpected(std::format("[CL Error] Invalid Option ID: {}", id));

    const auto &rt = (*_runtime)[id];
    if (!rt.parsed) return std::unexpected(std::format("Neither was value set now default value provided for: {}", id));

    const auto* info = this->_schema->options_.at(id);

    bool is_vector = (info->storage == Storage_kind::Vector);
    if (!rt.parsed && !is_vector)
        return std::unexpected("Option not set");

    const auto &val_variant = rt.runtime_value;
    if constexpr (is_std_array_v<std::decay_t<T>>)
    {
        using InnerT = typename T::value_type;
        if (!std::holds_alternative<std::vector<InnerT>>(val_variant))
            return std::unexpected("Type Mismatch");
        const auto &vec = std::get<std::vector<InnerT>>(val_variant);
        if (vec.size() != std::tuple_size_v<T>)
            return std::unexpected("Array size mismatch");
        T arr_result;
        std::copy(vec.begin(), vec.end(), arr_result.begin());
        val = std::move(arr_result);
        return {};
    }
    else
    {
        try
        {
            val = std::get<std::decay_t<T>>(val_variant);
            return {};
        }
        catch (const std::bad_variant_access &)
        {
            return std::unexpected("Type Mismatch");
        }
    }
}


template <typename T>
requires cl::Gettable_Type_C<T>
[[nodiscard]]
auto cl::Parse_res::get(std::string_view key) const -> std::optional<T>
{
    if (auto id = this->get_id_in_curr_scope(key); id)
    {
        try {
            return this->get<T>(*id);
        } catch(...) {
            throw;
        }
    }
    return std::nullopt;
}

auto cl::Parse_res::get_subcmd(Subcommand_id id) const -> std::optional<Parse_res>
{
    if (id >= this->_schema->sub_cmds_.size()) return std::nullopt;
    if (!this->_visited_subcommands.contains(id)) return std::nullopt;
    Parse_res out = *this;
    out.curr_subcmd = id;
    return out;
}

auto cl::Parse_res::get_subcmd(std::string_view key) const -> std::optional<Parse_res>
{
    auto& _map = this->_schema->sub_cmds_[curr_subcmd]->child_to_id;
    if (auto it = _map.find(key); it == _map.end()) return std::nullopt;
    else return this->get_subcmd(it->second);
}


template <typename T, typename... Args>
requires cl::Gettable_Type_C<T> && (sizeof...(Args) > 0)
[[nodiscard]]
auto cl::Parse_res::get(std::string_view subcmd_name, Args &&...rest) const -> std::optional<T>
{
    // 1. Try to find the subcommand in the current scope
    if (auto sub = this->get_subcmd(subcmd_name))
    {
        // 2. Recurse: Call get() on the new 'sub' view with the remaining arguments
        return sub->get<T>(std::forward<Args>(rest)...);
    }

    // Subcommand not found -> Path is invalid
    return std::nullopt;
}

template <typename T, typename... Args>
requires cl::Gettable_Type_C<T> && (sizeof...(Args) > 0)
[[nodiscard]]
auto cl::Parse_res::get(T* val, std::string_view subcmd_name, Args &&...rest) const -> std::expected<void, std::string>
{
    // 1. Try to find the subcommand in the current scope
    if (auto sub = this->get_subcmd(subcmd_name))
    {
        // 2. Recurse: Call get() on the new 'sub' view with the remaining arguments
        return sub->get<T>(val, std::forward<Args>(rest)...);
    }

    return std::unexpected(std::format("Subcommand: {} doesnt exist or wasn't set.", subcmd_name));
}

template <typename T>
requires cl::Gettable_Type_C<T>
[[nodiscard]]
auto cl::Parse_res::get(T *val, std::string_view key) const -> std::expected<void, std::string>
{
    if (!val)
        return std::unexpected("Output pointer is null");

    // 1. Find the ID in the current scope
    auto opt_id = this->get_id_in_curr_scope(key);

    if (!opt_id)
        return std::unexpected(std::format("Option '{}' not found in the current context.", key));

    // 2. Delegate to the existing ID-based getter (get(id, T&))
    // We dereference the pointer *val to pass it as a reference
    return this->get<T>(*opt_id, *val);
}


auto cl::Parse_res::is_seen(Opt_id id) -> bool
{
    if (id >= this->_runtime->size()) return false;
    if ((*this->_runtime)[id].parsed || (*this->_runtime)[id].count > 0) return true;
    return false;
}

auto cl::Parse_res::is_subcmd_chosen(Subcommand_id id) -> bool
{
    if (this->_visited_subcommands.contains(id)) return true;
    return false;
}

auto cl::Parse_res::is_seen(std::string_view key) -> bool
{
    if (auto id = this->get_id_in_curr_scope(key); id)
        return (((*this->_runtime)[*id].parsed) || ((*this->_runtime)[*id].count > 0));
    else
        return false;
}

auto cl::Parse_res::get_id_in_curr_scope(std::string_view key) const -> std::optional<Opt_id>
{
    if (key.length() == 1)
    {
        auto &_map = this->_schema->sub_cmds_[curr_subcmd]->short_arg_to_id_;
        if (auto it = _map.find(key); it != _map.end())
            return it->second;
        return std::nullopt;
    }

    auto &map = this->_schema->sub_cmds_[curr_subcmd]->long_arg_to_id_;
    if (auto it = map.find(key); it != map.end())
        return it->second;
    return std::nullopt;
}





inline std::ostream &operator<<(std::ostream &os, const cl::Parse_err &err)
{
    for (const auto &e : err.errors)
        os << "[Error] Option: " << (e.option.empty() ? "N/A" : e.option) << "\n" << "    | " << e.message << "\n";
    return os;
}

namespace cl::detail {

    [[nodiscard]] std::optional<std::string_view> Arg_stream::peek() const
    {
        if (empty())
            return std::nullopt;
        return args_[cur_];
    }

    std::string_view Arg_stream::pop()
    {
        if (empty())
            return {};
        return args_[cur_++];
    }

    std::size_t Arg_stream::size() { return args_.size() - cur_; }

    void Arg_stream::rewind()
    {
        if (cur_ > 1)
            cur_--;
    }
}

#endif  // !CL_IMPLEMENTATION

#endif  // !__CL_HPP_
