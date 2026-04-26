#ifndef _ERR_HPP_
#define _ERR_HPP_

#include <format>
#include <iostream>
#include <ostream>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace phos {
namespace err {

enum Phase { Parsing, TypeChecking, Runtime };

enum class Severity { Warning, Error };

struct render_options
{
    bool show_phase = true;
    bool show_severity = true;
};

struct detail
{
    std::string label;
    std::string value;
};

class renderer;

struct msg
{
    Severity severity = Severity::Error;
    std::string summary{""};
    std::string phase{""};
    size_t line = 0;
    size_t column = 0;
    std::string filename = "";
    std::vector<detail> details{};

    msg() : severity(Severity::Error), summary(""), phase("")
    {}

    msg(std::string msg, std::string ph = "unknown", size_t l = 0, size_t c = 0, std::string file_ = "", Severity sev = Severity::Error)
        : severity(sev), summary(std::move(msg)), phase(std::move(ph)), line(l), column(c), filename(std::move(file_))
    {}

    static std::string severity_name(Severity sev)
    {
        return sev == Severity::Warning ? "warning" : "error";
    }

    static msg make(
        Severity sev,
        std::string phase,
        size_t line,
        size_t column,
        std::string filename,
        std::string_view fmt,
        std::format_args args)
    {
        return msg(std::vformat(fmt, args), std::move(phase), line, column, std::move(filename), sev);
    }

    template <typename... Args>
    static msg error(std::string phase, size_t line, size_t column, std::string filename, std::format_string<Args...> fmt, Args &&...args)
    {
        return make(
            Severity::Error,
            std::move(phase),
            line,
            column,
            std::move(filename),
            fmt.get(),
            std::make_format_args(std::forward<Args>(args)...));
    }

    template <typename... Args>
    static msg warning(std::string phase, size_t line, size_t column, std::string filename, std::format_string<Args...> fmt, Args &&...args)
    {
        return make(
            Severity::Warning,
            std::move(phase),
            line,
            column,
            std::move(filename),
            fmt.get(),
            std::make_format_args(std::forward<Args>(args)...));
    }

    bool is_error() const
    {
        return severity == Severity::Error;
    }

    bool is_warning() const
    {
        return severity == Severity::Warning;
    }

    msg &add_detail(std::string label, std::string value)
    {
        details.push_back({std::move(label), std::move(value)});
        return *this;
    }

    msg &expected_got(std::string expected, std::string got)
    {
        add_detail("Expected", std::move(expected));
        add_detail("Got", std::move(got));
        return *this;
    }

    std::string format(render_options options = {}) const;
};

class renderer
{
public:
    static std::string format(const msg &diagnostic, render_options options = {})
    {
        std::ostringstream out;

        if (!diagnostic.filename.empty() && diagnostic.line > 0) {
            out << diagnostic.filename << ":" << diagnostic.line << ":" << diagnostic.column << ": ";
        } else if (diagnostic.line > 0) {
            out << diagnostic.line << ":" << diagnostic.column << ": ";
        }

        if (options.show_severity || options.show_phase) {
            out << "[";
            bool wrote = false;
            if (options.show_severity) {
                out << msg::severity_name(diagnostic.severity);
                wrote = true;
            }
            if (options.show_phase && !diagnostic.phase.empty()) {
                if (wrote) {
                    out << " in ";
                }
                out << diagnostic.phase;
            }
            out << "]: ";
        }

        out << diagnostic.summary;

        for (const auto &entry : diagnostic.details) {
            out << "\n  " << entry.label << ": ";

            std::istringstream value_stream(entry.value);
            std::string line_part;
            bool wrote_line = false;
            while (std::getline(value_stream, line_part)) {
                if (wrote_line) {
                    out << "\n    ";
                }
                out << line_part;
                wrote_line = true;
            }
            if (!wrote_line) {
                out << entry.value;
            }
        }

        return out.str();
    }

    static void print(std::ostream &out, const msg &diagnostic, render_options options = {})
    {
        out << format(diagnostic, options) << '\n';
    }
};

inline std::string msg::format(render_options options) const
{
    return renderer::format(*this, options);
}

class Engine
{
public:
    Engine() = default;
    explicit Engine(std::string phase) : default_phase_(std::move(phase))
    {}

    void set_phase(std::string phase)
    {
        default_phase_ = std::move(phase);
    }

    const std::string &phase() const
    {
        return default_phase_;
    }

    void push(msg diagnostic)
    {
        diagnostics_.push_back(std::move(diagnostic));
    }

    template <typename... Args>
    msg &error(size_t line, size_t column, std::string filename, std::format_string<Args...> fmt, Args &&...args)
    {
        diagnostics_.push_back(msg::error(default_phase_, line, column, std::move(filename), fmt, std::forward<Args>(args)...));
        return diagnostics_.back();
    }

    template <typename... Args>
    msg &warning(size_t line, size_t column, std::string filename, std::format_string<Args...> fmt, Args &&...args)
    {
        diagnostics_.push_back(msg::warning(default_phase_, line, column, std::move(filename), fmt, std::forward<Args>(args)...));
        return diagnostics_.back();
    }

    void append(const Engine &other)
    {
        diagnostics_.insert(diagnostics_.end(), other.diagnostics_.begin(), other.diagnostics_.end());
    }

    void append(std::vector<msg> other)
    {
        diagnostics_.insert(
            diagnostics_.end(),
            std::make_move_iterator(other.begin()),
            std::make_move_iterator(other.end()));
    }

    [[nodiscard]] bool empty() const
    {
        return diagnostics_.empty();
    }

    [[nodiscard]] bool has_errors() const
    {
        for (const auto &diagnostic : diagnostics_) {
            if (diagnostic.is_error()) {
                return true;
            }
        }
        return false;
    }

    [[nodiscard]] bool has_warnings() const
    {
        for (const auto &diagnostic : diagnostics_) {
            if (diagnostic.is_warning()) {
                return true;
            }
        }
        return false;
    }

    const std::vector<msg> &all() const
    {
        return diagnostics_;
    }

    std::vector<msg> take()
    {
        return std::move(diagnostics_);
    }

    void print(std::ostream &out = std::cerr, render_options options = {}) const
    {
        for (const auto &diagnostic : diagnostics_) {
            renderer::print(out, diagnostic, options);
        }
    }

private:
    std::string default_phase_;
    std::vector<msg> diagnostics_;
};

inline bool has_errors(const std::vector<msg> &messages)
{
    for (const auto &message : messages) {
        if (message.is_error()) {
            return true;
        }
    }
    return false;
}

inline bool has_errors(const Engine &engine)
{
    return engine.has_errors();
}

} // namespace err
} // namespace phos

#endif // _ERR_HPP_
