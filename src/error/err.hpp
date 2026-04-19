#ifndef _ERR_HPP_
#define _ERR_HPP_

#include <format>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace phos {
namespace err {

enum Phase { Parsing, TypeChecking, Runtime };
enum class Severity { Warning, Error };

struct detail
{
    std::string label;
    std::string value;
};

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

    std::string format() const
    {
        std::ostringstream out;

        if (!filename.empty() && line > 0) {
            out << filename << ":" << line << ":" << column << ": ";
        } else if (line > 0) {
            out << line << ":" << column << ": ";
        }

        out << "[" << severity_name(severity) << " in " << phase << "]: " << summary;

        for (const auto &entry : details) {
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

} // namespace err
} // namespace phos

#endif // _ERR_HPP_
