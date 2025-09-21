#ifndef _ERR_HPP_
#define _ERR_HPP_

#include <string>
#include <format>

namespace phos {
namespace err {

enum Phase { Parsing, TypeChecking, Runtime };

struct msg
{
    std::string message;
    std::string phase;
    size_t line = 0;
    size_t column = 0;
    std::string filename = "";
    msg(): message(""), phase("") {}

    msg(std::string msg, std::string ph = "unknown", size_t l = 0, size_t c = 0, std::string file_ = "")
        : message(std::move(msg)), phase(std::move(ph)), line(l), column(c), filename(file_ )
    {
    }

    std::string format() const
    {
        if (line > 0)
            return std::format("{}:{}: error: in phase: {} : {}.", line, column, phase, message);
        else
            return std::format("error: in phase: {}: {}", phase, message);
    }
};

} // namespace err
} // namespace phos

#endif // _ERR_HPP_
