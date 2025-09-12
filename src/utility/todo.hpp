#pragma once

#include <iostream>
#include <string>
#include <string_view>
#include <vector>
#include <format>
#include <source_location>
#include <ranges>
#include <cstdlib>

struct TodoEntry
{
    std::string message;
    std::string file;
    std::string function;
    int line;

    std::string format() const { return std::format("[{}:{} in {}]\n    {}\n", file, line, function, message); }
};

class TodoManager
{
    std::vector<TodoEntry> entries;

public:
    struct Proxy
    {
        TodoManager &manager;
        std::source_location loc;
        bool fatal;

        Proxy(TodoManager &m, bool f, std::source_location l = std::source_location::current()) : manager(m), loc(l), fatal(f) {}

        Proxy &operator<<(std::string_view msg)
        {
            TodoEntry entry{std::string(msg), loc.file_name(), loc.function_name(), static_cast<int>(loc.line())};
            manager.entries.push_back(entry);

            if (fatal)
            {
                std::cerr << "🚨 TODO FATAL!\n" << entry.format();
                std::abort();
            }

            return *this;
        }
    };

    Proxy operator()(bool fatal = false, std::source_location loc = std::source_location::current()) { return Proxy(*this, fatal, loc); }

    void report(std::ostream &os) const
    {
        if (entries.empty())
        {
            os << "✅ No TODOs found!\n";
            return;
        }
        os << "📋 TODO Report (" << entries.size() << " items)\n";
        os << "---------------------------------\n";
        for (auto const &e : entries | std::views::transform(&TodoEntry::format)) os << e;
    }
};

// Global manager
inline TodoManager __todo_global;
#define todo __todo_global(false)
#define todo_fatal __todo_global(true)
