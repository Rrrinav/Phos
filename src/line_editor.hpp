#pragma once

#include <iosfwd>
#include <string>
#include <vector>

namespace phos {

/*
 * [Line_editor]
 * Minimal interactive line editing for the REPL: history navigation
 * (up/down), cursor movement (left/right, Home/End), backspace/Delete,
 * Ctrl-A/E (line edges), Ctrl-U (kill line), Ctrl-C (abort line) and
 * Ctrl-D (EOF at an empty line). Falls back to plain std::getline when
 * stdin is not a terminal, so piped sessions behave exactly like before.
 */
class Line_editor
{
public:
    struct Result
    {
        enum class Status { Line, Eof, Interrupt };
        Status status = Status::Line;
        std::string line;
    };

    Line_editor() = default;
    ~Line_editor();

    bool interactive() const
    {
        return interactive_;
    }

    void set_interactive(bool value)
    {
        interactive_ = value;
    }

    // Reads one line. `prompt` is printed at the start of the line; every
    // redraw rewrites it, so multi-line prompts are not supported.
    Result read(std::ostream &out, const std::string &prompt);

    void clear_history()
    {
        history_.clear();
    }

private:
    static constexpr size_t kHistoryLimit = 500;

    bool interactive_ = false;
    bool raw_enabled_ = false;

    std::vector<std::string> history_;

    // Editing state
    std::string buffer_;
    size_t cursor_ = 0;
    int history_index_ = -1; // -1 = editing a fresh line
    std::string pending_;    // the line being edited before history nav

#ifndef _WIN32
    bool read_char(char &out);
#endif
    int read_key();
    void enable_raw();
    void disable_raw();
    void redraw(std::ostream &out, const std::string &prompt) const;
    void insert_char(std::ostream &out, const std::string &prompt, char c);
    void backspace(std::ostream &out, const std::string &prompt);
    void move_cursor(std::ostream &out, const std::string &prompt, int delta);
    void jump_to(std::ostream &out, const std::string &prompt, int position);
    void navigate_history(std::ostream &out, const std::string &prompt, int delta);
    void commit_line(const std::string &line);
};

} // namespace phos
