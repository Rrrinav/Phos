#include "line_editor.hpp"

#include <cstdio>
#include <iostream>

#ifdef _WIN32
#include <conio.h>
#include <windows.h>

#ifndef ENABLE_VIRTUAL_TERMINAL_PROCESSING
#define ENABLE_VIRTUAL_TERMINAL_PROCESSING 0x0004
#endif
#else
#include <termios.h>
#include <unistd.h>
#endif

namespace phos {

namespace {

// Key codes returned by read_key(); ordinary bytes come back as 0..255.
constexpr int kEof = -1;
constexpr int kKeyUp = -2;
constexpr int kKeyDown = -3;
constexpr int kKeyLeft = -4;
constexpr int kKeyRight = -5;
constexpr int kKeyHome = -6;
constexpr int kKeyEnd = -7;
constexpr int kKeyDelete = -8;

#ifdef _WIN32
DWORD g_original_mode = 0;
bool g_mode_saved = false;
#else
termios g_original_attrs;
bool g_attrs_saved = false;
#endif

} // namespace

Line_editor::~Line_editor()
{
    disable_raw();
}

#ifndef _WIN32
bool Line_editor::read_char(char &out)
{
    // std::cin.read reads exactly one byte because the tty is in raw mode.
    std::cin.read(&out, 1);
    return std::cin.good();
}
#endif

// Returns a byte (0..255), one of the kKey* constants, or kEof.
// - POSIX: decodes CSI sequences (ESC [ A/B/C/D/H/F/3~) into keys.
// - Windows: decodes the 0x00/0xE0 prefix + scan code form used by _getch.
int Line_editor::read_key()
{
#ifdef _WIN32
    int c = _getch();
    if (c == EOF) {
        return kEof;
    }
    if (c == 0x00 || c == 0xe0) {
        int scan = _getch();
        switch (scan) {
        case 0x48:
            return kKeyUp;
        case 0x50:
            return kKeyDown;
        case 0x4b:
            return kKeyLeft;
        case 0x4d:
            return kKeyRight;
        case 0x47:
            return kKeyHome;
        case 0x4f:
            return kKeyEnd;
        case 0x53:
            return kKeyDelete;
        default:
            return 0; // unknown extended key: drop it
        }
    }
    if (c == 0x1a) { // Ctrl-Z: Windows EOF convention
        return kEof;
    }
    return c;
#else
    char c = '\0';
    if (!read_char(c)) {
        return kEof;
    }
    if (c != 0x1b) {
        return static_cast<unsigned char>(c);
    }

    char seq = '\0';
    if (!read_char(seq) || seq != '[') {
        return 0x1b; // bare ESC: the caller drops control bytes
    }
    char code = '\0';
    if (!read_char(code)) {
        return kEof;
    }

    switch (code) {
    case 'A':
        return kKeyUp;
    case 'B':
        return kKeyDown;
    case 'C':
        return kKeyRight;
    case 'D':
        return kKeyLeft;
    case 'H':
        return kKeyHome;
    case 'F':
        return kKeyEnd;
    case '3': { // Delete
        char tilde = '\0';
        read_char(tilde);
        return kKeyDelete;
    }
    default:
        return 0x1b; // unknown sequence: drop it
    }
#endif
}

void Line_editor::enable_raw()
{
    if (!interactive_ || raw_enabled_) {
        return;
    }

#ifdef _WIN32
    HANDLE in = GetStdHandle(STD_INPUT_HANDLE);
    DWORD mode = 0;
    if (GetConsoleMode(in, &mode) == 0) {
        return;
    }
    if (!g_mode_saved) {
        g_original_mode = mode;
        g_mode_saved = true;
    }

    // Disable line buffering, echo, and Ctrl-C as a console event so control
    // keys arrive as plain bytes like on POSIX raw ttys.
    mode &= ~static_cast<DWORD>(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT | ENABLE_PROCESSED_INPUT);
    if (SetConsoleMode(in, mode) != 0) {
        raw_enabled_ = true;
    }

    // Redraws emit ANSI escapes; modern Windows consoles process them.
    HANDLE out = GetStdHandle(STD_OUTPUT_HANDLE);
    DWORD out_mode = 0;
    if (GetConsoleMode(out, &out_mode) != 0) {
        SetConsoleMode(out, out_mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
    }
#else
    termios attrs{};
    if (tcgetattr(STDIN_FILENO, &attrs) != 0) {
        return;
    }
    if (!g_attrs_saved) {
        g_original_attrs = attrs;
        g_attrs_saved = true;
    }

    attrs.c_lflag &= static_cast<tcflag_t>(~(ICANON | ECHO));
    attrs.c_iflag &= static_cast<tcflag_t>(~(ICRNL | IXON));
    attrs.c_cc[VMIN] = 1;
    attrs.c_cc[VTIME] = 0;

    if (tcsetattr(STDIN_FILENO, TCSANOW, &attrs) == 0) {
        raw_enabled_ = true;
    }
#endif
}

void Line_editor::disable_raw()
{
    if (!raw_enabled_) {
        return;
    }

#ifdef _WIN32
    if (g_mode_saved) {
        SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), g_original_mode);
    }
#else
    if (g_attrs_saved) {
        tcsetattr(STDIN_FILENO, TCSANOW, &g_original_attrs);
    }
#endif
    raw_enabled_ = false;
}

void Line_editor::redraw(std::ostream &out, const std::string &prompt) const
{
    out << "\r\x1b[K" << prompt << buffer_;
    size_t column = prompt.size() + cursor_;
    if (column > 0) {
        out << "\r\x1b[" << column << "C";
    }
    out.flush();
}

void Line_editor::insert_char(std::ostream &out, const std::string &prompt, char c)
{
    buffer_.insert(buffer_.begin() + static_cast<std::ptrdiff_t>(cursor_), c);
    cursor_++;
    redraw(out, prompt);
}

void Line_editor::backspace(std::ostream &out, const std::string &prompt)
{
    if (cursor_ == 0) {
        return;
    }
    cursor_--;
    buffer_.erase(cursor_, 1);
    redraw(out, prompt);
}

void Line_editor::move_cursor(std::ostream &out, const std::string &prompt, int delta)
{
    if (delta < 0 && cursor_ > 0) {
        cursor_--;
        redraw(out, prompt);
    } else if (delta > 0 && cursor_ < buffer_.size()) {
        cursor_++;
        redraw(out, prompt);
    }
}

void Line_editor::jump_to(std::ostream &out, const std::string &prompt, int position)
{
    int target = position < 0 ? 0 : position;
    if (static_cast<size_t>(target) > buffer_.size()) {
        target = static_cast<int>(buffer_.size());
    }
    cursor_ = static_cast<size_t>(target);
    redraw(out, prompt);
}

void Line_editor::navigate_history(std::ostream &out, const std::string &prompt, int delta)
{
    if (history_.empty()) {
        return;
    }

    if (history_index_ == -1) {
        pending_ = buffer_;
    }

    // history_index_ == -1 is the live edit; Up steps to the newest entry.
    int target = history_index_;
    if (target == -1) {
        target = (delta < 0) ? static_cast<int>(history_.size()) - 1 : -1;
    } else {
        target += delta;
    }

    if (target < -1 || target >= static_cast<int>(history_.size())) {
        return;
    }

    history_index_ = target;

    if (history_index_ == -1) {
        buffer_ = pending_;
    } else {
        buffer_ = history_[static_cast<size_t>(history_index_)];
    }
    cursor_ = buffer_.size();
    redraw(out, prompt);
}

void Line_editor::commit_line(const std::string &line)
{
    if (line.empty()) {
        return;
    }
    if (history_.empty() || history_.back() != line) {
        history_.push_back(line);
        if (history_.size() > kHistoryLimit) {
            history_.erase(history_.begin());
        }
    }
}

Line_editor::Result Line_editor::read(std::ostream &out, const std::string &prompt)
{
    buffer_.clear();
    cursor_ = 0;
    history_index_ = -1;
    pending_.clear();

    if (!interactive_) {
        std::string line;
        if (!std::getline(std::cin, line)) {
            return Result{Result::Status::Eof, ""};
        }
        return Result{Result::Status::Line, line};
    }

    enable_raw();
    out << prompt;
    out.flush();

    Result result{Result::Status::Line, ""};

    for (;;) {
        int key = read_key();
        if (key == kEof) {
            result.status = Result::Status::Eof;
            break;
        }

        if (key < 0) {
            switch (key) {
            case kKeyUp:
                navigate_history(out, prompt, -1);
                break;
            case kKeyDown:
                navigate_history(out, prompt, +1);
                break;
            case kKeyLeft:
                move_cursor(out, prompt, -1);
                break;
            case kKeyRight:
                move_cursor(out, prompt, +1);
                break;
            case kKeyHome:
                jump_to(out, prompt, 0);
                break;
            case kKeyEnd:
                jump_to(out, prompt, static_cast<int>(buffer_.size()));
                break;
            case kKeyDelete:
                if (cursor_ < buffer_.size()) {
                    buffer_.erase(cursor_, 1);
                    redraw(out, prompt);
                }
                break;
            default:
                break;
            }
            continue;
        }

        char c = static_cast<char>(key);

        if (c == '\n' || c == '\r') {
            out << "\r\n";
            out.flush();
            result.status = Result::Status::Line;
            result.line = buffer_;
            break;
        }
        if (c == 0x03) { // Ctrl-C: abort the current line
            out << "^C\r\n";
            out.flush();
            result.status = Result::Status::Interrupt;
            break;
        }
        if (c == 0x04) { // Ctrl-D: EOF at an empty line, otherwise clear
            if (buffer_.empty()) {
                out << "\r\n";
                out.flush();
                result.status = Result::Status::Eof;
                break;
            }
            buffer_.clear();
            cursor_ = 0;
            redraw(out, prompt);
            continue;
        }
        if (c == 0x01) { // Ctrl-A
            jump_to(out, prompt, 0);
            continue;
        }
        if (c == 0x05) { // Ctrl-E
            jump_to(out, prompt, static_cast<int>(buffer_.size()));
            continue;
        }
        if (c == 0x15) { // Ctrl-U: kill the whole line
            buffer_.clear();
            cursor_ = 0;
            redraw(out, prompt);
            continue;
        }
        if (c == 0x7f || c == 0x08) { // Backspace
            backspace(out, prompt);
            continue;
        }
        if (c >= 0x20) { // Printable
            insert_char(out, prompt, c);
        }
    }

    disable_raw();

    if (result.status == Result::Status::Line) {
        commit_line(result.line);
    }
    return result;
}

} // namespace phos
