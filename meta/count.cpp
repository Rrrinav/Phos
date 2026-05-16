#include <algorithm>
#include <cctype>
#include <filesystem>
#include <format>
#include <fstream>
#include <print>
#include <ranges>
#include <string>
#include <unordered_map>

namespace fs = std::filesystem;

struct File_stats
{
    uint64_t files = 0;

    uint64_t total_lines = 0;
    uint64_t blank_lines = 0;
    uint64_t comment_lines = 0;
    uint64_t code_lines = 0;

    uint64_t total_words = 0;
    uint64_t total_chars = 0;
    uint64_t total_bytes = 0;

    uint64_t include_count = 0;
    uint64_t class_count = 0;
    uint64_t struct_count = 0;
    uint64_t enum_count = 0;
    uint64_t function_estimate = 0;

    uint64_t longest_line = 0;
};

bool is_cpp_file(const fs::path &path)
{
    static constexpr std::string_view exts[] = {".cpp", ".cc", ".cxx", ".c", ".hpp", ".hh", ".hxx", ".h"};

    auto ext = path.extension().string();

    return std::ranges::find(exts, ext) != std::end(exts);
}

uint64_t count_words(const std::string &line)
{
    bool in_word = false;

    uint64_t count = 0;

    for (char c : line) {
        if (std::isspace(static_cast<unsigned char>(c))) {
            in_word = false;
        } else if (!in_word) {
            in_word = true;
            count++;
        }
    }

    return count;
}

bool looks_like_function(const std::string &line)
{
    return line.contains('(') && line.contains(')') && line.contains('{') && !line.starts_with("if") && !line.starts_with("for")
        && !line.starts_with("while") && !line.starts_with("switch") && !line.starts_with("catch");
}

File_stats analyze_file(const fs::path &path)
{
    File_stats stats{};

    std::ifstream file(path);

    if (!file.is_open()) {
        return stats;
    }

    stats.files = 1;
    stats.total_bytes = fs::file_size(path);

    std::string line{};

    bool in_block_comment = false;

    while (std::getline(file, line)) {
        stats.total_lines++;

        stats.total_chars += line.size();
        stats.total_words += count_words(line);

        stats.longest_line = std::max<uint64_t>(stats.longest_line, line.size());

        auto trimmed = line | std::views::drop_while([](char c) { return std::isspace(static_cast<unsigned char>(c)); });

        std::string t(trimmed.begin(), trimmed.end());

        if (t.empty()) {
            stats.blank_lines++;
            continue;
        }

        bool is_comment = false;

        if (in_block_comment) {
            is_comment = true;

            if (t.contains("*/")) {
                in_block_comment = false;
            }
        } else if (t.starts_with("//")) {
            is_comment = true;
        } else if (t.starts_with("/*")) {
            is_comment = true;

            if (!t.contains("*/")) {
                in_block_comment = true;
            }
        }

        if (is_comment) {
            stats.comment_lines++;
            continue;
        }

        stats.code_lines++;

        if (t.starts_with("#include")) {
            stats.include_count++;
        }

        if (t.contains("class ")) {
            stats.class_count++;
        }

        if (t.contains("struct ")) {
            stats.struct_count++;
        }

        if (t.contains("enum ")) {
            stats.enum_count++;
        }

        if (looks_like_function(t)) {
            stats.function_estimate++;
        }
    }

    return stats;
}

void merge(File_stats &dst, const File_stats &src)
{
    dst.files += src.files;

    dst.total_lines += src.total_lines;
    dst.blank_lines += src.blank_lines;
    dst.comment_lines += src.comment_lines;
    dst.code_lines += src.code_lines;

    dst.total_words += src.total_words;
    dst.total_chars += src.total_chars;
    dst.total_bytes += src.total_bytes;

    dst.include_count += src.include_count;
    dst.class_count += src.class_count;
    dst.struct_count += src.struct_count;
    dst.enum_count += src.enum_count;
    dst.function_estimate += src.function_estimate;

    dst.longest_line = std::max(dst.longest_line, src.longest_line);
}

std::string human_size(uint64_t bytes)
{
    constexpr const char *suffixes[] = {"B", "KB", "MB", "GB"};

    double size = static_cast<double>(bytes);

    int i = 0;

    while (size >= 1024.0 && i < 3) {
        size /= 1024.0;
        ++i;
    }

    return std::format("{:.2f} {}", size, suffixes[i]);
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        std::println("Usage: stats <directory>");
        return 1;
    }

    fs::path root = argv[1];

    if (!fs::exists(root) || !fs::is_directory(root)) {
        std::println("Invalid directory.");
        return 1;
    }

    File_stats total{};

    std::unordered_map<std::string, uint64_t> ext_counts;

    for (const auto &entry : fs::recursive_directory_iterator(root)) {
        if (!entry.is_regular_file()) {
            continue;
        }

        auto path = entry.path();

        if (!is_cpp_file(path)) {
            continue;
        }

        auto stats = analyze_file(path);

        merge(total, stats);

        ext_counts[path.extension().string()]++;
    }

    std::println("Directory              : {}", root.string());

    std::println("C/C++ Files            : {}", total.files);
    std::println("Total Size             : {}", human_size(total.total_bytes));
    std::println("Total Lines            : {}", total.total_lines);
    std::println("Code Lines             : {}", total.code_lines);
    std::println("Total Words            : {}", total.total_words);
    std::println("Total Characters       : {}", total.total_chars);
    std::println("Longest Line           : {}", total.longest_line);
    std::println("Classes                : {}", total.class_count);
    std::println("Structs                : {}", total.struct_count);
    std::println("Enums                  : {}", total.enum_count);
    std::println("Estimated Functions    : {}", total.function_estimate);
    std::println("Files                  :");

    for (const auto &[ext, count] : ext_counts) {
        std::println("    {:<10} {}", ext, count);
    }

    return 0;
}
