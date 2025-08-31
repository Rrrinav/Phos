#include <cstdlib>
#include <filesystem>
#include <vector>
#include <tuple>
#include <string>
#include <sstream>
#define B_LDR_IMPLEMENTATION
#define BLD_USE_CONFIG
#include "b_ldr.hpp"

auto &cfg = bld::Config::get();

const std::string SRC = "./src/";
const std::string BIN = "./bin/";
const std::string TARGET = BIN + "phos";

// diff util
std::string string_diff(const std::string &got, const std::string &expected)
{
    auto g_lines = bld::str::chop_by_delimiter(got, "\n");
    auto e_lines = bld::str::chop_by_delimiter(expected, "\n");

    std::ostringstream out;
    size_t n = std::max(g_lines.size(), e_lines.size());

    for (size_t i = 0; i < n; ++i)
    {
        std::string g = (i < g_lines.size() ? g_lines[i] : "<missing>");
        std::string e = (i < e_lines.size() ? e_lines[i] : "<missing>");
        if (g != e)
        {
            out << "expected: " << e << "\n";
            out << "got     : " << g << "\n";
        }
    }
    return out.str();
}

void build_interpreter()
{
    bld::fs::create_dir_if_not_exists(BIN);
    if (!std::filesystem::exists(TARGET) && !bld::execute({"g++", "-o", TARGET, SRC + "main.cpp", "--std=c++23", "-O2"}))
    {
        bld::log(bld::Log_type::ERR, "Building failed.");
        std::exit(EXIT_FAILURE);
    }
}

std::tuple<int, int, std::vector<std::pair<std::string, std::string>>> run_tests()
{
    std::string dir = cfg["test"];
    if (dir.empty())
    {
        bld::log(bld::Log_type::ERR, "Enter test dir");
        std::exit(EXIT_FAILURE);
    }

    build_interpreter();
    auto files = bld::fs::get_all_files_with_extensions(dir, {"phos"});

    int tests_passed = 0;
    int tests_failed = 0;
    std::vector<std::pair<std::string, std::string>> failed;

    for (const auto &f : files)
    {
        std::string output{};
        if (!bld::read_process_output({TARGET, f}, output))
        {
            bld::log(bld::Log_type::ERR, "Reading process output failed");
            std::exit(EXIT_FAILURE);
        }

        // assume expected file is "<test>.expected"
        std::string expected_file = bld::fs::get_stem(f, true) + ".expected";
        std::string expected{};
        if (!bld::fs::read_file(expected_file, expected))
        {
            bld::log(bld::Log_type::ERR, "Reading expected output file failed: " + expected_file);
            std::exit(EXIT_FAILURE);
        }

        if (output == expected)
        {
            tests_passed++;
        }
        else
        {
            tests_failed++;
            std::string diff = string_diff(output, expected);
            failed.push_back({f, diff});
        }
    }

    return {tests_passed, tests_failed, failed};
}

int main(int argc, char *argv[])
{
    BLD_REBUILD_AND_ARGS();

    if (cfg["test"])
    {
        auto [passed, failed, info] = run_tests();
        bld::log(bld::Log_type::INFO, "Tests passed: " + std::to_string(passed) + ", failed: " + std::to_string(failed));

        for (auto &[file, diff] : info)
        {
            bld::log(bld::Log_type::ERR, "Test failed: " + file);
            std::cerr << diff << "\n";
        }
        return 0;
    }
    else
    {
        build_interpreter();
        return 0;
    }
}
