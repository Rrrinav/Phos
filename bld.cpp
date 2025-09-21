#include <cstdlib>
#include <filesystem>
#include <format>
#include <vector>
#include <tuple>
#include <string>
#include <sstream>
#define B_LDR_IMPLEMENTATION
#define BLD_USE_CONFIG
#include "b_ldr.hpp"
#include "rep-gen/report_gen.hpp"

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

void build_interpreter(bool release = false)
{
    using namespace std::filesystem;

    bld::fs::create_dir_if_not_exists(BIN);

    // Gather all source files
    auto cpp_files = bld::fs::get_all_files_with_extensions(SRC, {"cpp"}, true);
    auto hpp_files = bld::fs::get_all_files_with_extensions(SRC, {"hpp"}, true);

    std::vector<std::string> objs;
    std::vector<bld::Proc> builds;
    std::vector<std::string> files_to_build;

    // Step 1: Determine which .cpp files actually need rebuilding
    for (auto f : cpp_files)
    {
        std::string out_p = bld::fs::get_stem(bld::str::replace(f, SRC, BIN), true) + ".o";
        objs.push_back(out_p);

        bld::fs::create_dir_if_not_exists(path(out_p).parent_path().string());

        bool needs_rebuild = bld::is_executable_outdated(f, out_p);

        // Dependency logic
        if (!needs_rebuild)
        {
            if (f.find("main.cpp") != std::string::npos)
            {
                // main.cpp depends on ALL headers
                for (auto &hpp : hpp_files)
                {
                    if (bld::is_executable_outdated(hpp, out_p))
                    {
                        needs_rebuild = true;
                        break;
                    }
                }
            }
            else
            {
                // other .cpp depends only on matching header (if present)
                std::string header = bld::str::replace(f, ".cpp", ".hpp");
                if (exists(header) && bld::is_executable_outdated(header, out_p))
                    needs_rebuild = true;
            }
        }

        if (needs_rebuild)
            files_to_build.push_back(f);
    }

    // Step 2: Compile all files that need rebuilding asynchronously
    if (!files_to_build.empty())
        bld::log(bld::Log_type::INFO, "Rebuilding " + std::to_string(files_to_build.size()) + " files...");

    for (auto &f : files_to_build)
    {
        std::string out_p = bld::fs::get_stem(bld::str::replace(f, SRC, BIN), true) + ".o";

        bld::Command cmp_cmd = {"g++", "-c", f, "-o", out_p, "--std=c++23"};
        if (release)
            cmp_cmd.add_parts("-O2");
        else
            cmp_cmd.add_parts("-ggdb", "-O0");

        bld::log(bld::Log_type::INFO, std::format("Building: {}", f));
        auto p = bld::execute_async(cmp_cmd);
        if (p)
            builds.push_back(p);
        else
            bld::log(bld::Log_type::ERR, "Failed to spawn build for " + f);
    }

    bld::wait_procs(builds);

    // Step 3: Link all .o files into final binary
    bld::Command link_cmd = {"g++", "-o", TARGET};
    for (auto &o : objs)
        link_cmd.add_parts(o);
    link_cmd.add_parts("--std=c++23");

    if (release)
        link_cmd.add_parts("-O2");
    else
        link_cmd.add_parts("-ggdb", "-O0");

    if (!bld::execute(link_cmd))
    {
        bld::log(bld::Log_type::ERR, "Linking failed.");
        std::exit(EXIT_FAILURE);
    }

    bld::log(bld::Log_type::INFO, "Build complete.");
}

std::tuple<std::vector<std::string>, std::vector<std::pair<std::string, std::string>>> run_tests()
{
    std::string dir = cfg["test"];
    if (dir.empty())
    {
        bld::log(bld::Log_type::ERR, "Enter test dir");
        std::exit(EXIT_FAILURE);
    }

    if (!std::filesystem::exists(TARGET))
        build_interpreter();

    auto files = bld::fs::get_all_files_with_extensions(dir, {"phos"});

    int tests_passed = 0;
    int tests_failed = 0;
    std::vector<std::pair<std::string, std::string>> failed;
    std::vector<std::string> passed_files;

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
            passed_files.push_back(f);
        }
        else
        {
            tests_failed++;
            std::string diff = string_diff(output, expected);
            failed.push_back({f, diff});
        }
    }

    return {passed_files, failed};
}

int main(int argc, char *argv[])
{
    BLD_REBUILD_AND_ARGS();

    if (cfg["test"])
    {
        auto [passed_files, failed_info] = run_tests();
        int passed_count = passed_files.size();
        int failed_count = failed_info.size();

        bld::log(bld::Log_type::INFO, "Tests passed: " + std::to_string(passed_count) + ", failed: " + std::to_string(failed_count));

        // Call the new report generator with the separated lists
        report_gen::generate_html_report(passed_files, failed_info);

        for (auto &[file, diff] : failed_info)
        {
            bld::log(bld::Log_type::ERR, "Test failed: " + file);
            std::cerr << diff << "\n";
        }
        return failed_count > 0 ? 1 : 0;
    }
    else if (cfg["clean"])
    {
        bld::fs::remove_dir(BIN);
    }
    else
    {
        bool rel = cfg["rel"] ? true : false;
        build_interpreter(rel);
        return 0;
    }
}
