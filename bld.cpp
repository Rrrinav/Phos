#include <cstdlib>
#include <exception>
#include <filesystem>
#include <format>
#include <print>
#include <vector>
#include <tuple>
#include <string>
#include <sstream>
#include <atomic>
#include <mutex>
#include <thread>

#define B_LDR_IMPLEMENTATION
#define BLD_USE_CONFIG
#define BLD_NO_COLORS
#include "b_ldr.hpp"

auto &cfg = bld::Config::get();

const std::string SRC = "./src/";
const std::string BIN = "./bin/";
const std::string TARGET = BIN + "phos";

const std::vector<std::string> COMMON_FLAGS = {"--std=c++23", "-pthread"};
const std::vector<std::string> DEBUG_FLAGS = {"-ggdb", "-O0"};
const std::vector<std::string> RELEASE_FLAGS = {"-O2", "-DNDEBUG"};

struct Progress
{
    std::mutex mtx;
    size_t total{0};
    std::atomic<size_t> done{0};

    void init(size_t n)
    {
        total = n;
        done = 0;
    }

    void tick(const std::string &label = "")
    {
        size_t cur = ++done;
        std::lock_guard<std::mutex> lk(mtx);
        print(cur, label);
    }

    void print(size_t cur, const std::string &label = "") const
    {
        if (total == 0)
            return;
        constexpr int BAR = 40;
        int filled = static_cast<int>((cur * BAR) / total);
        int pct = static_cast<int>((cur * 100) / total);

        std::string bar(filled, '=');
        if (filled < BAR)
            bar += '>';
        bar.resize(BAR, ' ');

        std::string lbl = label.size() > 30 ? "..." + label.substr(label.size() - 27) : label;

        std::fprintf(stderr, "\r  [%s] %3d%% (%zu/%zu)  %-33s", bar.c_str(), pct, cur, total, lbl.c_str());
        std::fflush(stderr);

        if (cur >= total)
            std::fprintf(stderr, "\n");
    }
};

static Progress progress;

static std::string string_diff(const std::string &got, const std::string &expected)
{
    auto g = bld::str::chop_by_delimiter(got, "\n");
    auto e = bld::str::chop_by_delimiter(expected, "\n");
    std::ostringstream out;
    size_t n = std::max(g.size(), e.size());
    for (size_t i = 0; i < n; ++i)
    {
        std::string gl = (i < g.size() ? g[i] : "<missing>");
        std::string el = (i < e.size() ? e[i] : "<missing>");
        if (gl != el)
        {
            out << "  expected: " << el << "\n";
            out << "  got     : " << gl << "\n";
        }
    }
    return out.str();
}

static void add_mode_flags(bld::Command &cmd, bool release)
{
    for (auto &f : COMMON_FLAGS) cmd.add_parts(f);
    if (release)
        for (auto &f : RELEASE_FLAGS) cmd.add_parts(f);
    else
        for (auto &f : DEBUG_FLAGS) cmd.add_parts(f);
}

void build_interpreter(bool release = false, bool force = false)
{
    using namespace std::filesystem;

    bld::fs::create_dir_if_not_exists(BIN);

    auto cpp_files = bld::fs::get_all_files_with_extensions(SRC, {"cpp"}, true);
    auto hpp_files = bld::fs::get_all_files_with_extensions(SRC, {"hpp"}, true);

    std::vector<std::string> objs;
    std::vector<std::string> files_to_build;

    for (auto &f : cpp_files)
    {

        std::string out_p = bld::fs::get_stem(bld::str::replace(f, SRC, BIN), true) + ".o";
        objs.push_back(out_p);

        bld::fs::create_dir_if_not_exists(path(out_p).parent_path().string());

        if (force)
        {
            files_to_build.push_back(f);
            continue;
        }

        bool needs = bld::is_executable_outdated(f, out_p);

        if (!needs)
        {
            // main.cpp: check all headers; others: check paired header only
            if (f.find("main.cpp") != std::string::npos)
            {
                for (auto &h : hpp_files)
                    if (bld::is_executable_outdated(h, out_p))
                    {
                        needs = true;
                        break;
                    }
            }
            else
            {
                std::string hdr = bld::str::replace(f, ".cpp", ".hpp");
                if (exists(hdr) && bld::is_executable_outdated(hdr, out_p))
                    needs = true;
            }
        }

        if (needs)
            files_to_build.push_back(f);
    }

    if (files_to_build.empty())
    {
        bld::log(bld::Log_type::INFO, "Nothing to rebuild.");
    }
    else
    {
        bld::log(bld::Log_type::INFO, std::format("Compiling {} file(s) [{}] ...", files_to_build.size(), release ? "release" : "debug"));

        progress.init(files_to_build.size());

        std::vector<bld::Proc> procs;
        std::vector<std::string> labels;
        procs.reserve(files_to_build.size());
        labels.reserve(files_to_build.size());

        for (auto &f : files_to_build)
        {
            std::string out_p = bld::fs::get_stem(bld::str::replace(f, SRC, BIN), true) + ".o";

            bld::Command cmd = {"g++", "-c", f, "-o", out_p};
            add_mode_flags(cmd, release);

            auto p = bld::execute_async(cmd);
            if (p)
            {
                procs.push_back(p);
                labels.push_back(bld::fs::get_file_name(f));
            }
            else
                bld::log(bld::Log_type::ERR, "Failed to start: " + f);
        }

        std::vector<bool> done(procs.size(), false);
        size_t remaining = procs.size();
        while (remaining > 0)
        {
            for (size_t i = 0; i < procs.size(); ++i)
            {
                if (done[i])
                    continue;
                auto ws = bld::try_wait_nb(procs[i]);
                if (ws.exited)
                {
                    done[i] = true;
                    --remaining;
                    progress.tick(labels[i]);
                    if (!ws.normal || ws.exit_code != 0)
                        bld::log(bld::Log_type::ERR, "Compilation failed: " + files_to_build[i]);
                    bld::cleanup_process(procs[i]);
                }
            }
            if (remaining > 0)
                std::this_thread::sleep_for(std::chrono::milliseconds(50));
        }
    }

    bld::log(bld::Log_type::INFO, "Linking -> " + TARGET);
    bld::Command link_cmd = {"g++", "-o", TARGET};
    for (auto &o : objs) link_cmd.add_parts(o);
    add_mode_flags(link_cmd, release);

    if (!bld::execute(link_cmd))
    {
        bld::log(bld::Log_type::ERR, "Linking failed.");
        std::exit(EXIT_FAILURE);
    }

    bld::log(bld::Log_type::INFO, "Build complete -> " + TARGET);
}

void build_custom_interpreter(bool release = false, bool force = false)
{
    using namespace std::filesystem;

    bld::fs::create_dir_if_not_exists(BIN);

    std::string objs_val = cfg["objs"];
    if (objs_val.empty())
    {
        bld::log(bld::Log_type::ERR, "No 'objs' list provided. Use -objs=file1.cpp,file2.cpp");
        std::exit(EXIT_FAILURE);
    }

    auto requested = bld::str::chop_by_delimiter(objs_val, ",");

    std::vector<std::string> objs_to_link;
    std::vector<bld::Proc> procs;
    std::vector<std::string> labels;

    for (auto req : requested)
    {
        req = bld::str::trim(req);  // trim the local copy
        std::string src_file = SRC + req;
        std::string out_p = bld::fs::get_stem(bld::str::replace(src_file, SRC, BIN), true) + ".o";

        bld::fs::create_dir_if_not_exists(path(out_p).parent_path().string());
        objs_to_link.push_back(out_p);

        bool needs = force || bld::is_executable_outdated(src_file, out_p);
        if (!needs)
        {
            std::string hdr = bld::str::replace(src_file, ".cpp", ".hpp");
            if (exists(hdr) && bld::is_executable_outdated(hdr, out_p))
                needs = true;
        }

        if (needs)
        {
            bld::Command cmd = {"g++", "-c", src_file, "-o", out_p};
            add_mode_flags(cmd, release);
            auto p = bld::execute_async(cmd);
            if (p)
            {
                procs.push_back(p);
                labels.push_back(req);
            }
        }
    }

    if (!procs.empty())
    {
        bld::log(bld::Log_type::INFO, std::format("Compiling {} file(s) [custom, {}] ...", procs.size(), release ? "release" : "debug"));
        progress.init(procs.size());

        std::vector<bool> done(procs.size(), false);
        size_t remaining = procs.size();
        while (remaining > 0)
        {
            for (size_t i = 0; i < procs.size(); ++i)
            {
                if (done[i])
                    continue;
                auto ws = bld::try_wait_nb(procs[i]);
                if (ws.exited)
                {
                    done[i] = true;
                    --remaining;
                    progress.tick(labels[i]);
                    if (!ws.normal || ws.exit_code != 0)
                        bld::log(bld::Log_type::ERR, "Compilation failed: " + labels[i]);
                    bld::cleanup_process(procs[i]);
                }
            }
            if (remaining > 0)
                std::this_thread::sleep_for(std::chrono::milliseconds(50));
        }
    }

    bld::log(bld::Log_type::INFO, "Linking -> " + TARGET);
    bld::Command link_cmd = {"g++", "-o", TARGET};
    for (auto &o : objs_to_link) link_cmd.add_parts(o);
    add_mode_flags(link_cmd, release);

    if (!bld::execute(link_cmd))
    {
        bld::log(bld::Log_type::ERR, "Custom linking failed.");
        std::exit(EXIT_FAILURE);
    }

    bld::log(bld::Log_type::INFO, "Custom build complete -> " + TARGET);
}

std::tuple<std::vector<std::string>, std::vector<std::pair<std::string, std::string>>> run_tests()
{
    // cfg["test"] holds the directory value (e.g. -test=./tests)
    std::string dir = cfg["test"];
    if (dir.empty())
    {
        bld::log(bld::Log_type::ERR, "Provide test directory with -test=<dir>");
        std::exit(EXIT_FAILURE);
    }

    if (!std::filesystem::exists(TARGET))
        build_interpreter();

    auto files = bld::fs::get_all_files_with_extensions(dir, {"phos"});

    if (files.empty())
    {
        bld::log(bld::Log_type::WARNING, "No .phos test files found in: " + dir);
        return {{}, {}};
    }

    bld::log(bld::Log_type::INFO, std::format("Running {} test(s) ...", files.size()));
    progress.init(files.size());

    std::vector<std::pair<std::string, std::string>> failed;
    std::vector<std::string> passed;

    for (auto &f : files)
    {
        std::string output{};
        bld::Command run_cmd = {TARGET, f};
        if (!bld::read_process_output(run_cmd, output))
        {
            bld::log(bld::Log_type::ERR, "Failed to run interpreter on: " + f);
            std::exit(EXIT_FAILURE);
        }

        std::string expected_file = bld::fs::get_stem(f, true) + ".expected";
        std::string expected{};
        if (!bld::fs::read_file(expected_file, expected))
        {
            bld::log(bld::Log_type::ERR, "Missing expected output: " + expected_file);
            std::exit(EXIT_FAILURE);
        }

        if (output == expected)
            passed.push_back(f);
        else
            failed.push_back({f, string_diff(output, expected)});

        progress.tick(bld::fs::get_file_name(f));
    }

    return {passed, failed};
}

auto compile_custom(std::string ip, std::string op = "./new")
{
    namespace stdfs = std::filesystem;

    // std::filesystem::equivalent only works if those files exist;
    bld::fs::write_entire_file(op, "");

    bld::Command cmd;
    cmd.add_parts("g++", "-o", op, ip, "-Wall", "--std=c++23", "-lm", "-pthread", "-ggdb", "-O0");

    bld::fs::walk_directory(BIN, [ip, op, &cmd] (bld::fs::Walk_fn_opt& opt) -> bool {
        if (stdfs::is_directory(opt.path)) return true;
        if (stdfs::equivalent(opt.path, "./bin/main.o")) return true;
        if (stdfs::equivalent(opt.path, "./bin/phos")) return true;
        if (stdfs::equivalent(opt.path, "./bin/new")) return true;
        if (stdfs::equivalent(opt.path, op)) return true;
        cmd.add_parts(opt.path.string());
        return true;
    });

    if (!bld::execute(cmd))
    {
        bld::fs::remove(op);
        return false;
    }
    return true;
}

int main(int argc, char *argv[])
{
    BLD_REBUILD_AND_ARGS();

    // register custom flags so they're recognised by parse_args
    cfg.add_flag("rel", "Release build (-O2 -DNDEBUG)");
    cfg.add_flag("force", "Force rebuild of all files");
    cfg.add_flag("clean", "Remove build directory");
    cfg.add_option("test", "", "Run tests in given directory (e.g. -test=./tests)");
    cfg.add_option("objs", "", "Comma-separated source files for custom build");

    bool release = static_cast<bool>(cfg["rel"]);
    bool force = static_cast<bool>(cfg["force"]);
    if (cfg["clean"])
    {
        bld::log(bld::Log_type::INFO, "Cleaning " + BIN);
        bld::fs::remove_dir(BIN);
        bld::log(bld::Log_type::INFO, "Done.");
    }
    else if (!std::string(cfg["objs"]).empty())
    {
        build_custom_interpreter(release, force);
    }
    else
    {
        build_interpreter(release, force);
    }

    if (cfg["compile"])
    {
        std::string file{""};
        if (cfg["o"])
            file = std::string(cfg["o"]);
        else
            file =  "./bin/new";

        compile_custom(cfg["compile"], file);
        bld::log(bld::Log_type::INFO, "Build complete -> " + file);
    }

    return 0;
}
