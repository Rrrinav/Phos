#include <algorithm>
#include <chrono>
#include <cstdlib>
#include <filesystem>
#include <sstream>
#include <string>
#include <string_view>
#include <tuple>
#include <unordered_set>
#include <vector>

#define B_LDR_IMPLEMENTATION
#include "bld_r.hpp"

inline auto &cfg = bld::Config::get();

const std::string SRC = "./src/";
const std::string BIN = "./bin/";
const std::string BIN_META = BIN + "meta/";
const std::string TARGET = BIN + "phos";

// Objects and static libs are per-profile: the archive stores basenames, so
// sharing one lib across profiles would silently mix/release-stale objects,
// and `ar rcs` never drops members for deleted sources.
static std::string profile_key()
{
    if (bool(cfg["rel"])) {
        return "release";
    }
    if (bool(cfg["san"])) {
        return "debug-san";
    }
    return "debug";
}

static std::string lib_path()
{
    return BIN + "libphos-" + profile_key() + ".a";
}

const std::vector<std::string> COMMON_FLAGS = {"--std=c++23", "-pthread", "-I./src", "-Wpedantic", "-Wall", "-Wextra"};
const std::vector<std::string> DEBUG_FLAGS = {"-ggdb", "-O0"};
const std::vector<std::string> RELEASE_FLAGS = {"-O2", "-DNDEBUG"};
const std::vector<std::string> SANITIZER_FLAGS = {"-fsanitize=address", "-fsanitize=undefined"};

static std::vector<std::string> mode_flags(bool release)
{
    std::vector<std::string> out = COMMON_FLAGS;
    if (release) {
        out.insert(out.end(), RELEASE_FLAGS.begin(), RELEASE_FLAGS.end());
    } else {
        out.insert(out.end(), DEBUG_FLAGS.begin(), DEBUG_FLAGS.end());
        if (bool(cfg["san"])) {
            out.insert(out.end(), SANITIZER_FLAGS.begin(), SANITIZER_FLAGS.end());
        }
    }
    return out;
}

static std::string make_obj_path(const std::string &src, const std::string &obj_root)
{
    std::string rel = src;
    if (rel.rfind(SRC, 0) == 0) {
        rel = rel.substr(SRC.size());
    }
    std::filesystem::path p = std::filesystem::path(obj_root) / rel;
    p.replace_extension(".o");
    return p.string();
}

static std::string depfile_path(const std::string &obj)
{
    std::filesystem::path p(obj);
    p.replace_extension(".d");
    return p.string();
}

static std::string object_root()
{
    return BIN + profile_key() + "/";
}

// Precise header tracking via compiler depfiles (-MMD -MP -MF). The old
// sibling-header-only tracking silently linked stale objects when a
// widely-included header changed (ABI mismatch). If no depfile exists yet
// (first build), fall back to src + sibling header; the missing object
// forces a build anyway, which then writes the depfile.
static std::vector<std::string> compile_inputs(const std::string &src, const std::string &obj)
{
    auto dep = bld::fs::read_file(depfile_path(obj));
    if (dep) {
        std::string text = *dep;
        // Join backslash-newline continuations.
        std::string flat;
        flat.reserve(text.size());
        for (size_t i = 0; i < text.size(); ++i) {
            if (text[i] == '\\' && i + 1 < text.size() && text[i + 1] == '\n') {
                ++i;
                flat += ' ';
            } else if (text[i] == '\n' || text[i] == '\r') {
                flat += ' ';
            } else {
                flat += text[i];
            }
        }
        std::vector<std::string> inputs;
        std::string cur;
        auto flush = [&]() {
            if (!cur.empty() && cur.back() != ':' && cur != "\\") {
                inputs.push_back(cur);
            }
            cur.clear();
        };
        for (char c : flat) {
            if (c == ' ' || c == '\t') {
                flush();
            } else {
                cur += c;
            }
        }
        flush();
        if (!inputs.empty()) {
            return inputs;
        }
    }

    std::vector<std::string> fallback = {src};
    std::string hdr = src;
    const std::string from = ".cpp";
    const std::string to = ".hpp";
    if (hdr.size() >= from.size() && hdr.compare(hdr.size() - from.size(), from.size(), from) == 0) {
        hdr.replace(hdr.size() - from.size(), from.size(), to);
        if (bld::fs::exists(hdr)) {
            fallback.push_back(hdr);
        }
    }
    return fallback;
}

static std::vector<std::string> all_cpp_sources()
{
    auto res = bld::fs::find_by_ext("src", ".cpp");
    if (!res) {
        bld::log::e("Failed to scan src/: {}", res.error().message());
        return {};
    }
    std::vector<std::string> out = std::move(*res);
    std::sort(out.begin(), out.end());
    return out;
}

static bld::Cmd compile_cmd(const std::string &src, const std::string &obj, const std::vector<std::string> &flags)
{
    bld::Cmd cmd{"g++", "-c", src, "-o", obj, "-MMD", "-MP", "-MF", depfile_path(obj)};
    for (auto &f : flags) {
        cmd.push(f);
    }
    return cmd;
}

static bool run_plan(bld::Plan &plan, bool force)
{
    std::expected<bld::Run_result, bld::Err> res;
    if (force) {
        res = bld::run(plan, bld::force{});
    } else {
        res = bld::run(plan);
    }
    if (!res) {
        bld::log::e("Build failed: {}", res.error());
        return false;
    }
    if (res->skipped > 0 && res->ran == 0) {
        bld::log::i("Everything up to date.");
    } else {
        bld::log::i("Build steps: {} ran, {} up-to-date, {} failed.", res->ran, res->skipped, res->failed);
    }
    return true;
}

// High-level build steps
static bool build_core_library(const std::string &obj_root, bool release, bool force, std::vector<std::string> &obj_paths)
{
    auto cpp_files = all_cpp_sources();
    cpp_files.erase(
        std::remove_if(cpp_files.begin(), cpp_files.end(), [](const std::string &f) { return f.find("main.cpp") != std::string::npos; }),
        cpp_files.end());

    auto flags = mode_flags(release);

    obj_paths.clear();
    for (auto &s : cpp_files) {
        obj_paths.push_back(make_obj_path(s, obj_root));
    }

    // Ensure obj dirs exist up-front (avoids races between parallel tasks).
    for (auto &o : obj_paths) {
        std::string parent = std::filesystem::path(o).parent_path().string();
        if (!parent.empty() && !bld::fs::exists(parent)) {
            std::ignore = bld::fs::make_dirs(parent);
        }
    }
    if (!bld::fs::exists(BIN)) {
        std::ignore = bld::fs::make_dirs(BIN);
    }

    bld::Plan plan;
    for (size_t i = 0; i < cpp_files.size(); ++i) {
        const std::string &src = cpp_files[i];
        const std::string &obj = obj_paths[i];
        std::string task_name = "compile " + src;
        auto &t = plan.add(task_name, compile_cmd(src, obj, flags));
        std::string name = t.name;
        for (auto &in : compile_inputs(src, obj)) {
            plan.needs(name, in);
        }
        plan.produces(name, obj);
    }

    const std::string lib = lib_path();
    // `ar rcs` never drops members for deleted/renamed sources, so prune the
    // archive when its member set no longer matches this build's objects.
    {
        std::string current_members;
        if (bld::fs::exists(lib)) {
            if (auto proc = bld::run(bld::Cmd{"ar", "t", lib}, bld::out_err_str{current_members}); !proc) {
                bld::log::w("Could not list archive members for {}", lib);
            }
        }
        std::unordered_set<std::string> wanted;
        for (auto &o : obj_paths) {
            wanted.insert(std::filesystem::path(o).filename().string());
        }
        bool stale = !bld::fs::exists(lib);
        if (!stale) {
            std::istringstream list(current_members);
            std::string member;
            size_t count = 0;
            while (std::getline(list, member)) {
                member = std::string(bld::str::trim(member));
                if (member.empty()) {
                    continue;
                }
                ++count;
                if (!wanted.contains(member)) {
                    stale = true;
                    break;
                }
            }
            stale = stale || count != wanted.size();
        }
        if (stale) {
            std::ignore = bld::fs::remove(lib);
        }
    }

    bld::Cmd ar{"ar", "rcs", lib};
    for (auto &o : obj_paths) {
        ar.push(o);
    }
    auto &at = plan.add("archive libphos", ar);
    std::string aname = at.name;
    for (auto &o : obj_paths) {
        plan.needs(aname, o);
    }
    plan.produces(aname, lib);

    return run_plan(plan, force);
}

void build_interpreter(bool release = false, bool force = false)
{
    if (!bld::fs::exists(BIN)) {
        std::ignore = bld::fs::make_dirs(BIN);
    }
    std::string objr = object_root();
    if (!bld::fs::exists(objr)) {
        std::ignore = bld::fs::make_dirs(objr);
    }
    const std::string obj_root = object_root();

    bld::log::i("Building interpreter [{}] ...", release ? "release" : "debug");

    std::vector<std::string> obj_paths;
    if (!build_core_library(obj_root, release, force, obj_paths)) {
        std::exit(EXIT_FAILURE);
    }

    // main.o depends on main.cpp; conservatively also on all headers
    // (same as the old script: any header change rebuilds main).
    const std::string main_src = SRC + "main.cpp";
    const std::string main_obj = obj_root + "main.o";
    auto flags = mode_flags(release);

    {
        bld::Plan plan;
        auto &t = plan.add("compile src/main.cpp", compile_cmd(main_src, main_obj, flags));
        std::string name = t.name;
        for (auto &in : compile_inputs(main_src, main_obj)) {
            plan.needs(name, in);
        }
        plan.produces(name, main_obj);

        bld::Cmd link{"g++", "-o", TARGET, main_obj, lib_path()};
        for (auto &f : flags) {
            link.push(f);
        }
        auto &lt = plan.add("link phos", link);
        std::string lname = lt.name;
        plan.needs(lname, main_obj);
        plan.needs(lname, lib_path());
        plan.produces(lname, TARGET);

        if (!run_plan(plan, force)) {
            std::exit(EXIT_FAILURE);
        }
    }

    bld::log::i("Build complete -> {}", TARGET);
}

void build_custom_interpreter(bool release = false, bool force = false)
{
    if (!bld::fs::exists(BIN)) {
        std::ignore = bld::fs::make_dirs(BIN);
    }
    std::string objr2 = object_root();
    if (!bld::fs::exists(objr2)) {
        std::ignore = bld::fs::make_dirs(objr2);
    }

    std::string objs_val = std::string(cfg["objs"]);
    if (objs_val.empty()) {
        bld::log::e("No 'objs' list provided. Use -objs=file1.cpp,file2.cpp");
        std::exit(EXIT_FAILURE);
    }

    const std::string obj_root = object_root();
    bld::log::i("Building custom interpreter [{}] ...", release ? "release" : "debug");

    std::vector<std::string> lib_objs;
    if (!build_core_library(obj_root, release, force, lib_objs)) {
        std::exit(EXIT_FAILURE);
    }

    auto flags = mode_flags(release);
    std::vector<std::string> extra_srcs;
    for (auto part : bld::str::split(objs_val, ',')) {
        std::string req(bld::str::trim(part));
        if (!req.empty()) {
            extra_srcs.push_back(SRC + req);
        }
    }

    std::vector<std::string> extra_objs;
    for (auto &s : extra_srcs) {
        extra_objs.push_back(make_obj_path(s, obj_root));
    }
    for (auto &o : extra_objs) {
        std::string parent = std::filesystem::path(o).parent_path().string();
        if (!parent.empty() && !bld::fs::exists(parent)) {
            std::ignore = bld::fs::make_dirs(parent);
        }
    }

    bld::Plan plan;
    for (size_t i = 0; i < extra_srcs.size(); ++i) {
        std::string task_name = "compile " + extra_srcs[i];
        auto &t = plan.add(task_name, compile_cmd(extra_srcs[i], extra_objs[i], flags));
        std::string name = t.name;
        for (auto &in : compile_inputs(extra_srcs[i], extra_objs[i])) {
            plan.needs(name, in);
        }
        plan.produces(name, extra_objs[i]);
    }

    bld::Cmd link{"g++", "-o", TARGET};
    for (auto &o : extra_objs) {
        link.push(o);
    }
    link.push(lib_path());
    for (auto &f : flags) {
        link.push(f);
    }
    auto &lt = plan.add("link phos (custom)", link);
    std::string lname = lt.name;
    for (auto &o : extra_objs) {
        plan.needs(lname, o);
    }
    plan.needs(lname, lib_path());
    plan.produces(lname, TARGET);

    if (!run_plan(plan, force)) {
        std::exit(EXIT_FAILURE);
    }
    bld::log::i("Build complete -> {}", TARGET);
}

static std::string stem_no_ext(const std::string &p)
{
    std::filesystem::path fp(p);
    return (fp.parent_path() / fp.stem()).string();
}

std::tuple<std::vector<std::string>, std::vector<std::pair<std::string, std::string>>> run_tests(bool release)
{
    std::string dir = std::string(cfg["test"]);
    if (dir.empty()) {
        dir = "./tests";
    }

    if (!bld::fs::is_dir(dir)) {
        bld::log::e("Test directory does not exist: {}", dir);
        std::exit(EXIT_FAILURE);
    }

    if (!bld::fs::exists(TARGET)) {
        build_interpreter(release);
    }

    auto files_res = bld::fs::find_by_ext(dir, ".phos");
    if (!files_res) {
        bld::log::e("Failed to scan tests: {}", files_res.error().message());
        std::exit(EXIT_FAILURE);
    }
    std::vector<std::string> files = std::move(*files_res);
    // The REPL suite has its own harness (tests/repl/*.in + run_repl_tests.sh);
    // helper.phos is a REPL fixture, not a standalone program. Exclude it here.
    files.erase(
        std::remove_if(files.begin(), files.end(), [](const std::string &f) { return f.find("tests/repl") != std::string::npos; }),
        files.end());
    std::sort(files.begin(), files.end());

    if (files.empty()) {
        bld::log::w("No .phos test files found in: {}", dir);
        return {{}, {}};
    }

    bld::log::i("Running {} test(s) ...", files.size());

    std::vector<std::pair<std::string, std::string>> failed;
    std::vector<std::string> passed;

    auto stem_no_ext = [](const std::string &p) {
        std::filesystem::path fp(p);
        return (fp.parent_path() / fp.stem()).string();
    };

    for (const auto &f : files) {
        // Merged stdout+stderr capture (mirrors the old read_process_output).
        // run() — unlike capture() — does not log expected non-zero exits
        // as errors, keeping error-test output quiet unless they fail.
        std::string output;
        bool ran_ok = false;
        if (auto proc = bld::run(bld::Cmd{TARGET, f}, bld::out_err_str{output}); proc) {
            ran_ok = (proc->status_code() == 0);
        } else {
            failed.push_back({f, "Failed to spawn interpreter: " + proc.error().msg});
            continue;
        }

        // Expected-error tests: a sibling "<file>.error" file contains the
        // diagnostic substring the interpreter must produce on stderr/exit.
        const std::string error_file = stem_no_ext(f) + ".error";
        std::string expected_error;
        bool is_error_test = false;
        if (auto r = bld::fs::read_file(error_file); r) {
            expected_error = *r;
            is_error_test = true;
        }

        if (is_error_test) {
            std::string needle(bld::str::trim(expected_error));
            if (!ran_ok && !needle.empty() && output.find(needle) != std::string::npos) {
                passed.push_back(f);
            } else {
                std::string detail = ran_ok ? "Expected the interpreter to fail, but it ran successfully."
                                            : "Expected diagnostic not found in output:\n" + output;
                failed.push_back({f, detail});
            }
            continue;
        }

        if (!ran_ok) {
            failed.push_back({f, "Interpreter exited non-zero:\n" + output});
            continue;
        }

        const std::string expected_file = stem_no_ext(f) + ".expected";
        auto exp_res = bld::fs::read_file(expected_file);
        if (!exp_res) {
            failed.push_back({f, "Missing expected output: " + expected_file});
            continue;
        }

        auto diff = bld::test::compute_diff(*exp_res, output);
        if (diff) {
            passed.push_back(f);
        } else {
            failed.push_back({f, std::format("{:n}", diff)});
        }
    }

    return {passed, failed};
}

static void run_repl_suite()
{
    const std::string script = "tests/repl/run_repl_tests.sh";
    if (!bld::fs::exists(script) || !bld::fs::exists(TARGET)) {
        return;
    }
    bld::log::i("Running REPL suite ...");
    auto proc = bld::run(bld::Cmd{"bash", script});
    if (!proc) {
        bld::log::w("REPL suite failed to spawn: {}", proc.error());
        return;
    }
    if (proc->status_code() != 0) {
        bld::log::w("REPL suite reported failures (exit {}).", proc->status_code());
    } else {
        bld::log::i("REPL suite passed.");
    }
}

// Runs each bench/*.phos N times, checks the checksum output against the
// sibling .expected file, and reports median wall time. Returns false if any
// bench fails to spawn, exits non-zero, or prints unexpected output.
static bool run_benches(bool release)
{
    const std::string dir = "./bench";
    if (!bld::fs::is_dir(dir)) {
        bld::log::e("Bench directory does not exist: {}", dir);
        return false;
    }

    if (!bld::fs::exists(TARGET)) {
        build_interpreter(release);
    }

    auto files_res = bld::fs::find_by_ext(dir, ".phos");
    if (!files_res) {
        bld::log::e("Failed to scan benches: {}", files_res.error().message());
        return false;
    }
    std::vector<std::string> files = std::move(*files_res);
    std::sort(files.begin(), files.end());

    if (files.empty()) {
        bld::log::w("No bench files found in: {}", dir);
        return true;
    }

    constexpr int kRepeats = 3;
    bool all_ok = true;

    bld::log::i("Running {} bench(s) x{} ...", files.size(), kRepeats);
    for (const auto &f : files) {
        std::vector<double> samples;
        samples.reserve(kRepeats);
        std::string output;
        bool ran_ok = true;

        for (int i = 0; i < kRepeats; ++i) {
            std::string run_out;
            auto start = std::chrono::steady_clock::now();
            auto proc = bld::run(bld::Cmd{TARGET, f}, bld::out_err_str{run_out});
            auto elapsed = std::chrono::steady_clock::now() - start;
            if (!proc) {
                bld::log::e("BENCH FAIL {}: {}", f, proc.error());
                ran_ok = false;
                break;
            }
            if (proc->status_code() != 0) {
                bld::log::e("BENCH FAIL {}: interpreter exited with code {}", f, proc->status_code());
                ran_ok = false;
                break;
            }
            samples.push_back(std::chrono::duration<double, std::milli>(elapsed).count());
            output = std::move(run_out);
        }

        if (!ran_ok) {
            all_ok = false;
            continue;
        }

        auto exp_res = bld::fs::read_file(stem_no_ext(f) + ".expected");
        if (!exp_res) {
            bld::log::e("BENCH FAIL {}: missing expected output", f);
            all_ok = false;
            continue;
        }
        if (bld::test::compute_diff(*exp_res, output)) {
            std::sort(samples.begin(), samples.end());
            double median = samples[samples.size() / 2];
            bld::log::i("BENCH {}: {:.0f} ms (median of {})", f, median, kRepeats);
        } else {
            bld::log::e("BENCH FAIL {}: unexpected output:\n{}", f, output);
            all_ok = false;
        }
    }

    return all_ok;
}

bool compile_custom(const std::string &ip, const std::string &op, bool release)
{
    const std::string lib = lib_path();
    if (!bld::fs::exists(lib)) {
        bld::log::e("Core library not found. Build the interpreter first.");
        return false;
    }

    auto flags = mode_flags(release);
    bld::Cmd cmd{"g++", "-o", op, ip, lib, "-lm"};
    for (auto &f : flags) {
        cmd.push(f);
    }

    auto proc = bld::run(cmd);
    if (!proc || proc->status_code() != 0) {
        std::ignore = bld::fs::remove(op);
        return false;
    }
    return true;
}

void build_meta()
{
    if (!bld::fs::exists(BIN_META)) {
        std::ignore = bld::fs::make_dirs(BIN_META);
    }

    auto files_res = bld::fs::find_by_ext("./meta", ".cpp");
    if (!files_res) {
        bld::log::e("Failed to scan meta/: {}", files_res.error().message());
        std::exit(EXIT_FAILURE);
    }

    bld::Plan plan;
    for (auto &src : *files_res) {
        std::filesystem::path p(src);
        std::string out = BIN_META + p.stem().string();
        bld::Cmd cmd{
            "g++",
            src,
            "-o",
            out,
            "-MMD",
            "-MP",
            "-MF",
            depfile_path(out),
            "-O2",
            "--std=c++26",
            "-pthread",
            "-Wpedantic",
            "-Wall",
            "-freflection",
            "-I./src"};
        auto &t = plan.add("meta " + p.stem().string(), cmd);
        std::string name = t.name;
        for (auto &in : compile_inputs(src, out)) {
            plan.needs(name, in);
        }
        plan.produces(name, out);
    }

    if (!run_plan(plan, false)) {
        std::exit(EXIT_FAILURE);
    }
}

void format()
{
    std::vector<std::string> roots = {"src", "meta", "tests"};
    std::vector<std::string> files;
    for (auto &r : roots) {
        auto res = bld::fs::find_by_ext(r, ".c", ".h", ".cpp", ".hpp", ".cc", ".hh");
        if (res) {
            files.insert(files.end(), res->begin(), res->end());
        }
    }
    std::sort(files.begin(), files.end());

    bld::Plan plan;
    for (auto &f : files) {
        plan.add("format " + f, bld::Cmd{"clang-format", "-i", f});
    }
    auto res = bld::run(plan);
    if (!res) {
        bld::log::e("Formatting failed: {}", res.error());
    }
}

int main(int argc, char *argv[])
{
    if (auto r = bld::rebuild_this_when_needed_ext(argc, argv, {"-pthread"}); !r) {
        bld::log::e("Self-rebuild failed: {}", r.error());
        return 1;
    }

    cfg.add_option("rel", bld::Config::Bool, "Release build (-O2 -DNDEBUG)", false);
    cfg.add_option("san", bld::Config::Bool, "Enable ASan/UBSan in debug builds", false);
    cfg.add_option("force", bld::Config::Bool, "Force rebuild of all files", false);
    cfg.add_option("clean", bld::Config::Bool, "Remove build directory", false);
    cfg.add_option("meta", bld::Config::Bool, "Build meta-programming utilities", false);
    cfg.add_option("code-gen", bld::Config::Bool, "Build meta-programming utilities", false);
    cfg.add_option("count", bld::Config::Bool, "Count lines, files, etc.", false);
    cfg.add_option("format", bld::Config::Bool, "Format the code using clangd.", false);
    cfg.add_option("bench", bld::Config::Bool, "Run bench/*.phos timing suite", false);

    cfg.add_option("test", bld::Config::String, "Run tests in given directory (defaults to ./tests; -test runs the whole suite)", std::string{""});
    cfg.add_option("objs", bld::Config::String, "Comma-separated source files for custom build", std::string{""});
    cfg.add_option("compile", bld::Config::String, "Compile a custom source file alongside the VM", std::string{""});
    cfg.add_option("o", bld::Config::String, "Output executable name for -compile", std::string{""});

    // Old UX allowed bare `-test` to mean "run the whole suite".
    // The new Config requires a value for String options, so rewrite
    // a bare `-test` (no `=` and no following value) to the default dir.
    std::vector<std::string> norm_args;
    norm_args.reserve(static_cast<size_t>(argc));
    for (int i = 0; i < argc; ++i) {
        std::string a = argv[i] ? argv[i] : "";
        std::string key = bld::Config::normalize_key(a);
        bool is_bare_test = (key == "test" && a.find('=') == std::string::npos);
        if (is_bare_test) {
            bool has_value = false;
            if (i + 1 < argc && argv[i + 1] != nullptr) {
                std::string_view nxt = argv[i + 1];
                has_value = !nxt.empty() && nxt[0] != '-';
            }
            if (!has_value) {
                norm_args.push_back(a + "=./tests");
                continue;
            }
        }
        norm_args.push_back(std::move(a));
    }
    std::vector<char *> pargv;
    pargv.reserve(norm_args.size());
    for (auto &s : norm_args) {
        pargv.push_back(s.data());
    }
    int pargc = static_cast<int>(pargv.size());

    if (auto p = cfg.parse(pargc, pargv.data()); !p) {
        bld::log::e("{}", p.error());
        return 1;
    } else if (p->help_requested) {
        return 0;
    }

    const bool release = bool(cfg["rel"]);
    const bool force = bool(cfg["force"]);

    if (bool(cfg["format"])) {
        format();
        return 0;
    }

    if (bool(cfg["bench"])) {
        return run_benches(release) ? 0 : 1;
    }

    if (bool(cfg["count"])) {
        if (bld::fs::exists("./bin/meta/count")) {
            auto proc = bld::run(bld::Cmd{"./bin/meta/count", "./src"});
            return (proc && proc->status_code() == 0) ? 0 : 1;
        } else {
            build_meta();
            auto proc = bld::run(bld::Cmd{"./bin/meta/count", "./src"});
            return (proc && proc->status_code() == 0) ? 0 : 1;
        }
    }

    if (bool(cfg["code-gen"])) {
        if (bld::fs::exists("./bin/meta/basic_meta")) {
            auto proc = bld::run(bld::Cmd{"./bin/meta/basic_meta"});
            return (proc && proc->status_code() == 0) ? 0 : 1;
        } else {
            build_meta();
            auto proc = bld::run(bld::Cmd{"./bin/meta/basic_meta"});
            return (proc && proc->status_code() == 0) ? 0 : 1;
        }
    }

    if (bool(cfg["meta"])) {
        build_meta();
        return 0;
    }

    if (bool(cfg["clean"])) {
        bld::log::i("Cleaning {} ...", BIN);
        std::ignore = bld::fs::remove(BIN);
        bld::log::i("Done.");
        return 0;
    }

    if (std::string(cfg["test"]).empty() == false) {
        auto [passed, failed] = run_tests(release);

        bld::log::i("{} passed, {} failed.", passed.size(), failed.size());

        for (const auto &[file, diff] : failed) {
            bld::log::e("FAIL: {}", file);
            std::println("{}", diff);
        }

        // Run the REPL harness too when running the default suite so the
        // refactor stays covered on both runners.
        std::string tdir = std::string(cfg["test"]);
        if (tdir == "./tests" && failed.empty()) {
            run_repl_suite();
        }

        return failed.empty() ? 0 : 1;
    }

    if (!std::string(cfg["objs"]).empty()) {
        build_custom_interpreter(release, force);
    } else {
        build_interpreter(release, force);
    }

    if (std::string(cfg["compile"]).empty() == false) {
        std::string csrc = std::string(cfg["compile"]);
        std::string cout = std::string(cfg["o"]);
        const std::string out = cout.empty() ? BIN + "new" : cout;
        if (compile_custom(csrc, out, release)) {
            bld::log::i("Build complete -> {}", out);
        } else {
            return 1;
        }
    }

    return 0;
}
