#include <cstdlib>
#include <filesystem>
#include <format>
#include <sstream>
#include <string>
#include <tuple>
#include <vector>

#define B_LDR_IMPLEMENTATION
#define BLD_USE_CONFIG
#include "b_ldr.hpp"

inline auto &cfg = bld::Config::get();

const std::string SRC      = "./src/";
const std::string BIN      = "./bin/";
const std::string BIN_META = BIN + "meta/";
const std::string LIB      = BIN + "libphos.a";
const std::string TARGET   = BIN + "phos";

const std::vector<std::string> COMMON_FLAGS    = {"--std=c++23", "-pthread", "-I./src", "-Wpedantic", "-Wall", "-Wextra"};
const std::vector<std::string> DEBUG_FLAGS     = {"-ggdb", "-O0"};
const std::vector<std::string> RELEASE_FLAGS   = {"-O2", "-DNDEBUG"};
const std::vector<std::string> SANITIZER_FLAGS = {"-fsanitize=address", "-fsanitize=undefined"};

//  Tiny helpers
static bool needs_rebuild(const std::string &output, const std::vector<std::string> &inputs)
{
    if (!std::filesystem::exists(output)) {
        return true;
    }

    auto out_time = std::filesystem::last_write_time(output);
    for (const auto &inp : inputs) {
        if (std::filesystem::exists(inp) && std::filesystem::last_write_time(inp) > out_time) {
            return true;
        }
    }

    return false;
}

static std::string make_obj_path(const std::string &src, const std::string &obj_root)
{
    return bld::fs::get_stem(bld::str::replace(src, SRC, obj_root), true) + ".o";
}

static std::string object_root(bool release)
{
    return BIN + (release ? "release/" : "debug/");
}

static void add_mode_flags(bld::Command &cmd, bool release)
{
    for (const auto &f : COMMON_FLAGS) {
        cmd.add_parts(f);
    }

    if (release) {
        for (const auto &f : RELEASE_FLAGS) {
            cmd.add_parts(f);
        }
    } else {
        for (const auto &f : DEBUG_FLAGS) {
            cmd.add_parts(f);
        }
        if (static_cast<bool>(cfg["san"])) {
            for (const auto &f : SANITIZER_FLAGS) {
                cmd.add_parts(f);
            }
        }
    }
}

static std::string string_diff(const std::string &got, const std::string &expected)
{
    auto g = bld::str::chop_by_delimiter(got, "\n");
    auto e = bld::str::chop_by_delimiter(expected, "\n");
    std::ostringstream out;
    size_t n = std::max(g.size(), e.size());
    for (size_t i = 0; i < n; ++i) {
        std::string gl = (i < g.size() ? g[i] : "<missing>");
        std::string el = (i < e.size() ? e[i] : "<missing>");
        if (gl != el) {
            out << "  expected: " << el << "\n";
            out << "  got     : " << gl << "\n";
        }
    }
    return out.str();
}

//  Core build primitives
static bool
compile_parallel(const std::vector<std::string> &srcs, const std::string &obj_root, bool release, bool force, std::vector<std::string> &obj_paths)
{
    std::vector<bld::Proc> procs;

    for (const auto &src : srcs) {
        std::string out_p = make_obj_path(src, obj_root);
        obj_paths.push_back(out_p);

        std::vector<std::string> deps = {src};
        std::string hdr = bld::str::replace(src, ".cpp", ".hpp");
        if (std::filesystem::exists(hdr)) {
            deps.push_back(hdr);
        }

        if (!force && !needs_rebuild(out_p, deps)) {
            continue;
        }

        bld::fs::create_dir_if_not_exists(std::filesystem::path(out_p).parent_path().string());

        bld::Command cmd = {"g++", "-c", src, "-o", out_p};
        add_mode_flags(cmd, release);
        procs.push_back(bld::execute_async(cmd));
    }

    if (procs.empty()) {
        return false;
    }

    auto res = bld::wait_procs(procs);
    if (!res.failed_indices.empty()) {
        bld::logger::e("{} compilation job(s) failed.", res.failed_indices.size());
        std::exit(EXIT_FAILURE);
    }
    return true;
}

static bool archive(const std::vector<std::string> &obj_paths, const std::string &lib_path, bool force, bool any_obj_rebuilt)
{
    if (!force && !any_obj_rebuilt && std::filesystem::exists(lib_path)) {
        return false;
    }

    bld::logger::i("Archiving {} ...", lib_path);

    bld::Command cmd = {"ar", "rcs", lib_path};
    for (const auto &o : obj_paths) {
        cmd.add_parts(o);
    }

    if (!bld::execute(cmd)) {
        bld::logger::e("ar failed: {}", lib_path);
        std::exit(EXIT_FAILURE);
    }
    return true;
}

static bool link(const std::string &target, const std::vector<std::string> &inputs, bool release, bool force, bool any_input_rebuilt)
{
    if (!force && !any_input_rebuilt && std::filesystem::exists(target)) {
        return false;
    }

    bld::logger::i("Linking {} ...", target);

    bld::Command cmd = {"g++", "-o", target};
    for (const auto &i : inputs) {
        cmd.add_parts(i);
    }
    add_mode_flags(cmd, release);

    if (!bld::execute(cmd)) {
        bld::logger::e("Linking failed: {}", target);
        std::exit(EXIT_FAILURE);
    }
    return true;
}

//  High-level build steps
static bool build_core_library(const std::string &obj_root, bool release, bool force)
{
    auto cpp_files = bld::fs::get_all_files_with_extensions(SRC, {"cpp"}, true);

    cpp_files.erase(
        std::remove_if(cpp_files.begin(), cpp_files.end(), [](const std::string &f) { return f.find("main.cpp") != std::string::npos; }),
        cpp_files.end());

    std::vector<std::string> obj_paths;
    bool any_compiled = compile_parallel(cpp_files, obj_root, release, force, obj_paths);

    return archive(obj_paths, LIB, force, any_compiled);
}

void build_interpreter(bool release = false, bool force = false)
{
    bld::fs::create_dirs_if_not_exists(BIN, object_root(release));

    const std::string obj_root = object_root(release);

    bld::logger::i("Building interpreter [{}] ...", release ? "release" : "debug");

    bool lib_rebuilt = build_core_library(obj_root, release, force);

    const std::string main_src = SRC + "main.cpp";
    const std::string main_obj = obj_root + "main.o";

    auto hpp_files = bld::fs::get_all_files_with_extensions(SRC, {"hpp"}, true);

    std::vector<std::string> main_deps = {main_src};
    main_deps.insert(main_deps.end(), hpp_files.begin(), hpp_files.end());

    bool main_rebuilt = false;
    if (force || needs_rebuild(main_obj, main_deps)) {
        bld::Command cmd = {"g++", "-c", main_src, "-o", main_obj};
        add_mode_flags(cmd, release);
        if (!bld::execute(cmd)) {
            bld::logger::e("Failed to compile main.cpp");
            std::exit(EXIT_FAILURE);
        }
        main_rebuilt = true;
    }

    bool linked = link(TARGET, {main_obj, LIB}, release, force, lib_rebuilt || main_rebuilt);

    if (!linked && !lib_rebuilt && !main_rebuilt) {
        bld::logger::i("Everything up to date.");
    } else {
        bld::logger::i("Build complete -> {}", TARGET);
    }
}

void build_custom_interpreter(bool release = false, bool force = false)
{
    bld::fs::create_dirs_if_not_exists(BIN, object_root(release));

    std::string objs_val = cfg["objs"];
    if (objs_val.empty()) {
        bld::logger::e("No 'objs' list provided. Use -objs=file1.cpp,file2.cpp");
        std::exit(EXIT_FAILURE);
    }

    const std::string obj_root = object_root(release);

    bld::logger::i("Building custom interpreter [{}] ...", release ? "release" : "debug");

    bool lib_rebuilt = build_core_library(obj_root, release, force);

    auto requested = bld::str::chop_by_delimiter(objs_val, ",");
    std::vector<std::string> extra_srcs;
    for (auto &req : requested) {
        req = bld::str::trim(req);
        if (!req.empty()) {
            extra_srcs.push_back(SRC + req);
        }
    }

    std::vector<std::string> extra_objs;
    bool extra_rebuilt = compile_parallel(extra_srcs, obj_root, release, force, extra_objs);

    std::vector<std::string> link_inputs = extra_objs;
    link_inputs.push_back(LIB);

    bool linked = link(TARGET, link_inputs, release, force, lib_rebuilt || extra_rebuilt);

    if (!linked && !lib_rebuilt && !extra_rebuilt) {
        bld::logger::i("Everything up to date.");
    } else {
        bld::logger::i("Build complete -> {}", TARGET);
    }
}

std::tuple<std::vector<std::string>, std::vector<std::pair<std::string, std::string>>> run_tests(bool release)
{
    std::string dir = cfg["test"];
    if (dir.empty()) {
        bld::logger::e("Provide test directory with -test=<dir>");
        std::exit(EXIT_FAILURE);
    }

    if (!std::filesystem::exists(TARGET)) {
        build_interpreter(release);
    }

    auto files = bld::fs::get_all_files_with_extensions(dir, {"phos"});
    if (files.empty()) {
        bld::logger::w("No .phos test files found in: {}", dir);
        return {{}, {}};
    }

    bld::logger::i("Running {} test(s) ...", files.size());

    std::vector<std::pair<std::string, std::string>> failed;
    std::vector<std::string> passed;

    for (const auto &f : files) {
        std::string output;
        if (!bld::read_process_output(bld::Command{TARGET, f}, output)) {
            bld::logger::e("Failed to run interpreter on: {}", f);
            std::exit(EXIT_FAILURE);
        }

        const std::string expected_file = bld::fs::get_stem(f, true) + ".expected";
        std::string expected;
        if (!bld::fs::read_file(expected_file, expected)) {
            bld::logger::e("Missing expected output: {}", expected_file);
            std::exit(EXIT_FAILURE);
        }

        if (output == expected) {
            passed.push_back(f);
        } else {
            failed.push_back({f, string_diff(output, expected)});
        }
    }

    return {passed, failed};
}

bool compile_custom(const std::string &ip, const std::string &op, bool release)
{
    if (!std::filesystem::exists(LIB)) {
        bld::logger::e("Core library not found. Build the interpreter first.");
        return false;
    }

    bld::Command cmd;
    cmd.add_parts("g++", "-o", op, ip, LIB, "-lm");
    add_mode_flags(cmd, release);

    if (!bld::execute(cmd)) {
        bld::fs::remove(op);
        return false;
    }
    return true;
}

void build_meta()
{
    bld::fs::create_dirs_if_not_exists(BIN_META);

    std::vector<bld::Proc> procs;

    bld::fs::walk_directory("./meta", [&](bld::fs::Walk_fn_opt &opt) -> bool {
        if (opt.type != bld::fs::Path_type::File || opt.path.extension() != ".cpp") {
            return true;
        }

        const std::string stem = bld::fs::get_stem(opt.path.filename().string());
        const std::string out = BIN_META + stem;

        if (needs_rebuild(out, {opt.path.string()})) {
            bld::Command cmd;
            cmd.add_parts("g++");
            cmd.add_parts(opt.path.string());
            cmd.add_parts("-o", out);
            cmd.add_parts("-O2");
            cmd.append({"--std=c++26", "-pthread", "-Wpedantic", "-Wall", "-freflection"});
            procs.push_back(bld::execute_async(cmd));
        }

        return true;
    });

    if (procs.empty()) {
        bld::logger::i("Meta binaries up to date.");
        return;
    }

    auto res = bld::wait_procs(procs);
    if (!res.failed_indices.empty()) {
        bld::logger::e("Meta build failed for one or more files.");
        std::exit(EXIT_FAILURE);
    }
}

//  Entry point
int main(int argc, char *argv[])
{
    BLD_REBUILD_AND_ARGS();

    cfg.add_flag("rel", "Release build (-O2 -DNDEBUG)");
    cfg.add_flag("san", "Enable ASan/UBSan in debug builds");
    cfg.add_flag("force", "Force rebuild of all files");
    cfg.add_flag("clean", "Remove build directory");
    cfg.add_flag("meta", "Build meta-programming utilities");
    cfg.add_flag("code-gen", "Build meta-programming utilities");
    cfg.add_flag("count", "Count lines, files, etc.");

    cfg.add_option("test", "", "Run tests in given directory (e.g. -test=./tests)");
    cfg.add_option("objs", "", "Comma-separated source files for custom build");
    cfg.add_option("compile", "", "Compile a custom source file alongside the VM");
    cfg.add_option("o", "", "Output executable name for -compile");

    const bool release = static_cast<bool>(cfg["rel"]);
    const bool force   = static_cast<bool>(cfg["force"]);

    if (cfg["count"]) {
        if (std::filesystem::exists("./bin/meta/count")) {
            std::system("./bin/meta/count ./src");
            return 0;
        } else {
            build_meta();
            std::system("./bin/meta/count ./src");
            return 0;
        }
    }

    if (cfg["code-gen"]) {
        if (std::filesystem::exists("./bin/meta/basic_meta")) {
            std::system("./bin/meta/basic_meta");
            return 0;
        } else {
            build_meta();
            std::system("./bin/meta/basic_meta");
            return 0;
        }
    }

    if (cfg["h"] || cfg["help"]) {
        cfg.show_help(false);
        return 0;
    }

    if (cfg["meta"]) {
        build_meta();
    }

    if (cfg["clean"]) {
        bld::logger::i("Cleaning {} ...", BIN);
        bld::fs::remove_dir(BIN);
        bld::logger::i("Done.");
        return 0;
    }

    if (cfg["test"]) {
        auto [passed, failed] = run_tests(release);

        bld::logger::i("{} passed, {} failed.", passed.size(), failed.size());

        for (const auto &[file, diff] : failed) {
            bld::logger::e("FAIL: {}", file);
            std::cerr << diff;
        }

        return failed.empty() ? 0 : 1;
    }

    if (!std::string(cfg["objs"]).empty()) {
        build_custom_interpreter(release, force);
    } else {
        build_interpreter(release, force);
    }

    if (cfg["compile"]) {
        const std::string out = cfg["o"] ? std::string(cfg["o"]) : BIN + "new";
        if (compile_custom(cfg["compile"], out, release)) {
            bld::logger::i("Build complete -> {}", out);
        }
    }

    return 0;
}
