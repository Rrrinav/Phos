#include <cstdlib>
#include <filesystem>
#include <format>
#include <sstream>
#include <string>
#include <thread>
#include <tuple>
#include <vector>

#define B_LDR_IMPLEMENTATION
#define BLD_USE_CONFIG
#include "b_ldr.hpp"

inline auto &cfg = bld::Config::get();

const std::string SRC = "./src/";
const std::string BIN = "./bin/";
const std::string BIN_META = BIN + "meta/";
const std::string TARGET = BIN + "phos";

const std::vector<std::string> COMMON_FLAGS      = {"--std=c++23", "-pthread", "-I./src", "-Wpedantic", "-Wall", "-Wextra"};
const std::vector<std::string> DEBUG_FLAGS       = {"-ggdb", "-O0"};
const std::vector<std::string> RELEASE_FLAGS     = {"-O2", "-DNDEBUG"};
const std::vector<std::string> SANITIZER_FLAGS   = {"-fsanitize=address", "-fsanitize=undefined"};
const std::vector<std::string> META_FILES_TO_RUN = {"basic_meta.cpp"};

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

static void build_core_library(bld::Dep_graph &graph, const std::string &obj_root, bool release, bool force)
{
    auto cpp_files = bld::fs::get_all_files_with_extensions(SRC, {"cpp"}, true);
    auto hpp_files = bld::fs::get_all_files_with_extensions(SRC, {"hpp"}, true);

    std::vector<std::string> core_objs;

    for (const auto &f : cpp_files) {
        if (f.find("main.cpp") != std::string::npos) {
            continue;
        }

        std::string out_p = bld::fs::get_stem(bld::str::replace(f, SRC, obj_root), true) + ".o";
        core_objs.push_back(out_p);
        bld::fs::create_dir_if_not_exists(std::filesystem::path(out_p).parent_path().string());

        if (force && std::filesystem::exists(out_p)) {
            std::filesystem::remove(out_p);
        }

        std::vector<std::string> deps = {f};
        std::string hdr = bld::str::replace(f, ".cpp", ".hpp");
        if (std::filesystem::exists(hdr)) {
            deps.push_back(hdr);
        }

        bld::Command cmd = {"g++", "-c", f, "-o", out_p};
        add_mode_flags(cmd, release);
        graph.add_dep(bld::Dep(out_p, deps, cmd));
    }

    const std::string lib_path = BIN + "libphos.a";
    if (force && std::filesystem::exists(lib_path)) {
        std::filesystem::remove(lib_path);
    }

    bld::Command ar_cmd = {"ar", "rcs", lib_path};
    for (const auto &o : core_objs) {
        ar_cmd.add_parts(o);
    }

    graph.add_dep(bld::Dep(lib_path, core_objs, ar_cmd));
}

static void link_and_build(
    bld::Dep_graph &graph,
    const std::string &target_bin,
    const std::vector<std::string> &objs,
    bool release,
    bool force,
    const std::string &build_label)
{
    bld::Command link_cmd = {"g++", "-o", target_bin};
    for (const auto &o : objs) {
        link_cmd.add_parts(o);
    }
    add_mode_flags(link_cmd, release);

    if (force && std::filesystem::exists(target_bin)) {
        std::filesystem::remove(target_bin);
    }

    graph.add_dep(bld::Dep(target_bin, objs, link_cmd));

    bld::log(bld::Log_type::INFO, std::format("Building {} [{}] ...", build_label, release ? "release" : "debug"));

    size_t threads = cfg["threads"].as_int();
    if (threads == 0) {
        threads = std::thread::hardware_concurrency();
    }

    if (!graph.build_parallel(target_bin, threads)) {
        bld::log(bld::Log_type::ERR, build_label + " build failed.");
        std::exit(EXIT_FAILURE);
    }

    bld::log(bld::Log_type::INFO, build_label + " build complete -> " + target_bin);
}

void build_interpreter(bool release = false, bool force = false)
{
    bld::fs::create_dir_if_not_exists(BIN);
    const std::string obj_root = object_root(release);
    bld::fs::create_dir_if_not_exists(obj_root);

    bld::Dep_graph graph;

    build_core_library(graph, obj_root, release, force);
    const std::string lib_path = BIN + "libphos.a";

    std::string main_src = SRC + "main.cpp";
    std::string main_obj = obj_root + "main.o";
    bld::fs::create_dir_if_not_exists(std::filesystem::path(main_obj).parent_path().string());

    if (force && std::filesystem::exists(main_obj)) {
        std::filesystem::remove(main_obj);
    }

    bld::Command main_cmd = {"g++", "-c", main_src, "-o", main_obj};
    add_mode_flags(main_cmd, release);

    auto hpp_files = bld::fs::get_all_files_with_extensions(SRC, {"hpp"}, true);
    std::vector<std::string> main_deps = {main_src};
    main_deps.insert(main_deps.end(), hpp_files.begin(), hpp_files.end());

    graph.add_dep(bld::Dep(main_obj, main_deps, main_cmd));

    link_and_build(graph, TARGET, {main_obj, lib_path}, release, force, "standard interpreter");
}

void build_custom_interpreter(bool release = false, bool force = false)
{
    bld::fs::create_dir_if_not_exists(BIN);
    const std::string obj_root = object_root(release);
    bld::fs::create_dir_if_not_exists(obj_root);

    std::string objs_val = cfg["objs"];
    if (objs_val.empty()) {
        bld::log(bld::Log_type::ERR, "No 'objs' list provided. Use -objs=file1.cpp,file2.cpp");
        std::exit(EXIT_FAILURE);
    }

    bld::Dep_graph graph;
    build_core_library(graph, obj_root, release, force);
    const std::string lib_path = BIN + "libphos.a";

    auto requested = bld::str::chop_by_delimiter(objs_val, ",");
    std::vector<std::string> objs_to_link;

    for (auto req : requested) {
        req = bld::str::trim(req);
        std::string src_file = SRC + req;
        std::string out_p = bld::fs::get_stem(bld::str::replace(src_file, SRC, obj_root), true) + ".o";

        bld::fs::create_dir_if_not_exists(std::filesystem::path(out_p).parent_path().string());
        objs_to_link.push_back(out_p);

        if (force && std::filesystem::exists(out_p)) {
            std::filesystem::remove(out_p);
        }

        std::vector<std::string> deps = {src_file};
        std::string hdr = bld::str::replace(src_file, ".cpp", ".hpp");
        if (std::filesystem::exists(hdr)) {
            deps.push_back(hdr);
        }

        bld::Command cmd = {"g++", "-c", src_file, "-o", out_p};
        add_mode_flags(cmd, release);
        graph.add_dep(bld::Dep(out_p, deps, cmd));
    }

    objs_to_link.push_back(lib_path);

    link_and_build(graph, TARGET, objs_to_link, release, force, "custom interpreter");
}

std::tuple<std::vector<std::string>, std::vector<std::pair<std::string, std::string>>> run_tests(bool release)
{
    std::string dir = cfg["test"];
    if (dir.empty()) {
        bld::log(bld::Log_type::ERR, "Provide test directory with -test=<dir>");
        std::exit(EXIT_FAILURE);
    }

    if (!std::filesystem::exists(TARGET)) {
        build_interpreter(release);
    }

    auto files = bld::fs::get_all_files_with_extensions(dir, {"phos"});

    if (files.empty()) {
        bld::log(bld::Log_type::WARNING, "No .phos test files found in: " + dir);
        return {{}, {}};
    }

    bld::log(bld::Log_type::INFO, std::format("Running {} test(s) ...", files.size()));

    std::vector<std::pair<std::string, std::string>> failed;
    std::vector<std::string> passed;

    for (const auto &f : files) {
        std::string output{};
        bld::Command run_cmd = {TARGET, f};

        if (!bld::read_process_output(run_cmd, output)) {
            bld::log(bld::Log_type::ERR, "Failed to run interpreter on: " + f);
            std::exit(EXIT_FAILURE);
        }

        std::string expected_file = bld::fs::get_stem(f, true) + ".expected";
        std::string expected{};
        if (!bld::fs::read_file(expected_file, expected)) {
            bld::log(bld::Log_type::ERR, "Missing expected output: " + expected_file);
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

auto compile_custom(std::string ip, std::string op, bool release)
{
    const std::string lib_path = BIN + "libphos.a";

    if (!std::filesystem::exists(lib_path)) {
        bld::log(bld::Log_type::ERR, "Core library not found. Build the interpreter first.");
        return false;
    }

    bld::fs::write_entire_file(op, "");

    bld::Command cmd;
    cmd.add_parts("g++", "-o", op, ip, lib_path, "-lm");
    add_mode_flags(cmd, release);

    if (!bld::execute(cmd)) {
        bld::fs::remove(op);
        return false;
    }
    return true;
}

auto build_meta()
{
    bld::fs::create_dirs_if_not_exists(BIN_META);
    std::vector<bld::Proc> procs;

    bld::fs::walk_directory("./meta", [&](bld::fs::Walk_fn_opt &opt) -> bool {
        bld::Command cmd{};
        cmd.add_parts("g++");
        cmd.add_parts(opt.path.string());
        cmd.add_parts("-o", BIN_META + bld::fs::get_stem(opt.path.filename().string()));
        cmd.add_parts("-O2");
        cmd.append({"--std=c++26", "-pthread", "-Wpedantic", "-Wall", "-freflection"});

        procs.push_back(bld::execute_async(cmd));
        return true;
    });

    auto result = bld::wait_procs(procs, true);
    if (!result.failed_indices.empty()) {
        bld::log(bld::Log_type::ERR, "Meta build failed for one or more files.");
        std::exit(EXIT_FAILURE);
    }
}

int main(int argc, char *argv[])
{
    BLD_REBUILD_AND_ARGS();

    cfg.add_flag("rel", "Release build (-O2 -DNDEBUG)");
    cfg.add_flag("san", "Enable ASan/UBSan in debug builds");
    cfg.add_flag("force", "Force rebuild of all files");
    cfg.add_flag("clean", "Remove build directory");
    cfg.add_flag("count", "count number of lines, files, etc.");

    cfg.add_option("test", "", "Run tests in given directory (e.g. -test=./tests)");
    cfg.add_option("objs", "", "Comma-separated source files for custom build");
    cfg.add_option("compile", "", "Compile a custom source file alongside the VM");
    cfg.add_option("o", "", "Output executable name for custom compile");

    bool release = static_cast<bool>(cfg["rel"]);
    bool force = static_cast<bool>(cfg["force"]);

    if (cfg["h"] || cfg["help"]) {
        cfg.show_help(true);
        return 0;
    }

    if (cfg["meta"]) {
        build_meta();
    }

    if (cfg["clean"]) {
        bld::log(bld::Log_type::INFO, "Cleaning " + BIN);
        bld::fs::remove_dir(BIN);
        bld::log(bld::Log_type::INFO, "Done.");
    } else if (cfg["test"]) {
        auto [passed, failed] = run_tests(release);
        return failed.empty() ? 0 : 1;
    } else if (!std::string(cfg["objs"]).empty()) {
        build_custom_interpreter(release, force);
    } else {
        build_interpreter(release, force);
    }

    if (cfg["compile"]) {
        std::string file = cfg["o"] ? std::string(cfg["o"]) : "./bin/new";
        compile_custom(cfg["compile"], file, release);
        bld::log(bld::Log_type::INFO, "Build complete -> " + file);
    }

    return 0;
}
