/*
  Copyright Dec 2025, Rinav (github: rrrinav)

  Permission is hereby granted, free of charge,
  to any person obtaining a copy of this software and associated documentation files(the “Software”),
  to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute,
  sublicense, and / or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
  subject to the following conditions :

  The above copyright notice and this permission notice shall be included in all copies
  or
  substantial portions of the Software.

  THE SOFTWARE IS PROVIDED “AS IS”,
  WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
  DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/*
  INFO: DEFINES:
  01. B_LDR_IMPLEMENTATION          : Include all the implementation in the header file.
  02. BLD_REBUILD_YOURSELF_ONCHANGE : Rebuild the build executable if the source file is newer than the executable and run it.
  03. BLD_HANDLE_ARGS               : Handle command-line arguments (only run and config commands).
  04. BLD_REBUILD_AND_ARGS          : Rebuild the executable if the source file is newer than the executable and handle command-line arguments.
  05. BLD_NO_COLORS                 : Disable colors in log messages.
  06. BLD_USE_CONFIG                : Enable the configuration system in the build tool.
  07. BLD_DEFAULT_CONFIG_FILE       : File to save the configuration to.
  08. BLD_NO_LOGGING                : No logging in the build tool.
  09. BLD_VERBOSE_0                 : No output in the tool.
  10. BLD_VERBOSE_1                 : No verbose output in the tool. Only prints errors. No INFO or WARNING messages.
  11. BLD_VERBOSE_2                 : Only prints errors and warning. No INFO messages.
    Verbosity is full by default.
*/

#pragma once

#ifdef _WIN32
  #define WIN32_LEAN_AND_MEAN
  #define _WINUSER_
  #define _WINGDI_
  #define _IMM_
  #define _WINCON_
  #include <windows.h>
  #include <direct.h>
  #include <shellapi.h>
#else
  #include <sys/types.h>
  #include <sys/utsname.h>
  #include <sys/wait.h>
  #include <unistd.h>
  #include <fcntl.h>
#endif

#include <condition_variable>
#include <chrono>
#include <cstddef>
#include <cstring>
#include <mutex>
#include <queue>
#include <string>
#include <thread>
#include <unordered_map>
#include <unordered_set>
#include <vector>

/* @brief: Rebuild the build executable if the source file is newer than the executable and run it
 * @description: Takes no parameters and calls bld::rebuild_yourself_onchange_and_run() with the current file and executable.
 *    bld::rebuild_yourself_onchange() detects compiler itself (g++, clang++, cl supported) and rebuilds the executable
 *    if the source file is newer than the executable. It then restarts the new executable and exits the current process.
 * If you want more cntrol, use bld::rebuild_yourself_onchange() or bld::rebuild_yourself_onchange_and_run() directly.
 */
#define BLD_REBUILD_YOURSELF_ONCHANGE() bld::rebuild_yourself_onchange_and_run(__FILE__, argv[0])
// Same but with compiler specified...
#define C_BLD_REBUILD_YOURSELF_ONCHANGE(compiler) bld::rebuild_yourself_onchange_and_run(__FILE__, argv[0], compiler)

/* @brief: Handle command-line arguments
 * @description: Takes argc and argv and calls bld::handle_args() with them.
 *    Right now, it only handles 'run' and 'config' commands.
 *    Read the documentation of function for more information.
 */
#define BLD_HANDLE_ARGS() bld::handle_args(argc, argv)

#define BLD_REBUILD_AND_ARGS()     \
  BLD_REBUILD_YOURSELF_ONCHANGE(); \
  BLD_HANDLE_ARGS()

/* File to save the configuration to.
 * You can change this to any file name you want.
 */
#define BLD_DEFAULT_CONFIG_FILE "build.conf"

namespace bld
{
  // Log type is enumeration for bld::function to show type of loc>
  enum class Log_type { INFO, WARNING, ERR, DEBUG };

  // Process execution result
  #ifdef _WIN32
    using pid = DWORD;
  #else
    using pid = pid_t;
  #endif
  // Abstraction for process for cross platform API.
    enum class State
    {
      INIT_ERROR,  // Failed to start
      RUNNING,     // Currently executing
      EXITED,      // Exited normally
      SIGNALLED,   // Terminated by signal (Unix only)
      WAIT_ERROR   // Error during wait operation
    };

    // Single process handle
    struct Proc
    {
      bool ok       = false;
      pid p_id      = 0;
      State state   = State::INIT_ERROR;
      int exit_code = 0;

    #ifndef _WIN32
      int signal = 0;
    #else
      HANDLE process_handle = nullptr;
      HANDLE thread_handle  = nullptr;
    #endif

      // Constructors
      Proc() = default;

      explicit Proc(pid_t p) : p_id(p)
      {
        if (p > 0)
        {
          ok = true;
          state = State::RUNNING;
        }
      }

    #ifdef _WIN32
      Proc(HANDLE proc_handle, HANDLE thread_handle, pid_t pid) : pid(pid), process_handle(proc_handle), thread_handle(thread_handle)
      {
        if (proc_handle != nullptr)
        {
          ok = true;
          state = State::RUNNING;
        }
        else
        {
          ok = false;
          state = State::INIT_ERROR;
        }
      }
    #endif
      // Convenience operators
      explicit operator bool() const { return ok; }
      bool operator!() const { return !ok; }

      // State queries
      bool is_running() const { return ok && state == State::RUNNING; }
      bool has_exited() const { return state == State::EXITED || state == State::SIGNALLED; }
      bool succeeded() const { return state == State::EXITED && exit_code == 0; }
      bool is_valid() const { return ok; }
    };

    // Process info after exit
    struct Exit_status
    {
      bool normal;
      int exit_code;
    #ifndef _WIN32
      int signal;
    #endif

      operator bool() const { return normal && exit_code == 0; }
      bool operator!() const { return !normal || exit_code != 0; }
    };

    // Process info after non blocking wait.
    struct Wait_status
    {
      bool normal = false;
      int exit_code = 0;
      bool exited = false;
      bool invalid_proc = false;
      bool waitpid_failed = false;
    #ifndef _WIN32
      int signal;
    #endif

      operator bool() const { return normal && exit_code == 0; }
      bool operator!() const { return !normal || exit_code != 0; }
    };

    // Return type for parallel execution
    struct Par_exec_res
    {
      size_t completed;                        // Number of successfully completed commands/procs
      std::vector<size_t> failed_indices;      // Indices of commands/procs that failed
      std::vector<Exit_status> exit_statuses;  // Exit statuses of procs/commands in order.

      Par_exec_res() : completed(0) {}
    };
  // File descriptor/handle type
  #ifdef _WIN32
    using Fd = HANDLE;
    const Fd INVALID_FD = INVALID_HANDLE_VALUE;
  #else
    using Fd = int;
    const Fd INVALID_FD = -1;
  #endif

  // Redirection configuration
  struct Redirect
  {
    Fd stdin_fd  = INVALID_FD;   // Redirect stdin from this fd
    Fd stdout_fd = INVALID_FD;  // Redirect stdout to this fd
    Fd stderr_fd = INVALID_FD;  // Redirect stderr to this fd

    Redirect() = default;
    Redirect(Fd in, Fd out, Fd err) : stdin_fd(in), stdout_fd(out), stderr_fd(err) {}
    Redirect(const std::string& in = "", const std::string& out = "", const std::string& err = "");
    Redirect(const Redirect&) = delete;
    Redirect& operator=(const Redirect&) = delete;

    static Redirect in(const std::string &_path)  { return Redirect(_path, "", ""); }
    static Redirect out(const std::string &_path) { return Redirect("", _path, ""); }
    static Redirect err(const std::string &_path) { return Redirect("", "", _path); }
    ~Redirect();
  };

  // Logging function used by library
  void internal_log(Log_type type, const std::string &msg);

  // Logging function for users
  void log(bld::Log_type type, const std::string &msg);

  // Struct to hold command parts
  struct Command
  {
    std::vector<std::string> parts;  // > parts of the command

    Command() : parts{} {}
    // @tparam args ( variadic template ): Command parts
    template <typename... Args>
    Command(Args... args);
    template <typename... Args>
    void add_parts(Args... args);
    // Get the full command as a single string
    std::string get_command_string() const;
    // Get the full command as a C-style arguments for sys calls
    std::vector<char *> to_exec_args() const;
    bool is_empty() const { return parts.empty(); }
    // Get the command as a printable string wrapped in single quotes
    std::string get_print_string() const;
    void clear() { parts.clear(); }
  };

  // Class to save configuration
  class Config
  {
  private:
    Config() = default;
    Config(const Config &) = delete;
    Config &operator=(const Config &) = delete;

  public:

  #ifdef __clang__
    std::string compiler = "clang++";
  #elif defined(__GNUC__)
    std::string compiler = "g++";
  #elif defined(_MSC_VER)
    std::string compiler = "cl";
  #else
    std::string compiler = "g++";
  #endif
    std::string target         = "main";
    std::string build_dir      = "./build";
    std::string compiler_flags = "-O2";
    std::string linker_flags   = "";
    std::string pre_build      = "";
    std::string post_build     = "";

    bool verbose      = false;
    bool hot_reload   = false;
    bool override_run = false;
    size_t threads    = 1;

    std::vector<std::string> hot_reload_files;
    std::vector<std::string> cmd_args;

  private:
    // Simple storage - strings and flags
    mutable std::unordered_map<std::string, std::string> values;
    mutable std::unordered_set<std::string> flags;

    // Custom config definitions
    struct CustomConfig
    {
      std::string default_value;
      std::string description;
      bool is_flag;  // true for flags, false for key=value
    };
    std::unordered_map<std::string, CustomConfig> custom_configs;

  public:
    static Config &get();
    // Define custom config options
    void add_flag(const std::string &name, const std::string &description = "");
    void add_option(const std::string &name, const std::string &default_value = "", const std::string &description = "");

    // Get all custom configs (for help display)
    const std::unordered_map<std::string, CustomConfig> &get_custom_configs() const;

    // Magic config access
    class Config_proxy
    {
    private:
      std::string key;
      Config *config;

    public:
      Config_proxy(const std::string &k, Config *c);

      // Set value
      Config_proxy &operator=(const std::string &value);
      Config_proxy &operator=(const char *value);

      // Get as string
      operator std::string() const;

      // Check if flag is set or value equals something
      bool operator==(const std::string &other) const;
      bool operator==(const char *other) const;

      // Convert to bool - true if flag is set or value is truthy
      operator bool() const;

      // Check if this key exists at all
      bool exists() const;

      // Get as int (0 if not a number)
      int as_int() const;
    };

    Config_proxy operator[](const std::string &key);
    const Config_proxy operator[](const std::string &key) const;

    void initialize_builtin_options();
    // Argument parsing
    void parse_args(const std::vector<std::string> &args);

    // Help and documentation
    void show_help() const;

    // Debug: show all stored config
    void dump() const;

    // File operations
    bool load_from_file(const std::string &filename = "build.conf");
    bool save_to_file(const std::string &filename = "build.conf") const;

  private:
    bool is_true(const std::string &value) const;
    bool is_number(const std::string &value) const;
    void parse_file_list(const std::string &value);
  };

  void handle_config_command(const std::vector<std::string>& args, const std::string& program_name);
  void handle_args(int argc, char* argv[]);
  /* @brief: Convert command-line arguments to vector of strings
   * @param argc ( int ): Number of arguments
   * @param argv ( char*[] ): Array of arguments
   */
  bool args_to_vec(int argc, char *argv[], std::vector<std::string> &args);

  /* @beief: Validate the command before executing
   * @param command ( Command ): Command to validate
   */
  bool validate_command(const Command &command);

  /* @brief: Wait for the process to complete
   * @param pid ( pid_t ): Process ID to wait for
   * @return: returns a code to indicate success or failure
   *   >1 : Command executed successfully, returns pid of fork.
   *    0 : Command failed to execute or something wrong on system side
   *   -1 : No command to execute or something wrong on user side
   * @description: Wait for the process to complete and log the status. Use this function instead of direct waitpid
   */
  Exit_status wait_proc(Proc proc);

  /* @breif: Clean up a process after completed
   * @param proc: Takes a Proc to cleanup.
   * @description: Closes handle and sets other values in windows and sets pid = -1 in linux.
   *               Additionaly sets res = USER_ERROR
   */
  void cleanup_process(Proc &proc);

  /* @breif: Checks if provided process is running or not
   * @param proc: Process to check
   * @return bool: return value
   */
  Wait_status try_wait_nb(const Proc &proc);

  /*
   * @brief: Waits on multiple async processes
   * @params pids (vector<pid_t>): process ids to wait on
   * @param sleep_ms (int): Time to sleep to avoid busy looping.
   * @return Exec_par_result: process ids that failed
   */
  Par_exec_res wait_procs(std::vector<Proc> procs, int sleep_ms = 50);

  /* @brief: Execute the command
   * @param command ( Command ): Command to execute, must be a valid process command and not shell command
   * @return: returns a code to indicate success or failure
   *   >1 : Command executed successfully, returns pid of fork.
   *    0 : Command failed to execute or something wrong on system side
   *   -1 : No command to execute or something wrong on user side
   * @description: Execute the command using fork and log the status alongwith
   */
  Exit_status execute(const Command &command);

  /* @brief: Redirect output of a child process to a file descriptor
   * @param Command: command which will be executed with redirected output
   * @redirect: File descriptors to redirect outputs to.
   */
  Exit_status execute_redirect(const Command& command, const Redirect& redirect);

  /* @brief: Execute the command asynchronously (without waiting)
   * @param command ( Command ): Command to execute, must be a valid process command and not shell command
   * @return: returns a code to indicate success or failure
   *   >0 : Command executed successfully, returns pid of fork.
   *    0 : Command failed to execute or something wrong on system side
   *   -1 : No command to execute or something wrong on user side
   * @description: Execute the command using fork and log the status alongwith
   */
  Proc execute_async(const Command &command);

  /* @brief: like execute_redirect but without wait.
   * @param Command: command which will be executed with redirected output
   * @redirect: File descriptors to redirect outputs to.
   */
  Proc execute_async_redirect(const Command& command, const Redirect& redirect);

  /* @brief: Open a file descriptor to write to
   * @param: Path to the file
   * @return: File descriptor opened
   * @description: These can be used in Redirect struct to be used in redirecting functions
   */
  Fd open_for_read(const std::string &path);

  /* @brief: Open a file descriptor to read from
   * @param: Path to the file
   * @return: File descriptor opened
   * @description: These can be used in Redirect struct to be used in redirecting functions
   */
  Fd open_for_write(const std::string &path, bool append = false);

  /* @brief: Close file descriptors
   * @param Fd: File descriptors
   */
  template <typename... Fds, typename = std::enable_if_t<(std::is_integral_v<Fds> && ...)>>
  void close_fd(Fds ...fds);


  /* @brief: Execute multiple commands on multiple threads.
   * @param cmds: Vector of commands to execute
   * @param threads: Number of parallel threads (default: hardware concurrency - 1). Change if you want.
   * @param strict: If true, stop all threads if an error occurs even in one command.
   * @return: Exec_par_result
   */
  Par_exec_res execute_threads(const std::vector<bld::Command> &cmds, size_t threads = (std::thread::hardware_concurrency() - 1),
                                       bool strict = true);

  /* @description: Print system metadata:
   *  1. Operating System
   *  2. Compiler
   *  3. Architecture
   */
  void print_metadata();

  // Get no of cores, logical or physical
  int get_n_procs(bool physical_cores_only = false);

  /* @brief: Execute the shell command with preprocessed parts
   * @param cmd ( string ): Command to execute in shell
   * @description: runs std::system
   * @return: the return value of the std::system
   */
  int execute_shell(std::string command);

  /* @brief: Execute the shell command with preprocessed parts but asks wether to execute or not first
   * @param cmd ( Command ): Command to execute in shell
   * @param prompt ( bool ): Ask for confirmation before executing
   * @description: Execute the shell command with preprocessed parts with prompting
   *    Uses execute function to execute the preprocessed command
   *    return the return value of the execute function
   */
  int execute_shell(std::string command, bool prompt);

  /* @brief: Read output from a process command execution
   * @param cmd ( Command ): Command struct containing the process command and arguments
   * @param output ( std::string& ): Reference to string where output will be stored
   * @param buffer_size ( size_t ): Size of buffer for reading output (default: 4096)
   * @return ( bool ): true if successful, false otherwise
   */
  bool read_process_output(const Command &cmd, std::string &output, size_t buffer_size = 4096);

  /* @brief: Read output from a shell command execution
   * @param shell_cmd ( std::string ): Shell command to execute and read output from
   * @param output ( std::string& ): Reference to string where output will be stored
   * @param buffer_size ( size_t ): Size of buffer for reading output (default: 4096)
   * @return ( bool ): true if successful, false otherwise
   */
  bool read_shell_output(const std::string &shell_cmd, std::string &output);

  /* @brief: Check if the executable is outdated i.e. source file is newer than the executable
   * @param file_name ( std::string ): Source file name
   * @param executable ( std::string ): Executable file name
   * @description: Check if the source file is newer than the executable. Uses last write time to compare.
   *  Can be used for anything realistically, just enter paths and it will return.
   *  it basically returns ( modify_time(file) > modify_time(executable) ) irrespective of file type
   */
  bool is_executable_outdated(std::string file_name, std::string executable);

  //TODO: Make rebuild and run work on windows

  /* @brief: Rebuild the executable if the source file is newer than the executable and runs it
   * @param filename ( std::string ): Source file name
   * @param executable ( std::string ): Executable file name
   * @param compiler ( std::string ): Compiler command to use (default: "")
   *  It can detect compiler itself if not provided
   *  Supported compilers: g++, clang++, cl
   *  @description: Generally used for actual build script
   */
  void rebuild_yourself_onchange_and_run(const std::string &filename, const std::string &executable, std::string compiler = "");

  /* @brief: Rebuild the executable if the source file is newer than the executable
   * @param filename ( std::string ): Source file name (C++ only)
   * @param executable ( std::string ): Executable file name
   * @param compiler ( std::string ): Compiler command to use (default: "")
   *  It can detect compiler itself if not provided
   *  Supported compilers: g++, clang++, cl
   *  @description: Actually for general use and can be used to rebuild any *C++* file since it doesn't restart the process
   */
  void rebuild_yourself_onchange(const std::string &filename, const std::string &executable, std::string compiler);

  /* @brief: Handle the 'run' command
   * @param args ( std::vector<std::string> ): Arguments for the command
   * @description: Handle the 'run' command. If no arguments are provided, it runs the target executable from config.
   */
  int handle_run_command(std::vector<std::string> args);

  /* @brief: Check if a string starts with a prefix
   * @param str ( std::string ): String to check
   * @param prefix ( std::string ): Prefix to check
   * @description: If size of prefix is greater than size of string, returns false
   *  Uses std::string::compare() to compare the prefix with the string
   */
  bool starts_with(const std::string &str, const std::string &prefix);

  namespace time
  {
    struct stamp
    {
      using clock = std::chrono::steady_clock;
      clock::time_point tp;

      stamp() : tp(clock::now()) {}
      explicit stamp(clock::time_point t) : tp(t) {}

      static stamp now() { return stamp(clock::now()); }

      void reset() { this->tp = clock::now(); }

      template <class Rep = double, class Unit = std::chrono::milliseconds, typename =
        std::enable_if_t<std::is_arithmetic_v<Rep> &&
        std::is_base_of_v<std::chrono::duration<typename Unit::rep, typename Unit::period>, Unit>>>
      Rep time_spent(const stamp &later = stamp::now())
      {
        return std::chrono::duration_cast<Unit>(later.tp - this->tp).count();
      }
    };

    template <class Rep = double, class Unit = std::chrono::milliseconds, typename =
      std::enable_if_t<std::is_arithmetic_v<Rep> &&
      std::is_base_of_v<std::chrono::duration<typename Unit::rep, typename Unit::period>, Unit>>>
    Rep since(const stamp &earlier, const stamp &later = stamp::now())
    {
      return std::chrono::duration_cast<Unit>(later.tp - earlier.tp).count();
    }
  };  // namespace time

  namespace fs
  {
    /* @brief: Read entire file content into a string
     * @param path: Path to the file
     * @param content: Reference to string where content will be stored
     * @return: True if successful, false otherwise
     */
    bool read_file(const std::string &path, std::string &content);

    /* @brief: Write string content to a file
     * @param path: Path to the file
     * @param content: Content to write
     * @return: true if successful, false otherwise
     */
    bool write_entire_file(const std::string &path, const std::string &content);

    /* @brief: Append string content to a file
     * @param path: Path to the file
     * @param content: Content to append
     * @return: true if successful, false otherwise
     */
    bool append_file(const std::string &path, const std::string &content);

    /* @brief: Read file line by line, calling a function for each line
     * @param path: Path to the file
     * @param func: Function to call for each line
     * @return: true if successful, false otherwise
     */
    bool read_lines(const std::string &path, std::vector<std::string> &lines);

    /* @brief: Replace text in file
     * @param path: Path to the file
     * @param from: Text to replace
     * @param to: Text to replace with
     * @return: true if successful, false otherwise
     */
    bool replace_in_file(const std::string &path, const std::string &from, const std::string &to);

    /* @brief: Copy a file
     * @param from: Source path
     * @param to: Destination path
     * @param overwrite = true: Whether to overwrite if destination exists
     * @return: true if successful, false otherwise
     */
    bool copy_file(const std::string &from, const std::string &to, bool overwrite = false);

    /* @brief: Move/Rename a file
     * @param from: Source path
     * @param to: Destination path
     * @return: true if successful, false otherwise
     */
    bool move_file(const std::string &from, const std::string &to);

    /* @brief: Get file extension
     * @param path: Path to the file
     * @return: File extension including the dot, or empty string if none
     */
    std::string get_extension(const std::string &path);

    /* @brief: Get file name without extension
     * @param path: Path to the file
     * @param with_full_path: true to include full path, false to get only the file name
     * @return: Get file name without extension, full string if no extension
     */
    std::string get_stem(const std::string &path, bool with_full_path = false);

    /* @brief: Create directory and all parent directories if they don't exist
     * @param path: Path to create
     * @return: true if successful, false otherwise
     */
    bool create_directory(const std::string &path);

    /* @brief: Create directory and all parent directories if they don't exist
     * @param path: Path to create
     * @return: true if successful, false otherwise
     */
    bool create_dir_if_not_exists(const std::string &path);

    /* @brief: Create multiple directories and all parent directories if they don't exist
     * @param paths: Paths to create
     * @return: true if successful, false otherwise
     */
    template <typename... Paths, typename = std::enable_if_t<(std::is_convertible_v<Paths, std::string> && ...)>>
    bool create_dirs_if_not_exists(const Paths &...paths);

    /* @brief: Remove directory and all its contents if it exists
     * @param path: Path to remove
     * @return: true if successful, false otherwise
     */
    bool remove_dir(const std::string &path);

    // Remove files variadic
    template <typename... Paths, typename = std::enable_if_t<(std::is_convertible_v<Paths, std::string> && ...)>>
    void remove(const Paths &...paths);

    /* @brief: Get list of all files in directory
     * @param path: Directory path
     * @param recursive = true: Whether to include files in subdirectories
     * @return: Vector of file paths
     */
    std::vector<std::string> list_files_in_dir(const std::string &path, bool recursive = false);

    /* @brief: Get list of all directories in directory
     * @param path: Directory path
     * @param recursive = true: Whether to include subdirectories of subdirectories
     * @return: Vector of directory paths
     */
    std::vector<std::string> list_directories(const std::string &path, bool recursive = false);

    /* @brief: Get the file name from a full path
     * @param full_path: Full path to the file
     * @return: File name
     */
    std::string get_file_name(std::string full_path);

    /* @brief: Get the directory path from a full path
     * @param full_path: Full path to the file
     * @return: Directory path
     */
    std::string strip_file_name(std::string full_path);

    /* @breif: get all files with specific extensions in a directory
     * @param string: path to the directory
     * @param vector<string>: vector of extrensions to look for.
     * @param bool (false): whether to look into a directory recursively
     * @param bool (false): whether the matching must be case insensitive or not
     */
    std::vector<std::string> get_all_files_with_extensions(const std::string &path,const std::vector<std::string> &extensions,
                                                           bool recursive = false, bool case_insensitive = false);

    std::vector<std::string> get_all_files_with_name(const std::string &dir, const std::string &name, bool recursive = false);
  }  // namespace fs

  namespace env
  {
    /* @brief: Get the value of an environment variable
     * @param name: Name of the environment variable
     * @return: Value of the environment variable
     *          empty string if not found
     */
    std::string get(const std::string &key);

    /* @brief: Set the value of an environment variable
     * @param name: Name of the environment variable
     * @return: true if successful, false otherwise
     */
    bool set(const std::string &key, const std::string &value);

    /* @brief: Check if an environment variable exists
     * @param name: Name of the environment variable
     * @return: true if exists, false otherwise
     */
    bool exists(const std::string &key);

    /* @brief: Unset the value of an environment variable
     * @param name: Name of the environment variable
     * @return: true if successful, false otherwise
     */
    bool unset(const std::string &key);

    /* @brief: Get all environment variables
     * @return: map of all environment variables
     */
    std::unordered_map<std::string, std::string> get_all();
  }  // namespace env

  namespace str
  {
    inline std::string trim(const std::string &str); // remove leading and trailing whitespace from a string including: ' ', '\t', '\n', '\r', '\f', '\v'
    std::string trim_left(const std::string &str);
    std::string trim_right(const std::string &str);

    inline std::string to_lower(const std::string &str);
    inline std::string to_upper(const std::string &str);
    bool starts_with(const std::string &str, const std::string &prefix);
    bool ends_with(const std::string &str, const std::string &suffix);

    std::string join(const std::vector<std::string> &strs, const std::string &delimiter);
    std::string trim_till(const std::string &str, char delimiter);
    std::vector<std::string> chop_by_delimiter(const std::string &s, const std::string &delimiter);

    std::string remove_duplicates(const std::string &str);
    std::string remove_duplicates_case_insensitive(const std::string &str);

    bool equal_ignorecase(const std::string &str1, const std::string &str2);
    bool is_numeric(const std::string &str);

    inline std::string replace(std::string str, const std::string &from, const std::string &to);
    std::string replace_all(const std::string &str, const std::string &from, const std::string &to);
  }  // namespace str

  struct Dep
  {
    std::string target;                     // Target/output file
    std::vector<std::string> dependencies;  // Input files/dependencies
    bld::Command command;                   // Command to build the target
    bool is_phony{false};                   // Whether this is a phony target

    // Default constructor
    Dep() = default;

    // Constructor for non-phony targets
    Dep(std::string target, std::vector<std::string> dependencies = {}, bld::Command command = {});

    // Constructor for phony targets
    Dep(std::string target, std::vector<std::string> dependencies, bool is_phony);

    // Implicit conversion from initializer list for better usability
    Dep(std::string target, std::initializer_list<std::string> deps, bld::Command command = {});

    // Copy constructor
    Dep(const Dep &other);

    // Move constructor
    Dep(Dep &&other) noexcept;

    // Copy assignment
    Dep &operator=(const Dep &other);

    // Move assignment
    Dep &operator=(Dep &&other) noexcept;
  };

  class Dep_graph
  {
    struct Node
    {
      Dep dep;
      std::vector<std::string> dependencies;  // Names of dependent nodes
      bool visited{false};
      bool in_progress{false};  // For cycle detection
      bool checked{false};
      std::vector<std::string> waiting_on;  // files that need to be built before this one

      Node(const Dep &d) : dep(d) {}
    };
    std::unordered_map<std::string, std::unique_ptr<Node>> nodes;
    std::unordered_set<std::string> checked_sources;

public:
    /* @brief Add a dependency to the graph.
     * @param dep The dependency to add.
     */
    void add_dep(const Dep &dep);

    /* @brief Add a phony target to the graph.
     * @param target The name of the phony target.
     * @param deps The dependencies of the phony target.
     */
    void add_phony(const std::string &target, const std::vector<std::string> &deps);

    /* @brief Check if a node needs to be rebuilt.
     * @param node The node to check.
     * @return true If the node needs to be rebuilt.
     * @return false If the node does not need to be rebuilt.
     */
    bool needs_rebuild(const Node *node);

    /* @brief Build the target.
     * @param target The name of the target to build.
     * @return true If the build was successful.
     * @return false If the build failed.
     */
    bool build(const std::string &target);

    /* @brief Build the dependency.
     * @param dep The dependency to build.
     * @return true If the build was successful.
     * @return false If the build failed.
     */
    bool build(const Dep &dep);

    /* @brief Build all targets in the graph.
     * @return true If all builds were successful.
     * @return false If any build failed.
     */
    bool build_all();

    /* @brief Build all targets in the graph but all previous checks are discarded (alternative method).
     * @return true If all builds were successful.
     * @return false If any build failed.
     */
    bool F_build_all();

    bool build_parallel(const std::string &target, size_t thread_count = std::thread::hardware_concurrency());
    bool build_all_parallel(size_t thread_count = std::thread::hardware_concurrency());

private:
    /* @brief Build a node in the graph.
     * @param target The name of the target to build.
     * @return true If the build was successful.
     * @return false If the build failed.
     */
    bool build_node(const std::string &target);

    /* @brief Detect cycles in the graph.
     * @param target The name of the target to check.
     * @param visited The set of visited nodes.
     * @param in_progress The set of nodes currently being processed.
     * @return true If a cycle was detected.
     * @return false If no cycle was detected.
     */
    bool detect_cycle(const std::string &target, std::unordered_set<std::string> &visited, std::unordered_set<std::string> &in_progress);

    /* @brief Prepare graph for parallel build
     * @param target The target to build
     * @param ready_targets The queue of targets
     */
    bool prepare_build_graph(const std::string &target, std::queue<std::string> &ready_targets);

    /* @brief Process completed target
     * @param target The target that was completed
     * @param ready_targets The queue of targets
     * @param queue_mutex The mutex for the queue
     * @param cv The condition variable
     */
    void process_completed_target(const std::string &target, std::queue<std::string> &ready_targets, std::mutex &queue_mutex,
                                  std::condition_variable &cv);
  };
}  // namespace bld

#ifdef B_LDR_IMPLEMENTATION

#include <cstdint>
#include <utility>
#include <algorithm>
#include <atomic>
#include <cstdlib>
#include <unordered_map>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <mutex>
#include <ostream>
#include <queue>
#include <sstream>
#include <utility>

void bld::internal_log(bld::Log_type type, const std::string &msg)
{
#ifdef BLD_NO_LOGGING
  return;
#endif

#ifdef BLD_NO_COLORS
  static constexpr const char *COLOUR_INFO = "";
  static constexpr const char *COLOUR_WARN = "";
  static constexpr const char *COLOUR_ERROR = "";
  static constexpr const char *COLOUR_DEBUG = "";
  static constexpr const char *COLOUR_RESET = "";
#else
  static constexpr const char *COLOUR_INFO = "\x1b[38;2;80;250;123m";    // mint green
  static constexpr const char *COLOUR_WARN = "\x1b[38;2;255;200;87m";    // amber
  static constexpr const char *COLOUR_ERROR = "\x1b[38;2;255;85;85m";    // red
  static constexpr const char *COLOUR_DEBUG = "\x1b[38;2;130;170;255m";  // light blue
  static constexpr const char *COLOUR_RESET = "\x1b[0m";
#endif

  switch (type)
  {
    case Log_type::INFO:
#ifndef BLD_VERBOSE_1
#ifndef BLD_VERBOSE_2
      std::cerr << COLOUR_INFO << "[INFO]: " << COLOUR_RESET << msg << std::endl;
      break;
#endif
#endif
      break;

    case Log_type::WARNING:
#ifndef BLD_VERBOSE_1
      std::cerr << COLOUR_WARN << "[WARNING]: " << COLOUR_RESET << msg << std::endl;
      std::cerr.flush();
      break;
#endif
      break;

    case Log_type::ERR:
      std::cerr << COLOUR_ERROR << "[ERROR]: " << COLOUR_RESET << msg << std::endl;
      std::cerr.flush();
      break;

    case Log_type::DEBUG:
      std::cerr << COLOUR_DEBUG << "[DEBUG]: " << COLOUR_RESET << msg << std::endl;
      break;

    default:
      std::cerr << "[UNKNOWN]: " << msg << std::endl;
      break;
  }
}

void bld::log(bld::Log_type type, const std::string &msg)
{
#ifdef BLD_NO_COLORS
  static constexpr const char *COLOUR_INFO = "";
  static constexpr const char *COLOUR_WARN = "";
  static constexpr const char *COLOUR_ERROR = "";
  static constexpr const char *COLOUR_DEBUG = "";
  static constexpr const char *COLOUR_RESET = "";
#else
  static constexpr const char *COLOUR_INFO = "\x1b[38;2;80;250;123m";    // mint green
  static constexpr const char *COLOUR_WARN = "\x1b[38;2;255;200;87m";    // amber
  static constexpr const char *COLOUR_ERROR = "\x1b[38;2;255;85;85m";    // red
  static constexpr const char *COLOUR_DEBUG = "\x1b[38;2;130;170;255m";  // light blue
  static constexpr const char *COLOUR_RESET = "\x1b[0m";
#endif

  switch (type)
  {
    case Log_type::INFO:
      std::cerr << COLOUR_INFO << "[INFO]: " << COLOUR_RESET << msg << std::endl;
      break;

    case Log_type::WARNING:
      std::cerr << COLOUR_WARN << "[WARNING]: " << COLOUR_RESET << msg << std::endl;
      std::cerr.flush();
      break;

    case Log_type::ERR:
      std::cerr << COLOUR_ERROR << "[ERROR]: " << COLOUR_RESET << msg << std::endl;
      std::cerr.flush();
      break;

    case Log_type::DEBUG:
      std::cerr << COLOUR_DEBUG << "[DEBUG]: " << COLOUR_RESET << msg << std::endl;
      break;

    default:
      std::cerr << "[UNKNOWN]: " << msg << std::endl;
      break;
  }
}

// Get the full command as a single string
std::string bld::Command::get_command_string() const
{
  std::stringstream ss;
  for (const auto &part : parts) ss << part << " ";
  return ss.str();
}

// Convert parts to C-style arguments for `execvp`
std::vector<char *> bld::Command::to_exec_args() const
{
  std::vector<char *> exec_args;
  for (const auto &part : parts) exec_args.push_back(const_cast<char *>(part.c_str()));
  exec_args.push_back(nullptr);  // Null terminator
  return exec_args;
}

std::string bld::Command::get_print_string() const
{
  if (parts.empty())
    return "''";
  std::stringstream ss;
  ss << "' " << parts[0];
  if (parts.size() == 1)
    return ss.str() + "'";

  for (int i = 1; i < parts.size(); i++) ss << " " << parts[i];

  ss << " '";

  return ss.str();
}

template <typename... Args>
bld::Command::Command(Args... args)
{
  (parts.emplace_back(args), ...);
}

template <typename... Args>
void bld::Command::add_parts(Args... args)
{
  (parts.emplace_back(args), ...);
}

bool bld::validate_command(const bld::Command &command)
{
  bld::internal_log(bld::Log_type::WARNING, "Do you want to execute " + command.get_print_string() + "in shell");
  std::cerr << "  [WARNING]: Answer[y/n]: ";
  std::string response;
  std::getline(std::cin, response);
  return (response == "y" || response == "Y");
}

bld::Exit_status bld::wait_proc(bld::Proc proc)
{
  bld::Exit_status status{};

  if (!proc.is_valid())
  {
    bld::internal_log(Log_type::ERR, "Invalid process");
    return status;  // normal=false by default
  }

#ifdef _WIN32
  DWORD wait_result = WaitForSingleObject(proc.process_handle, INFINITE);
  if (wait_result != WAIT_OBJECT_0)
  {
    bld::log(Log_type::ERR, "WaitForSingleObject failed. Error: " + std::to_string(GetLastError()));
    return status;
  }

  DWORD exit_code = 0;
  if (!GetExitCodeProcess(proc.process_handle, &exit_code))
  {
    bld::log(Log_type::ERR, "Failed to get exit code. Error: " + std::to_string(GetLastError()));
    return status;
  }

  status.normal = true;
  status.exit_code = static_cast<int>(exit_code);

  if (exit_code != 0)
    bld::log(Log_type::ERR, "Process exited with code: " + std::to_string(exit_code));

#else
  int wait_status;
  if (waitpid(proc.p_id, &wait_status, 0) == -1)
  {
    bld::internal_log(Log_type::ERR, "waitpid failed: " + std::string(strerror(errno)));
    return status;
  }

  if (WIFEXITED(wait_status))
  {
    status.normal = true;
    status.exit_code = WEXITSTATUS(wait_status);

    if (status.exit_code != 0)
      bld::internal_log(Log_type::ERR, "Process exited with code: " + std::to_string(status.exit_code));
  }
  else if (WIFSIGNALED(wait_status))
  {
    status.signal = WTERMSIG(wait_status);
    bld::internal_log(Log_type::ERR, "Process terminated by signal: " + std::to_string(status.signal));
  }
#endif

  return status;
}

bld::Par_exec_res bld::wait_procs(std::vector<bld::Proc> procs, int sleep_ms)
{
  bld::Par_exec_res result;
  result.exit_statuses.resize(procs.size());
  std::vector<bool> completed(procs.size(), false);
  size_t remaining = procs.size();

  while (remaining > 0)
  {
    for (size_t i = 0; i < procs.size(); ++i)
    {
      if (completed[i])
        continue;

      bld::Wait_status status = bld::try_wait_nb(procs[i]);

      if (status.exited && !status.waitpid_failed && !status.invalid_proc)  // Process has terminated
      {
        completed[i] = true;
        remaining--;

        // Convert Wait_status to Exit_status for storage
        Exit_status exit_status{};
        exit_status.normal = status.normal;
        exit_status.exit_code = status.exit_code;
      #ifndef _WIN32
        exit_status.signal = status.signal;
      #endif
        result.exit_statuses[i] = exit_status;

        if (status)  // success (normal && exit_code == 0)
          result.completed++;
        else
          result.failed_indices.push_back(i);

        bld::cleanup_process(procs[i]);
      }
      // If !status.exited, process is still running, continue polling
    }

    if (remaining > 0)
      std::this_thread::sleep_for(std::chrono::milliseconds(sleep_ms));
  }

  return result;
}

void bld::cleanup_process(bld::Proc &proc)
{
#ifdef _WIN32
  if (proc.thread_handle)
  {
    CloseHandle(proc.thread_handle);
    proc.thread_handle = nullptr;
  }
  if (proc.process_handle)
  {
    CloseHandle(proc.process_handle);
    proc.process_handle = nullptr;
  }
  proc.process_id = 0;
#else
  proc.p_id = -1;
#endif
  proc.state = State::INIT_ERROR;  // Mark as invalid
  proc.ok = false;
}

bld::Exit_status bld::execute(const Command &command)
{
  bld::internal_log(Log_type::INFO, "Executing: " + command.get_print_string());

  Proc proc = execute_async(command);
  if (!proc)
  {
    Exit_status status{};
    return status;
  }

  Exit_status status = wait_proc(proc);
  cleanup_process(proc);
  return status;
}

bld::Proc bld::execute_async(const Command &command)
{
  if (command.is_empty())
  {
    bld::internal_log(Log_type::ERR, "No command to execute.");
    return Proc{};  // Default constructor gives USER_ERROR
  }

#ifdef _WIN32
  STARTUPINFOA si = {sizeof(STARTUPINFOA)};
  PROCESS_INFORMATION pi;

  // Build command string
  std::string command_str;
  for (size_t i = 0; i < command.parts.size(); ++i)
  {
    if (i > 0)
      command_str += " ";

    const auto &part = command.parts[i];
    if (part.find(' ') != std::string::npos)
      command_str += "\"" + part + "\"";
    else
      command_str += part;
  }

  std::vector<char> cmd_buffer(command_str.begin(), command_str.end());
  cmd_buffer.push_back('\0');
  if (!CreateProcessA(nullptr, cmd_buffer.data(), nullptr, nullptr, FALSE, 0, nullptr, nullptr, &si, &pi))
  {
    bld::log(Log_type::ERR, "Failed to create process. Error: " + std::to_string(GetLastError()));
    Proc proc;
    proc.res = Proc_result::SYSTEM_ERROR;
    return proc;
  }

  return Proc(pi.hProcess, pi.hThread, pi.dwProcessId);

#else
  auto args = command.to_exec_args();
  pid_t pid = fork();

  if (pid == -1)
  {
    bld::internal_log(Log_type::ERR, "Failed to fork: " + std::string(strerror(errno)));
    Proc proc;
    proc.state = State::INIT_ERROR;
    return proc;
  }
  else if (pid == 0)
  {
    // Child process
    if (execvp(args[0], args.data()) == -1)
    {
      bld::internal_log(Log_type::ERR, "Failed to exec: " + std::string(strerror(errno)));
      exit(EXIT_FAILURE);
    }
    bld::internal_log(bld::Log_type::ERR, "Unreachable code after execvp!");
    std::exit(EXIT_FAILURE);
  }

  return Proc(pid);
#endif
}

bld::Proc bld::execute_async_redirect(const Command &command, const Redirect &redirect)
{
  if (command.is_empty())
  {
    bld::internal_log(Log_type::ERR, "No command to execute.");
    return Proc{};
  }

#ifdef _WIN32
  STARTUPINFOA si = {sizeof(STARTUPINFOA)};
  PROCESS_INFORMATION pi;

  // Set up redirection
  si.dwFlags |= STARTF_USESTDHANDLES;
  si.hStdInput = redirect.stdin_fd != INVALID_FD ? redirect.stdin_fd : GetStdHandle(STD_INPUT_HANDLE);
  si.hStdOutput = redirect.stdout_fd != INVALID_FD ? redirect.stdout_fd : GetStdHandle(STD_OUTPUT_HANDLE);
  si.hStdError = redirect.stderr_fd != INVALID_FD ? redirect.stderr_fd : GetStdHandle(STD_ERROR_HANDLE);

  // Build command string
  std::string command_str;
  for (size_t i = 0; i < command.parts.size(); ++i)
  {
    if (i > 0)
      command_str += " ";

    const auto &part = command.parts[i];
    if (part.find(' ') != std::string::npos)
      command_str += "\"" + part + "\"";
    else
      command_str += part;
  }

  std::vector<char> cmd_buffer(command_str.begin(), command_str.end());
  cmd_buffer.push_back('\0');

  if (!CreateProcessA(nullptr, cmd_buffer.data(), nullptr, nullptr, TRUE, 0, nullptr, nullptr, &si, &pi))
  {
    bld::log(Log_type::ERR, "Failed to create process. Error: " + std::to_string(GetLastError()));
    Proc proc;
    proc.res = Proc_result::SYSTEM_ERROR;
    return proc;
  }

  CloseHandle(pi.hThread);  // We don't need the thread handle
  return Proc(pi.hProcess, nullptr, pi.dwProcessId);

#else
  auto args = command.to_exec_args();
  pid_t pid = fork();

  if (pid == -1)
  {
    bld::internal_log(Log_type::ERR, "Failed to fork: " + std::string(strerror(errno)));
    Proc proc;
    proc.state = State::INIT_ERROR;
    return proc;
  }
  else if (pid == 0)
  {
    // Child process - set up redirection
    if (redirect.stdin_fd != INVALID_FD)
    {
      if (dup2(redirect.stdin_fd, STDIN_FILENO) == -1)
      {
        bld::internal_log(Log_type::ERR, "Failed to redirect stdin: " + std::string(strerror(errno)));
        exit(EXIT_FAILURE);
      }
    }

    if (redirect.stdout_fd != INVALID_FD)
    {
      if (dup2(redirect.stdout_fd, STDOUT_FILENO) == -1)
      {
        bld::internal_log(Log_type::ERR, "Failed to redirect stdout: " + std::string(strerror(errno)));
        exit(EXIT_FAILURE);
      }
    }

    if (redirect.stderr_fd != INVALID_FD)
    {
      if (dup2(redirect.stderr_fd, STDERR_FILENO) == -1)
      {
        bld::internal_log(Log_type::ERR, "Failed to redirect stderr: " + std::string(strerror(errno)));
        exit(EXIT_FAILURE);
      }
    }

    // Close redirected FDs in child
    if (redirect.stdin_fd != INVALID_FD)
      close(redirect.stdin_fd);
    if (redirect.stdout_fd != INVALID_FD)
      close(redirect.stdout_fd);
    if (redirect.stderr_fd != INVALID_FD)
      close(redirect.stderr_fd);

    // Execute command
    if (execvp(args[0], args.data()) == -1)
    {
      bld::internal_log(Log_type::ERR, "Failed to exec: " + std::string(strerror(errno)));
      exit(EXIT_FAILURE);
    }
  }

  // Parent process - close redirected FDs
  if (redirect.stdin_fd != INVALID_FD)
    close(redirect.stdin_fd);
  if (redirect.stdout_fd != INVALID_FD)
    close(redirect.stdout_fd);
  if (redirect.stderr_fd != INVALID_FD)
    close(redirect.stderr_fd);

  return Proc(pid);
#endif
}

bld::Exit_status bld::execute_redirect(const Command &command, const Redirect &redirect)
{
  bld::internal_log(Log_type::INFO, "Executing with redirection: " + command.get_print_string());

  Proc proc = execute_async_redirect(command, redirect);
  if (!proc)
    return Exit_status{};

  Exit_status status = wait_proc(proc);
  cleanup_process(proc);
  return status;
}

// Open a file for reading
bld::Fd bld::open_for_read(const std::string &path)
{
#ifdef _WIN32
  SECURITY_ATTRIBUTES sa = {sizeof(SECURITY_ATTRIBUTES)};
  sa.bInheritHandle = TRUE;

  HAND handle = CreateFileA(path.c_str(), GENERIC_READ, 0, &sa, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (handle == INVALID_HANDLE_VALUE)
    bld::log(Log_type::ERR, "Failed to open file for reading: " + path);
  return handle;
#else
  int fd = open(path.c_str(), O_RDONLY);
  if (fd == -1)
    bld::internal_log(Log_type::ERR, "Failed to open file for reading: " + path + " - " + std::string(strerror(errno)));
  return fd;
#endif
}

// Open a file for writing
bld::Fd bld::open_for_write(const std::string &path, bool append)
{
#ifdef _WIN32
  SECURITY_ATTRIBUTES sa = {sizeof(SECURITY_ATTRIBUTES)};
  sa.bInheritHandle = TRUE;

  DWORD disposition = append ? OPEN_ALWAYS : CREATE_ALWAYS;
  HANDLE handle = CreateFileA(path.c_str(), GENERIC_WRITE, 0, &sa, disposition, FILE_ATTRIBUTE_NORMAL, NULL);
  if (handle == INVALID_HANDLE_VALUE)
  {
    bld::log(Log_type::ERR, "Failed to open file for writing: " + path);
    return INVALID_FD;
  }

  if (append)
    SetFilePointer(handle, 0, NULL, FILE_END);

  return handle;
#else
  int flags = O_WRONLY | O_CREAT | (append ? O_APPEND : O_TRUNC);
  int fd = open(path.c_str(), flags, 0644);
  if (fd == -1)
    bld::internal_log(Log_type::ERR, "Failed to open file for writing: " + path + " - " + std::string(strerror(errno)));
  return fd;
#endif
}

bld::Redirect::Redirect(const std::string& _in, const std::string& _out, const std::string& _err)
{
  if (_in.empty())
    this->stdin_fd = bld::INVALID_FD;
  else
    this->stdin_fd = bld::open_for_read(_in);
  if (_out.empty())
    this->stdout_fd = bld::INVALID_FD;
  else
    this->stdout_fd = bld::open_for_write(_out);
  if (_err.empty())
    this->stderr_fd = bld::INVALID_FD;
  else
    this->stderr_fd = bld::open_for_write(_err);
}

bld::Redirect::~Redirect()
{
  if (this->stdin_fd  != bld::INVALID_FD) bld::close_fd(stdin_fd);
  if (this->stdout_fd != bld::INVALID_FD) bld::close_fd(stdout_fd);
  if (this->stderr_fd != bld::INVALID_FD) bld::close_fd(stderr_fd);
}

template <typename... Fds, typename>
void bld::close_fd(Fds ...fds)
{
  (..., (
  [&]
  {
    if (fds == INVALID_FD) return;

    #ifdef _WIN32
      CloseHandle((HANDLE)fds);
    #else
      close(fds);
    #endif
  }()));
}

bld::Par_exec_res bld::execute_threads(const std::vector<bld::Command> &cmds, size_t threads, bool strict)
{
  bld::Par_exec_res result;
  result.exit_statuses.resize(cmds.size());

  if (cmds.empty())
    return result;
  if (threads == 0)
    threads = 1;
  if (threads > std::thread::hardware_concurrency())
    threads = std::thread::hardware_concurrency();

  std::mutex queue_mutex, output_mutex;
  std::atomic<bool> stop_workers{false};  // Used when strict = true

  // Queue of command indices to process
  std::queue<size_t> cmd_queue;
  for (size_t i = 0; i < cmds.size(); ++i) cmd_queue.push(i);

  bld::internal_log(bld::Log_type::INFO, "Executing " + std::to_string(cmds.size()) + " commands on " + std::to_string(threads) + " threads...");

  // Worker function
  auto worker = [&]()
  {
    while (true)
    {
      if (strict && stop_workers)
        return;  // Stop all threads if strict and an error occurs

      size_t cmd_idx;
      {
        std::lock_guard<std::mutex> lock(queue_mutex);
        if (cmd_queue.empty())
          return;
        cmd_idx = cmd_queue.front();
        cmd_queue.pop();
      }

      // Run command
      bld::Exit_status execution_result = execute(cmds[cmd_idx]);

      // Record result
      result.exit_statuses[cmd_idx] = execution_result;

      if (!execution_result.normal)
      {
        {
          std::lock_guard<std::mutex> lock(queue_mutex);
          result.failed_indices.push_back(cmd_idx);
        }

        if (strict)
        {
          stop_workers = true;
          return;
        }
      }
      else
      {
        {
          std::lock_guard<std::mutex> lock(queue_mutex);
          result.completed++;
        }

        {
          std::lock_guard<std::mutex> lock(output_mutex);
          bld::internal_log(bld::Log_type::INFO, "Completed: " + cmds[cmd_idx].get_print_string());
        }
      }
    }
  };

  // Launch worker threads
  std::vector<std::thread> workers;
  size_t num_threads = std::min(threads, cmds.size());
  for (size_t i = 0; i < num_threads; ++i) workers.emplace_back(worker);

  // Wait for all threads to complete
  for (auto &t : workers)
    if (t.joinable())
      t.join();

  return result;
}

void bld::print_metadata()
{
  std::cerr << '\n';
  bld::internal_log(Log_type::INFO, "Printing system metadata...........................................");

  // 1. Get OS information
  std::string os_name = "Unknown";
  std::string os_version = "Unknown";
  std::string arch = "Unknown";

#ifdef _WIN32
  OSVERSIONINFOEX os_info;
  ZeroMemory(&os_info, sizeof(OSVERSIONINFOEX));
  os_info.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);

  if (GetVersionEx(reinterpret_cast<OSVERSIONINFO *>(&os_info)))
  {
    os_name = "Windows";
    os_version = std::to_string(os_info.dwMajorVersion) + "." + std::to_string(os_info.dwMinorVersion) + " (Build " +
                 std::to_string(os_info.dwBuildNumber) + ")";
  }

  // Detect architecture
  SYSTEM_INFO sys_info;
  GetNativeSystemInfo(&sys_info);
  switch (sys_info.wProcessorArchitecture)
  {
    case PROCESSOR_ARCHITECTURE_AMD64:
      arch = "64-bit";
      break;
    case PROCESSOR_ARCHITECTURE_INTEL:
      arch = "32-bit";
      break;
    case PROCESSOR_ARCHITECTURE_ARM64:
      arch = "ARM 64-bit";
      break;
    case PROCESSOR_ARCHITECTURE_ARM:
      arch = "ARM 32-bit";
      break;
    default:
      arch = "Unknown";
      break;
  }
#else
  struct utsname sys_info;
  if (uname(&sys_info) == 0)
  {
    os_name = sys_info.sysname;
    os_version = sys_info.release;
    arch = sys_info.machine;
  }
#endif

  std::cerr << "    Operating System: " << os_name << " " << os_version << " (" << arch << ")" << std::endl;

  // 2. Compiler information
  std::cerr << "    Compiler:         ";
#ifdef __clang__
  std::cerr << "Clang " << __clang_major__ << "." << __clang_minor__ << "." << __clang_patchlevel__;
#elif defined(__GNUC__)
  std::cerr << "GCC " << __GNUC__ << "." << __GNUC_MINOR__ << "." << __GNUC_PATCHLEVEL__;
#elif defined(_MSC_VER)
  std::cerr << "MSVC " << _MSC_VER;
#else
  std::cerr << "Unknown Compiler";
#endif
  std::cerr << std::endl;

  internal_log(Log_type::INFO, "...................................................................\n");
}

int bld::get_n_procs(bool physical_cores_only)
{
#ifdef _WIN32
  if (physical_cores_only)
  {
    // Proper Windows physical core count
    DWORD length = 0;
    GetLogicalProcessorInformation(nullptr, &length);
    if (GetLastError() != ERROR_INSUFFICIENT_BUFFER)
      return 0;

    auto *buffer = (SYSTEM_LOGICAL_PROCESSOR_INFORMATION *)malloc(length);
    if (!buffer)
      return 0;

    if (!GetLogicalProcessorInformation(buffer, &length))
    {
      free(buffer);
      return 0;
    }

    unsigned core_count = 0;
    DWORD byteOffset = 0;
    while (byteOffset < length)
    {
      auto *current = (SYSTEM_LOGICAL_PROCESSOR_INFORMATION *)((BYTE *)buffer + byteOffset);
      if (current->Relationship == RelationProcessorCore)
        core_count++;
      byteOffset += sizeof(SYSTEM_LOGICAL_PROCESSOR_INFORMATION);
    }

    free(buffer);
    return core_count;
  }
  else
  {
    // Logical processors
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    return sysinfo.dwNumberOfProcessors;
  }
#else
  if (physical_cores_only)
  {
    // Physical cores on Unix-like systems
    return sysconf(_SC_NPROCESSORS_CONF);  // _SC_NPROCESSORS_CONF for physical cores
  }
  else
  {
    // Logical processors on Unix-like systems
    return sysconf(_SC_NPROCESSORS_ONLN);  // _SC_NPROCESSORS_ONLN for logical processors
  }
#endif
}
// Execute the shell command with preprocessed parts
int bld::execute_shell(std::string cmd)
{
  std::cout.flush();
  return std::system(cmd.c_str());
}

int bld::execute_shell(std::string cmd, bool prompt)
{
  if (prompt)
  {
    if (validate_command(cmd))
      return execute_shell(cmd);
    else
      return -1;
  }
  // Execute the shell command using the original execute function
  return execute_shell(cmd);
}

bool bld::read_process_output(const Command &cmd, std::string &output, size_t buffer_size)
{
  if (cmd.is_empty())
  {
    bld::internal_log(Log_type::ERR, "No command to execute.");
    return false;
  }

  bld::internal_log(Log_type::INFO, "Executing with output: " + cmd.get_print_string());
  output.clear();

#ifdef _WIN32
  // Create an anonymous pipe for output
  SECURITY_ATTRIBUTES sa;
  sa.nLength = sizeof(SECURITY_ATTRIBUTES);
  sa.bInheritHandle = TRUE;
  sa.lpSecurityDescriptor = NULL;

  HANDLE read_pipe, write_pipe;
  if (!CreatePipe(&read_pipe, &write_pipe, &sa, 0))
  {
    bld::log(Log_type::ERR, "Failed to create pipe: " + std::to_string(GetLastError()));
    return false;
  }

  // Set up redirection to capture both stdout and stderr
  Redirect redirect(INVALID_FD, write_pipe, write_pipe);
  Proc proc = execute_async_redirect(cmd, redirect);

  if (!proc)
  {
    CloseHandle(read_pipe);
    CloseHandle(write_pipe);
    return false;
  }

  // Close write end in parent (child process has its own copy)
  CloseHandle(write_pipe);

  // Read output from the pipe
  std::vector<char> buffer(buffer_size);
  DWORD bytes_read;
  while (ReadFile(read_pipe, buffer.data(), static_cast<DWORD>(buffer.size()), &bytes_read, NULL) && bytes_read > 0)
    output.append(buffer.data(), bytes_read);

  CloseHandle(read_pipe);

  Exit_status result = wait_proc(proc);
  cleanup_process(proc);

  return result;  // Uses bool conversion operator

#else
  // Create a pipe for output
  int pipefd[2];
  if (pipe(pipefd) == -1)
  {
    bld::internal_log(Log_type::ERR, "Failed to create pipe: " + std::string(strerror(errno)));
    return false;
  }

  // Set up redirection to capture both stdout and stderr
  Redirect redirect(INVALID_FD, pipefd[1], pipefd[1]);
  Proc proc = execute_async_redirect(cmd, redirect);

  if (!proc)
  {
    close(pipefd[0]);
    close(pipefd[1]);
    return false;
  }

  // Close write end in parent (child process has its own copy)
  close(pipefd[1]);

  // Read output from the pipe
  std::vector<char> buffer(buffer_size);
  ssize_t bytes_read;
  while ((bytes_read = read(pipefd[0], buffer.data(), buffer.size())) > 0) output.append(buffer.data(), bytes_read);

  close(pipefd[0]);

  Exit_status result = wait_proc(proc);
  return result;
#endif
}

bld::Wait_status bld::try_wait_nb(const bld::Proc &proc)
{
  bld::Wait_status status{};
  // Default values: normal=false, exit_code=0, exited=false

  if (!proc.is_valid())
  {
    bld::internal_log(Log_type::ERR, "Invalid process for non-blocking wait");
    status.exited = true;
    status.invalid_proc = true;
    return status;  // exited=false, so caller knows to skip this process
  }

#ifdef _WIN32
  DWORD wait_result = WaitForSingleObject(proc.process_handle, 0);  // 0 = no wait

  if (wait_result == WAIT_OBJECT_0)
  {
    // Process has terminated
    status.exited = true;

    DWORD exit_code = 0;
    if (GetExitCodeProcess(proc.process_handle, &exit_code))
    {
      status.normal = true;
      status.exit_code = static_cast<int>(exit_code);
      if (exit_code != 0)
        bld::log(Log_type::ERR, "Process exited with code: " + std::to_string(exit_code));
    }
    else
    {
      bld::log(Log_type::ERR, "Failed to get exit code. Error: " + std::to_string(GetLastError()));
      // status.normal remains false, but exited=true so we know it terminated
    }
  }
  else if (wait_result == WAIT_TIMEOUT)
  {
    // Process is still running - exited remains false
  }
  else
  {
    // Error occurred
    bld::log(Log_type::ERR, "WaitForSingleObject failed. Error: " + std::to_string(GetLastError()));
    // exited remains false, caller will retry or handle as appropriate
  }

#else
  int wait_status;
  pid_t result = waitpid(proc.p_id, &wait_status, WNOHANG);  // WNOHANG = non-blocking

  if (result == proc.p_id)
  {
    // Process has terminated
    status.exited = true;

    if (WIFEXITED(wait_status))
    {
      status.normal = true;
      status.exit_code = WEXITSTATUS(wait_status);
      if (status.exit_code != 0)
        bld::internal_log(Log_type::ERR, "Process exited with code: " + std::to_string(status.exit_code));
    }
    else if (WIFSIGNALED(wait_status))
    {
      status.signal = WTERMSIG(wait_status);
      bld::internal_log(Log_type::ERR, "Process terminated by signal: " + std::to_string(status.signal));
      // status.normal remains false
    }
  }
  else if (result == 0)
  {
    status.exited = false;
  }
  else  // result == -1
  {
    // Error occurred
    if (errno == ECHILD)
    {
      // Process doesn't exist anymore (already reaped)
      bld::internal_log(Log_type::WARNING, "Process already reaped");
      status.exited = true;  // Treat as exited
      status.normal = true;  // Assume successful exit
    }
    else
    {
      bld::internal_log(Log_type::ERR, "waitpid failed: " + std::string(strerror(errno)));
      status.exited = true;
      status.waitpid_failed = true;
      // exited remains false, let caller handle retry
    }
  }
#endif

  return status;
}

bool bld::read_shell_output(const std::string &cmd, std::string &output)
{
#ifdef _WIN32
  FILE *pipe = _popen(command.c_str(), "r");
#else
  FILE *pipe = popen(cmd.c_str(), "r");
#endif

  if (!pipe)
  {
    bld::internal_log(Log_type::ERR, "Failed to open pipe for command: " + cmd);
    return false;
  }

  char buffer[4096];
  while (fgets(buffer, sizeof(buffer), pipe) != nullptr) output += buffer;

#ifdef _WIN32
  int exit_code = _pclose(pipe);
#else
  int exit_code = pclose(pipe);
#endif

  // Check if command executed successfully
  if (exit_code != 0)
  {
    bld::internal_log(Log_type::ERR, "Command failed with exit code: " + std::to_string(exit_code));
    return false;
  }

  return true;
}

bool bld::is_executable_outdated(std::string file_name, std::string executable)
{
  try
  {
    // Check if the source file exists
    if (!std::filesystem::exists(file_name))
    {
      bld::internal_log(Log_type::ERR, "Source file does not exist: " + file_name);
      return false;  // Or handle this case differently
    }

    // Check if the executable exists
    if (!std::filesystem::exists(executable))
      return true;  // Treat as changed if the executable doesn't exist

    // Get last write times
    auto last_write_time = std::filesystem::last_write_time(file_name);
    auto last_write_time_exec = std::filesystem::last_write_time(executable);

    // Compare timestamps
    return last_write_time > last_write_time_exec;
  }
  catch (const std::filesystem::filesystem_error &e)
  {
    bld::internal_log(Log_type::ERR, "Filesystem error: " + std::string(e.what()));
    return false;  // Or handle the error differently
  }
  catch (const std::exception &e)
  {
    bld::internal_log(Log_type::ERR, std::string(e.what()));
    return false;  // Or handle the error differently
  }
}

void bld::rebuild_yourself_onchange_and_run(const std::string &filename, const std::string &executable, std::string compiler)
{
  namespace fs = std::filesystem;
  // Convert to filesystem paths
  fs::path source_path(filename);
  fs::path exec_path(executable);
  fs::path backup_path = exec_path.string() + ".old";

  if (!bld::is_executable_outdated(filename, executable))
    return;  // No rebuild needed

  bld::internal_log(Log_type::INFO, "Build executable not up-to-date. Rebuilding...");

  // Create backup of existing executable if it exists
  if (fs::exists(exec_path))
  {
    try
    {
      if (fs::exists(backup_path))
        fs::remove(backup_path);  // Remove existing backup
      fs::rename(exec_path, backup_path);
      bld::internal_log(Log_type::INFO, "Created backup at: " + backup_path.string());
    }
    catch (const fs::filesystem_error &e)
    {
      bld::internal_log(Log_type::ERR, "Failed to create backup: " + std::string(e.what()));
      return;
    }
  }

  // Detect the compiler if not provided
  if (compiler.empty())
  {
#ifdef __clang__
    compiler = "clang++";
#elif defined(__GNUC__)
    compiler = "g++";
#elif defined(_MSC_VER)
    compiler = "cl";  // MSVC
#else
    bld::log(Log_type::ERROR, "Unknown compiler. Defaulting to g++.");
    compiler = "g++";
#endif
  }

  // Set up the compile command
  bld::Command cmd;
  cmd.parts = {compiler, source_path.string(), "-o", exec_path.string()};

  // Execute the compile command
  int compile_result = bld::execute(cmd);
  if (compile_result <= 0)
  {
    bld::internal_log(Log_type::ERR, "Compilation failed.");

    // Restore backup if compilation failed and backup exists
    if (fs::exists(backup_path))
    {
      try
      {
        fs::remove(exec_path);  // Remove failed compilation output if it exists
        fs::rename(backup_path, exec_path);
        bld::internal_log(Log_type::INFO, "Restored previous executable from backup.");
      }
      catch (const fs::filesystem_error &e)
      {
        bld::internal_log(Log_type::ERR, "Failed to restore backup: " + std::string(e.what()));
      }
    }
    return;
  }

  bld::internal_log(Log_type::INFO, "Compilation successful. Restarting w/o any args for safety...");

  // Verify the new executable exists and is executable
  if (!fs::exists(exec_path))
  {
    bld::internal_log(Log_type::ERR, "New executable not found after successful compilation.");
    return;
  }

  // Make sure the new executable has proper permissions
  try
  {
    fs::permissions(exec_path, fs::perms::owner_exec | fs::perms::group_exec | fs::perms::others_exec, fs::perm_options::add);
  }
  catch (const fs::filesystem_error &e)
  {
    bld::internal_log(Log_type::WARNING, "Failed to set executable permissions: " + std::string(e.what()));
  }

  // Run the new executable
  bld::Command restart_cmd;
  restart_cmd.parts = {exec_path.string()};

  int restart_result = bld::execute(restart_cmd);
  if (restart_result <= 0)
  {
    bld::internal_log(Log_type::ERR, "Failed to start new executable.");
    return;
  }

  // Only remove backup after successful restart
  try
  {
    if (fs::exists(backup_path))
      fs::remove(backup_path);
  }
  catch (const fs::filesystem_error &e)
  {
    bld::internal_log(Log_type::WARNING, "Failed to remove backup: " + std::string(e.what()));
  }

  // Exit the current process after successfully restarting
  std::exit(EXIT_SUCCESS);
}

void bld::rebuild_yourself_onchange(const std::string &filename, const std::string &executable, std::string compiler)
{
  if (bld::is_executable_outdated(filename, executable))
  {
    bld::internal_log(Log_type::INFO, "Build executable not up-to-date. Rebuilding...");
    bld::Command cmd = {};

    // Detect the compiler if not provided
    if (compiler.empty())
    {
    #ifdef __clang__
      compiler = "clang++";
    #elif defined(__GNUC__)
      compiler = "g++";
    #elif defined(_MSC_VER)
      compiler = "cl";  // MSVC uses 'cl' as the compiler command
    #else
      bld::log(Log_type::ERROR, "Unknown compiler. Defaulting to g++.");
      compiler = "g++";
    #endif
    }

    // Set up the compile command
    cmd.parts = {compiler, filename, "-o", executable};

    // Execute the compile command
    if (bld::execute(cmd) <= 0)
    {
      bld::internal_log(Log_type::WARNING, "Failed to rebuild executable.");
      return;
    }
  }
}

namespace bld
{
  // Static instance getter
  Config &Config::get()
  {
    static Config instance;
    static bool initialized = false;
    if (!initialized)
    {
      instance.initialize_builtin_options();
      initialized = true;
    }
    return instance;
  }

  // Define custom config options
  void Config::add_flag(const std::string &name, const std::string &description)
  {
    custom_configs[name] = {.default_value = "", .description = description, .is_flag = true};
  }

  void Config::add_option(const std::string &name, const std::string &default_value, const std::string &description)
  {
    custom_configs[name] = {.default_value = default_value, .description = description, .is_flag = false};
    if (!default_value.empty())
      values[name] = default_value;
  }

  const std::unordered_map<std::string, Config::CustomConfig> &Config::get_custom_configs() const { return custom_configs; }

  // Config_proxy implementation
  Config::Config_proxy::Config_proxy(const std::string &k, Config *c) : key(k), config(c) {}

  Config::Config_proxy &Config::Config_proxy::operator=(const std::string &value)
  {
    config->values[key] = value;
    config->flags.erase(key);  // Remove from flags if it was there

    // Sync to built-in members
    if (key == "compiler" || key == "c")
    {
      config->compiler = value;
    }
    else if (key == "target" || key == "t")
    {
      config->target = value;
    }
    else if (key == "build-dir" || key == "d")
    {
      config->build_dir = value;
    }
    else if (key == "flags" || key == "f")
    {
      config->compiler_flags = value;
    }
    else if (key == "link" || key == "l")
    {
      config->linker_flags = value;
    }
    else if (key == "threads" || key == "j")
    {
      if (str::is_numeric(value))
        config->threads = std::max(1, std::stoi(value));
    }
    else if (key == "pre")
    {
      config->pre_build = value;
    }
    else if (key == "post")
    {
      config->post_build = value;
    }

    return *this;
  }

  Config::Config_proxy &Config::Config_proxy::operator=(const char *value)
  {
    config->values[key] = std::string(value);
    config->flags.erase(key);
    return *this;
  }

  Config::Config_proxy::operator std::string() const
  {
    auto it = config->values.find(key);
    if (it != config->values.end())
      return it->second;

    auto custom_it = config->custom_configs.find(key);
    if (custom_it != config->custom_configs.end())
      return custom_it->second.default_value;

    // Fallback: check if it's a built-in that wasn't synced
    if (key == "compiler" || key == "c")
      return config->compiler;
    if (key == "target" || key == "t")
      return config->target;
    if (key == "build-dir" || key == "d")
      return config->build_dir;
    if (key == "flags" || key == "f")
      return config->compiler_flags;
    if (key == "link" || key == "l")
      return config->linker_flags;
    if (key == "threads" || key == "j")
      return std::to_string(config->threads);
    if (key == "pre")
      return config->pre_build;
    if (key == "post")
      return config->post_build;

    return "";
  }

  bool Config::Config_proxy::operator==(const std::string &other) const
  {
    // First check if it's a flag
    if (config->flags.count(key))
      return other == "true" || other == "yes" || other == "1" || other.empty();
    // Then check value
    auto it = config->values.find(key);
    return it != config->values.end() && it->second == other;
  }

  bool Config::Config_proxy::operator==(const char *other) const { return *this == std::string(other); }

  Config::Config_proxy::operator bool() const
  {
    if (config->flags.count(key))
      return true;

    auto it = config->values.find(key);
    if (it != config->values.end())
    {
      const std::string &value = it->second;
      return value == "true" || value == "yes" || value == "1" || (!value.empty() && value != "false" && value != "no" && value != "0");
    }

    // Check built-in boolean flags
    if (key == "verbose" || key == "v")
      return config->verbose;
    if (key == "hot-reload" || key == "hr")
      return config->hot_reload;
    if (key == "override-run")
      return config->override_run;

    return false;
  }

  bool Config::Config_proxy::exists() const { return config->flags.count(key) || config->values.count(key); }

  int Config::Config_proxy::as_int() const
  {
    std::string val = *this;
    if (val.empty() || val.find_first_not_of("0123456789") != std::string::npos)
      return 0;
    return std::stoi(val);
  }

  // Config subscript operators
  Config::Config_proxy Config::operator[](const std::string &key) { return Config_proxy(key, this); }

  const Config::Config_proxy Config::operator[](const std::string &key) const { return Config_proxy(key, const_cast<Config *>(this)); }

  void Config::initialize_builtin_options()
  {
    // Initialize built-in options as custom configs so they work with []
    add_option("compiler", compiler, "Compiler to use");
    add_option("c", compiler, "Compiler to use (short form)");
    add_option("target", target, "Target executable name");
    add_option("t", target, "Target executable name (short form)");
    add_option("build-dir", build_dir, "Build directory");
    add_option("d", build_dir, "Build directory (short form)");
    add_option("flags", compiler_flags, "Compiler flags");
    add_option("f", compiler_flags, "Compiler flags (short form)");
    add_option("link", linker_flags, "Linker flags");
    add_option("l", linker_flags, "Linker flags (short form)");
    add_option("threads", std::to_string(threads), "Number of build threads");
    add_option("j", std::to_string(threads), "Number of build threads (short form)");
    add_option("pre", pre_build, "Pre-build command");
    add_option("post", post_build, "Post-build command");

    // Flags
    add_flag("verbose", "Enable verbose output");
    add_flag("v", "Enable verbose output (short form)");
    add_flag("hot-reload", "Enable hot reload");
    add_flag("hr", "Enable hot reload (short form)");
    add_flag("override-run", "Override run behavior");
    add_flag("help", "Show help");
    add_flag("h", "Show help (short form)");
  }
  // Argument parsing
  void Config::parse_args(const std::vector<std::string> &args)
  {
    for (const auto &arg : args)
    {
      if (!str::starts_with(arg, "-"))
        continue;

      auto eq_pos = arg.find('=');

      if (eq_pos == std::string::npos)
      {
        std::string flag = arg.substr(1);
        flags.insert(flag);
        values.erase(flag);  // Remove from values if it was there

        if (flag == "v" || flag == "verbose")
        {
          verbose = true;
          flags.insert("verbose");
          flags.insert("v");
        }
        else if (flag == "hr" || flag == "hot-reload")
        {
          hot_reload = true;
          flags.insert("hot-reload");
          flags.insert("hr");
        }
        else if (flag == "override-run")
        {
          override_run = true;
        }
        else if (flag == "h" || flag == "help")
        {
          // Don't show help automatically, let user handle it
        }
      }
      else
      {
        // Key=value pair like -compiler=g++, -test=yes
        std::string key = arg.substr(1, eq_pos - 1);
        std::string value = arg.substr(eq_pos + 1);

        values[key] = value;
        flags.erase(key);  // Remove from flags if it was there

        // Use [] assignment operator which will sync to built-ins automatically
        (*this)[key] = value;
      }
    }
  }

  // Private helper methods
  bool Config::is_true(const std::string &value) const { return value == "true" || value == "1" || value == "yes"; }

  bool Config::is_number(const std::string &value) const
  {
    return !value.empty() && value.find_first_not_of("0123456789") == std::string::npos;
  }

  void Config::parse_file_list(const std::string &value)
  {
    if (!value.empty())
    {
      std::stringstream ss(value);
      std::string file;
      hot_reload_files.clear();
      while (std::getline(ss, file, ','))
        if (!file.empty())
          hot_reload_files.push_back(file);
      hot_reload = true;
    }
  }

  // Help and documentation
  void Config::show_help() const
  {
    std::cout << "Config Usage:\n"
              << "Flags (no value needed):\n"
              << "  -flag_name              Set flag (e.g., -test, -debug, -verbose)\n"
              << "\nKey=Value pairs:\n"
              << "  -key=value              Set config value (e.g., -compiler=clang++)\n"
              << "\nBuilt-in options:\n"
              << "  -c, -compiler=COMPILER  Compiler to use\n"
              << "  -t, -target=TARGET      Target executable name\n"
              << "  -f, -flags=FLAGS        Compiler flags\n"
              << "  -j, -threads=N          Build threads\n"
              << "  -v, -verbose            Enable verbose output\n"
              << "  -hr, -hot-reload        Enable hot reload\n"
              << "  --watch=files           Comma-separated files to watch\n";

    if (!custom_configs.empty())
    {
      std::cout << "\nCustom options:\n";
      for (const auto &[name, config] : custom_configs)
      {
        if (config.is_flag)
        {
          std::cout << "  -" << name;
          if (!config.description.empty())
            std::cout << "                    " << config.description;
          std::cout << "\n";
        }
        else
        {
          std::cout << "  -" << name << "=VALUE";
          if (!config.default_value.empty())
            std::cout << "           (default: " << config.default_value << ")";
          if (!config.description.empty())
            std::cout << " " << config.description;
          std::cout << "\n";
        }
      }
    }

    std::cout << "\nAny other -key=value and -flags are automatically stored!\n";
  }

  void Config::dump() const
  {
    std::cout << "=== Config Dump ===\n";
    std::cout << "Flags:\n";
    for (const auto &flag : flags) std::cout << "  " << flag << "\n";
    std::cout << "Values:\n";
    for (const auto &[key, value] : values) std::cout << "  " << key << " = " << value << "\n";
    std::cout << "==================\n";
  }

  // File operations
  bool Config::load_from_file(const std::string &filename)
  {
    std::ifstream file(filename);
    if (!file.is_open())
      return false;

    std::vector<std::string> file_args;
    std::string line;
    while (std::getline(file, line))
    {
      line.erase(0, line.find_first_not_of(" \t"));
      if (line.empty() || line[0] == '#')
        continue;

      if (line.find('=') != std::string::npos)
      {
        // key=value format
        if (!str::starts_with(line, "-"))
          line = "-" + line;
        file_args.push_back(line);
      }
      else
      {
        // Simple flag
        if (!str::starts_with(line, "-"))
          line = "-" + line;
        file_args.push_back(line);
      }
    }

    parse_args(file_args);
    return true;
  }

  bool Config::save_to_file(const std::string &filename) const
  {
    std::ofstream file(filename);
    if (!file.is_open())
      return false;

    file << "# Build configuration\n";

    // Save built-in options
    file << "compiler=" << compiler << "\n";
    file << "target=" << target << "\n";
    file << "build-dir=" << build_dir << "\n";
    file << "flags=" << compiler_flags << "\n";
    file << "threads=" << threads << "\n";

    if (verbose)
      file << "verbose\n";
    if (hot_reload)
      file << "hot-reload\n";
    if (override_run)
      file << "override-run\n";

    // Save custom flags
    for (const auto &flag : flags)
      if (flag != "verbose" && flag != "hot-reload" && flag != "override-run" && flag != "v" && flag != "hr")
        file << flag << "\n";

    // Save custom values
    for (const auto &[key, value] : values)
    {
      if (key != "compiler" && key != "target" && key != "build-dir" && key != "flags" && key != "threads" && key != "c" && key != "t" &&
          key != "d" && key != "f" && key != "j")
      {
        file << key << "=" << value << "\n";
      }
    }

    return true;
  }

  // Command handler functions
  void handle_config_command(const std::vector<std::string> &args, const std::string &program_name)
  {
    if (args.size() < 2)
    {
      std::cout << "Usage: " << program_name << " config [options]\n";
      Config::get().show_help();
      return;
    }

    std::vector<std::string> config_args(args.begin() + 1, args.end());
    Config::get().parse_args(config_args);
  }

  void handle_args(int argc, char *argv[])
  {
    std::vector<std::string> args;
    Config::get().cmd_args.clear();

    for (int i = 0; i < argc; ++i)
    {
      args.push_back(argv[i]);
      Config::get().cmd_args.push_back(argv[i]);
    }

    if (args.size() <= 1)
      return;

    std::string command = args[1];
    if (command == "-configure")
    {
      handle_config_command(args, argv[0]);
      bld::Config::get().save_to_file();
      internal_log(Log_type::INFO, "Configuration saved.");
    }
    if (command == "-use-config")
    {
      bld::Config::get().load_from_file();
      return;
    }
    handle_config_command(args, argv[0]);
    // Users can handle other commands themselves
  }

}  // namespace bld

bool bld::args_to_vec(int argc, char *argv[], std::vector<std::string> &args)
{
  if (argc < 1)
    return false;

  args.reserve(argc - 1);
  for (int i = 1; i < argc; i++) args.push_back(argv[i]);

  return true;
}

int bld::handle_run_command(std::vector<std::string> args)
{
#ifdef BLD_USE_CONFIG
  if (args.size() == 2)
  {
    bld::log(bld::Log_type::WARNING, "Command 'run' specified with the executable");
    bld::log(bld::Log_type::INFO, "Proceeding to run the specified command: " + args[1]);
    bld::Command cmd(args[1]);
    return bld::execute(cmd);
  }
  else if (args.size() > 2)
  {
    bld::log(bld::Log_type::ERR, "Too many arguments for 'run' command. Only executables are supported.");
    bld::log(bld::Log_type::INFO, "Usage: run <executable>");
    exit(EXIT_FAILURE);
  }
  if (bld::Config::get().target.empty())
  {
    bld::log(bld::Log_type::ERR, "No target executable specified in config");
    exit(1);
  }

  bld::Command cmd;
  cmd.parts.push_back(Config::get().target);

  bld::execute(cmd);
  exit(EXIT_SUCCESS);
#else
  if (args.size() < 2)
  {
    bld::internal_log(bld::Log_type::ERR,
             "No target executable specified in config. Config is disabled. Please enable BLD_USE_CONFIG macro to use the Config class.");
    exit(EXIT_FAILURE);
  }
  else if (args.size() == 2)
  {
    bld::internal_log(bld::Log_type::WARNING, "Command 'run' specified with the executable");
    bld::internal_log(bld::Log_type::INFO, "Proceeding to run the specified command: " + args[1]);
    bld::Command cmd(args[1]);
    return bld::execute(cmd);
  }
  else if (args.size() > 2)
  {
    bld::internal_log(bld::Log_type::ERR, "Too many arguments for 'run' command. Only executables are supported.");
    bld::internal_log(bld::Log_type::INFO, "Usage: run <executable>");
    exit(EXIT_FAILURE);
  }
#endif
  bld::internal_log(bld::Log_type::ERR, "Should never be reached: " + std::to_string(__LINE__));
  exit(EXIT_FAILURE);
}

bool bld::starts_with(const std::string &str, const std::string &prefix)
{
  if (prefix.size() > str.size())
    return false;
  return str.compare(0, prefix.size(), prefix) == 0;
}


bool bld::fs::read_file(const std::string &path, std::string &content)
{
  if (!std::filesystem::exists(path))
  {
    bld::internal_log(bld::Log_type::ERR, "File does not exist: " + path);
    return false;
  }

  std::ifstream file(path, std::ios::binary);

  if (!file)
  {
    bld::internal_log(bld::Log_type::ERR, "Failed to open file: " + path);
    return false;
  }

  content = std::string(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
  file.close();
  return true;
}

bool bld::fs::write_entire_file(const std::string &path, const std::string &content)
{
  std::ofstream file(path, std::ios::binary);

  if (!file)
  {
    bld::internal_log(bld::Log_type::ERR, "Failed to open file for writing: " + path);
    return false;
  }

  file << content;
  bool success = file.good();
  file.close();
  return success;
}

bool bld::fs::append_file(const std::string &path, const std::string &content)
{
  std::ofstream file(path, std::ios::app | std::ios::binary);
  if (!file)
  {
    bld::internal_log(bld::Log_type::ERR, "Failed to open file for appending: " + path);
    return false;
  }

  file << content;
  bool success = file.good();
  file.close();
  return success;
}

bool bld::fs::read_lines(const std::string &path, std::vector<std::string> &lines)
{
  std::ifstream file(path);
  if (!file)
  {
    bld::internal_log(bld::Log_type::ERR, "Failed to open file: " + path);
    return false;
  }

  std::string line;
  while (std::getline(file, line)) lines.push_back(line);

  return true;
}

bool bld::fs::replace_in_file(const std::string &path, const std::string &from, const std::string &to)
{
  std::string content = "";
  if (!bld::fs::read_file(path, content))
  {
    bld::internal_log(bld::Log_type::ERR, "Failed to read file: " + path);
    return false;
  }
  if (content.empty())
  {
    bld::internal_log(bld::Log_type::ERR, "Failed to read file or it is empty: " + path);
    return false;
  }
  size_t pos = 0;
  while ((pos = content.find(from, pos)) != std::string::npos)
  {
    content.replace(pos, from.length(), to);
    pos += to.length();
  }

  return bld::fs::write_entire_file(path, content);
}

bool bld::fs::copy_file(const std::string &from, const std::string &to, bool overwrite)
{
  try
  {
    if (!overwrite && std::filesystem::exists(to))
    {
      bld::internal_log(bld::Log_type::ERR, "Destination file already exists: " + to);
      return false;
    }
    std::filesystem::copy_file(from, to,
                               overwrite ? std::filesystem::copy_options::overwrite_existing : std::filesystem::copy_options::none);
    return true;
  }
  catch (const std::filesystem::filesystem_error &e)
  {
    bld::internal_log(bld::Log_type::ERR, "Failed to copy file: " + std::string(e.what()));
    return false;
  }
}

bool bld::fs::move_file(const std::string &from, const std::string &to)
{
  try
  {
    std::filesystem::rename(from, to);
    return true;
  }
  catch (const std::filesystem::filesystem_error &e)
  {
    bld::internal_log(bld::Log_type::ERR, "Failed to move file: " + std::string(e.what()));
    return false;
  }
}

std::string bld::fs::get_extension(const std::string &path)
{
  if (!std::filesystem::exists(path))
  {
    bld::internal_log(bld::Log_type::ERR, "File for extension request does not exist: " + path);
    return "";
  }
  std::filesystem::path p(path);
  return p.extension().string();
}

std::string bld::fs::get_stem(const std::string &path, bool with_full_path)
{
  std::string filename = path;
  if (!with_full_path)
    filename = bld::fs::get_file_name(path);
  auto pos = filename.find_last_of('.');
  return pos == std::string::npos ? filename : filename.substr(0, pos);
}

bool bld::fs::create_directory(const std::string &path)
{
  try
  {
    return std::filesystem::create_directories(path);
  }
  catch (const std::filesystem::filesystem_error &e)
  {
    bld::internal_log(bld::Log_type::ERR, "Failed to create directory: " + std::string(e.what()));
    return false;
  }
}

bool bld::fs::create_dir_if_not_exists(const std::string &path)
{
  if (std::filesystem::exists(path))
  {
    bld::internal_log(bld::Log_type::WARNING, "Directory ' " + path + " ' already exists, manage it yourself to not lose data!");
    return true;
  }

  try
  {
    bool created = std::filesystem::create_directories(path);
    if (created)
      bld::internal_log(bld::Log_type::INFO, "Directory created: " + path);
    else
      bld::internal_log(bld::Log_type::ERR, "Failed to create directory: " + path);
    return created;
  }
  catch (const std::filesystem::filesystem_error &e)
  {
    bld::internal_log(bld::Log_type::ERR, "Failed to create directory: " + std::string(e.what()));
    return false;
  }
}

template <typename... Paths, typename>
bool bld::fs::create_dirs_if_not_exists(const Paths &...paths)
{
  return (... && create_dir_if_not_exists(paths));
}

template <typename... Paths, typename>
void bld::fs::remove(const Paths &...paths)
{
  (... & std::filesystem::remove(paths));
}


bool bld::fs::remove_dir(const std::string &path)
{
  if (!std::filesystem::exists(path))
  {
    bld::internal_log(bld::Log_type::INFO, "Directory does not exist: " + path);
    return true;
  }

  try
  {
    std::uintmax_t removed_count = std::filesystem::remove_all(path);
    if (removed_count > 0)
      bld::internal_log(bld::Log_type::INFO, "Directory removed: " + path);
    else
      bld::internal_log(bld::Log_type::ERR, "Failed to remove directory: " + path);
    return removed_count > 0;
  }
  catch (const std::filesystem::filesystem_error &e)
  {
    bld::internal_log(bld::Log_type::ERR, "Failed to remove directory: " + std::string(e.what()));
    return false;
  }
}

std::vector<std::string> bld::fs::list_files_in_dir(const std::string &path, bool recursive)
{
  std::vector<std::string> files;
  try
  {
    if (recursive)
    {
      for (const auto &entry : std::filesystem::recursive_directory_iterator(path))
        if (entry.is_regular_file())
          files.push_back(entry.path().string());
    }
    else
    {
      for (const auto &entry : std::filesystem::directory_iterator(path))
        if (entry.is_regular_file())
          files.push_back(entry.path().string());
    }
  }
  catch (const std::filesystem::filesystem_error &e)
  {
    bld::internal_log(bld::Log_type::ERR, "Failed to list files: " + std::string(e.what()));
  }
  return files;
}

std::vector<std::string> bld::fs::list_directories(const std::string &path, bool recursive)
{
  std::vector<std::string> directories;
  try
  {
    if (recursive)
    {
      for (const auto &entry : std::filesystem::recursive_directory_iterator(path))
        if (entry.is_directory())
          directories.push_back(entry.path().string());
    }
    else
    {
      for (const auto &entry : std::filesystem::directory_iterator(path))
        if (entry.is_directory())
          directories.push_back(entry.path().string());
    }
  }
  catch (const std::filesystem::filesystem_error &e)
  {
    bld::internal_log(bld::Log_type::ERR, "Failed to list directories: " + std::string(e.what()));
  }
  return directories;
}

std::string bld::fs::get_file_name(std::string full_path)
{
  auto path = std::filesystem::path(full_path);
  return path.filename().string();
}

std::string bld::fs::strip_file_name(std::string full_path)
{
  auto path = std::filesystem::path(full_path);
  return path.parent_path().string();
}

std::vector<std::string> bld::fs::get_all_files_with_name(const std::string &dir, const std::string &name, bool recursive)
{
  namespace fs = std::filesystem;
  std::vector<std::string> results;

  try
  {
    if (!fs::exists(dir) || !fs::is_directory(dir))
    {
      bld::internal_log(bld::Log_type::WARNING, "Directory: " + dir + " doesnt exist.");
      return results;
    }
    if (recursive)
    {
      for (const auto &entry : fs::recursive_directory_iterator(dir, fs::directory_options::skip_permission_denied))
      {
        try
        {
          if (entry.is_regular_file() && entry.path().filename() == name)
            results.push_back(entry.path().string());
        }
        catch (const fs::filesystem_error &e)
        {
          bld::internal_log(bld::Log_type::WARNING, "Filesystem error: " + std::string(e.what()));
          bld::internal_log(bld::Log_type::INFO, "Skipping previous file");
          continue;
        }
      }
    }
    else
    {
      for (const auto &entry : fs::directory_iterator(dir, fs::directory_options::skip_permission_denied))
      {
        try
        {
          if (entry.is_regular_file() && entry.path().filename() == name)
            results.push_back(entry.path().string());
        }
        catch (const fs::filesystem_error &e)
        {
          bld::internal_log(bld::Log_type::WARNING, "Filesystem error: " + std::string(e.what()));
          bld::internal_log(bld::Log_type::INFO, "Skipping previous file");
          continue;
        }
      }
    }
  }
  catch (const fs::filesystem_error &e)
  {
    bld::internal_log(bld::Log_type::WARNING, "Filesystem error: " + std::string(e.what()));
  }

  return results;
}
// Alternative version with more robust error handling and logging
std::vector<std::string> bld::fs::get_all_files_with_extensions(const std::string &path,const std::vector<std::string> &extensions, bool recursive, bool case_insensitive)
{
  std::vector<std::string> matching_files;

  // Validate input
  if (path.empty())
  {
    bld::internal_log(Log_type::ERR, "Empty path provided: " + path);
    return matching_files;
  }

  if (extensions.empty())
  {
    bld::internal_log(Log_type::WARNING, "No extensions provided for path: " + path);
    return matching_files;
  }

  // Check if path exists and is a directory
  std::filesystem::path fs_path(path);
  std::error_code ec;

  if (!std::filesystem::exists(fs_path, ec))
  {
    bld::internal_log(Log_type::ERR, "Path does not exist: " + path);
    return matching_files;
  }

  if (ec)
  {
    bld::internal_log(Log_type::ERR, "Error checking path existence: " + ec.message());
    return matching_files;
  }

  if (!std::filesystem::is_directory(fs_path, ec))
  {
    bld::internal_log(Log_type::ERR, "Path is not a directory: " + path);
    return matching_files;
  }

  if (ec)
  {
    bld::internal_log(Log_type::ERR, "Error checking if path is directory: " + ec.message());
    return matching_files;
  }

  // Normalize extensions - handle both "cpp" and ".cpp" formats
  std::vector<std::string> normalized_extensions;
  for (const auto &ext : extensions)
  {
    if (ext.empty())
      continue;

    std::string normalized = ext;

    // Ensure extension starts with '.' - handle both "cpp" and ".cpp"
    if (normalized[0] != '.')
      normalized = "." + normalized;

    // Apply case transformation if case_insensitive is true
    if (case_insensitive)
      std::transform(normalized.begin(), normalized.end(), normalized.begin(), [](unsigned char c) { return std::tolower(c); });

    normalized_extensions.push_back(normalized);
  }

  if (normalized_extensions.empty())
  {
    bld::internal_log(Log_type::WARNING, "No valid extensions after normalization");
    return matching_files;
  }

  // Process files
  auto process_entry = [&](const std::filesystem::directory_entry &entry)
  {
    std::error_code entry_ec;

    if (!entry.is_regular_file(entry_ec))
      return;

    if (entry_ec)
      return;  // Skip this entry if we can't determine its type

    std::string file_ext = entry.path().extension().string();

    // Apply case transformation to file extension if case_insensitive is true
    std::string comparison_ext = file_ext;
    if (case_insensitive)
      std::transform(comparison_ext.begin(), comparison_ext.end(), comparison_ext.begin(), [](unsigned char c) { return std::tolower(c); });

    for (const auto &target_ext : normalized_extensions)
    {
      if (comparison_ext == target_ext)
      {
        matching_files.push_back(entry.path().string());
        break;
      }
    }
  };

  // Iterate through directory
  if (recursive)
  {
    for (const auto &entry :
         std::filesystem::recursive_directory_iterator(fs_path, std::filesystem::directory_options::skip_permission_denied, ec))
    {
      if (ec)
      {
        bld::internal_log(Log_type::WARNING, "Error during recursive iteration: " + ec.message());
        ec.clear();  // Clear error and continue
        continue;
      }
      process_entry(entry);
    }
  }
  else
  {
    for (const auto &entry : std::filesystem::directory_iterator(fs_path, ec))
    {
      if (ec)
      {
        bld::internal_log(Log_type::WARNING, "Error during directory iteration: " + ec.message());
        break;
      }
      process_entry(entry);
    }
  }

  return matching_files;
}

std::string bld::env::get(const std::string &key)
{
#ifdef _WIN32
  char buffer[32767];  // Maximum size for environment variables on Windows
  DWORD size = GetEnvironmentVariableA(key.c_str(), buffer, sizeof(buffer));
  return size > 0 ? std::string(buffer, size) : "";
#else
  const char *value = std::getenv(key.c_str());
  return value ? std::string(value) : "";
#endif
}

bool bld::env::set(const std::string &key, const std::string &value)
{
#ifdef _WIN32
  return SetEnvironmentVariableA(key.c_str(), value.c_str()) != 0;
#else
  return setenv(key.c_str(), value.c_str(), 1) == 0;
#endif
}

bool bld::env::exists(const std::string &key)
{
#ifdef _WIN32
  return GetEnvironmentVariableA(key.c_str(), nullptr, 0) > 0;
#else
  return std::getenv(key.c_str()) != nullptr;
#endif
}

bool bld::env::unset(const std::string &key)
{
#ifdef _WIN32
  return SetEnvironmentVariableA(key.c_str(), nullptr) != 0;
#else
  return unsetenv(key.c_str()) == 0;
#endif
}

#if defined(__APPLE__) || defined(__FreeBSD__)
#include <crt_externs.h>  // For `_NSGetEnviron()`
#define ENVIRON (*_NSGetEnviron())
#elif defined(_WIN32)
#include <windows.h>
#define ENVIRON nullptr  // Not used in Windows implementation
#else
extern char **environ;  // Standard for Linux
#define ENVIRON environ
#endif

std::unordered_map<std::string, std::string> bld::env::get_all()
{
  std::unordered_map<std::string, std::string> env_vars;

#if defined(_WIN32)
  // Windows uses `GetEnvironmentStrings()` to retrieve environment variables
  LPWCH env_block = GetEnvironmentStringsW();
  if (!env_block)
    return env_vars;

  LPWCH env = env_block;
  while (*env)
  {
    std::wstring entry(env);
    size_t pos = entry.find(L'=');
    if (pos != std::wstring::npos)
    {
      std::string key(entry.begin(), entry.begin() + pos);
      std::string value(entry.begin() + pos + 1, entry.end());
      env_vars[key] = value;
    }
    env += entry.size() + 1;  // Move to next environment variable
  }
  FreeEnvironmentStringsW(env_block);

#else
  // Linux, macOS, FreeBSD: Use `environ`
  for (char **env = ENVIRON; *env; ++env)
  {
    std::string entry(*env);
    size_t pos = entry.find('=');
    if (pos != std::string::npos)
      env_vars[entry.substr(0, pos)] = entry.substr(pos + 1);
  }
#endif
  return env_vars;
}

bld::Dep::Dep(std::string target, std::vector<std::string> dependencies, bld::Command command)
    : target(std::move(target)), dependencies(std::move(dependencies)), command(std::move(command))
{
}

// Constructor for phony targets
bld::Dep::Dep(std::string target, std::vector<std::string> dependencies, bool is_phony)
    : target(std::move(target)), dependencies(std::move(dependencies)), is_phony(is_phony)
{
}

// Implicit conversion from initializer list for better usability
bld::Dep::Dep(std::string target, std::initializer_list<std::string> deps, bld::Command command)
    : target(std::move(target)), dependencies(deps), command(std::move(command))
{
}

// Copy constructor
bld::Dep::Dep(const Dep &other) : target(other.target), dependencies(other.dependencies), command(other.command), is_phony(other.is_phony)
{
}

// Move constructor
bld::Dep::Dep(Dep &&other) noexcept
    : target(std::move(other.target)),
      dependencies(std::move(other.dependencies)),
      command(std::move(other.command)),
      is_phony(other.is_phony)
{
}

// Copy assignment
bld::Dep &bld::Dep::operator=(const Dep &other)
{
  if (this != &other)
  {
    target = other.target;
    dependencies = other.dependencies;
    command = other.command;
    is_phony = other.is_phony;
  }
  return *this;
}

// Move assignment
bld::Dep &bld::Dep::operator=(Dep &&other) noexcept
{
  if (this != &other)
  {
    target = std::move(other.target);
    dependencies = std::move(other.dependencies);
    command = std::move(other.command);
    is_phony = other.is_phony;
  }
  return *this;
}

void bld::Dep_graph::add_dep(const bld::Dep &dep)
{
  // We create a node and save it to map with key "File"
  auto node = std::make_unique<Node>(dep);
  node->dependencies = dep.dependencies;
  nodes[dep.target] = std::move(node);
}

void bld::Dep_graph::add_phony(const std::string &target, const std::vector<std::string> &deps)
{
  Dep phony_dep;
  phony_dep.target = target;
  phony_dep.dependencies = deps;
  phony_dep.is_phony = true;
  add_dep(phony_dep);
}

bool bld::Dep_graph::needs_rebuild(const Node *node)
{
  if (node->dep.is_phony)
    return true;
  if (!std::filesystem::exists(node->dep.target))
    return !node->dep.dependencies.empty();

  auto target_time = std::filesystem::last_write_time(node->dep.target);
  for (const auto &dep : node->dep.dependencies)
  {
    if (!std::filesystem::exists(dep))
    {
      bld::internal_log(bld::Log_type::ERR, "Dependency does not exist: " + dep);
      return true;  // Missing dependency forces rebuild
    }
    if (std::filesystem::last_write_time(dep) > target_time)
      return true;
  }
  return false;  // Explicit return when no rebuild needed
}

bool bld::Dep_graph::build(const std::string &target)
{
  std::unordered_set<std::string> visited, in_progress;
  if (detect_cycle(target, visited, in_progress))
  {
    bld::internal_log(bld::Log_type::ERR, "Circular dependency detected for target: " + target);
    return false;
  }
  checked_sources.clear();
  return build_node(target);
}

bool bld::Dep_graph::build(const Dep &dep)
{
  add_dep(dep);
  return build(dep.target);
}

bool bld::Dep_graph::build_all()
{
  bool success = true;
  for (const auto &node : nodes)
    if (!build(node.first))
      success = false;
  return success;
}

bool bld::Dep_graph::F_build_all()
{
  checked_sources.clear();
  bool success = true;
  for (const auto &node : nodes)
    if (!build(node.first))
      success = false;
  return success;
}

bool bld::Dep_graph::build_node(const std::string &target)
{
  auto it = nodes.find(target);
  if (it == nodes.end())
  {
    if (std::filesystem::exists(target))
    {
      if (checked_sources.find(target) == checked_sources.end())
      {
        bld::internal_log(bld::Log_type::INFO, "Using existing source file: " + target);
        checked_sources.insert(target);
      }
      return true;
    }
    bld::internal_log(bld::Log_type::ERR, "Target not found: " + target);
    return false;
  }

  Node *node = it->second.get();
  if (node->checked)  // Skip if we've already checked this node
    return true;

  // First build all dependencies
  for (const auto &dep : node->dependencies)
    if (!build_node(dep))
      return false;

  // Check if we need to rebuild
  if (!needs_rebuild(node))
  {
    bld::internal_log(bld::Log_type::INFO, "Target up to date: " + target);
    node->checked = true;
    return true;
  }

  // Execute build command if not phony
  if (!node->dep.is_phony && !node->dep.command.is_empty())
  {
    bld::internal_log(bld::Log_type::INFO, "Building target: " + target);
    if (execute(node->dep.command) <= 0)
    {
      bld::internal_log(bld::Log_type::ERR, "Failed to build target: " + target);
      return false;
    }
  }
  else if (node->dep.is_phony)
    bld::internal_log(bld::Log_type::INFO, "Phony target: " + target);
  else
    bld::internal_log(bld::Log_type::WARNING, "No command for target: " + target);

  node->checked = true;
  return true;
}

bool bld::Dep_graph::detect_cycle(const std::string &target, std::unordered_set<std::string> &visited,
                                  std::unordered_set<std::string> &in_progress)
{
  if (in_progress.find(target) != in_progress.end())
    return true;  // Cycle detected

  if (visited.find(target) != visited.end())
    return false;  // Already processed

  auto it = nodes.find(target);
  if (it == nodes.end())
    return false;  // Target doesn't exist

  in_progress.insert(target);

  for (const auto &dep : it->second->dependencies)
    if (detect_cycle(dep, visited, in_progress))
      return true;

  in_progress.erase(target);
  visited.insert(target);
  return false;
}

bool bld::Dep_graph::build_parallel(const std::string &target, size_t thread_count)
{
  if (thread_count > std::thread::hardware_concurrency() - 1)
    thread_count = std::thread::hardware_concurrency() - 1;
  if (thread_count == 0)
    thread_count = 1;

  std::unordered_set<std::string> visited, in_progress;
  if (detect_cycle(target, visited, in_progress))
  {
    bld::internal_log(bld::Log_type::ERR, "Circular dependency detected for target: " + target);
    return false;
  }

  bld::internal_log(bld::Log_type::INFO, "Building all targets in parallel using " + std::to_string(thread_count) + " threads");

  std::queue<std::string> ready_targets;
  if (!prepare_build_graph(target, ready_targets))
    return false;

  std::mutex queue_mutex, log_mutex;
  std::condition_variable cv;
  std::atomic<bool> build_failed{false};
  std::atomic<size_t> active_builds{0};
  std::atomic<size_t> completed_targets{0};
  const size_t total_targets = nodes.size();

  auto worker = [&]()
  {
    while (!build_failed)
    {
      std::string current_target;
      {
        std::unique_lock<std::mutex> lock(queue_mutex);
        if (ready_targets.empty() && completed_targets < total_targets)
        {
          cv.wait_for(lock, std::chrono::milliseconds(100),
                      [&]() { return !ready_targets.empty() || build_failed || completed_targets == total_targets; });
        }

        if (ready_targets.empty() || build_failed)
          return;

        current_target = ready_targets.front();
        ready_targets.pop();
        nodes[current_target]->in_progress = true;
        active_builds++;
      }

      auto node = nodes[current_target].get();
      if (needs_rebuild(node))
      {
        if (!node->dep.is_phony && !node->dep.command.is_empty())
        {
          {
            std::lock_guard<std::mutex> log_lock(log_mutex);
            bld::internal_log(bld::Log_type::INFO, "Building target: " + current_target);
          }

          if (execute(node->dep.command) <= 0)
          {
            {
              std::lock_guard<std::mutex> log_lock(log_mutex);
              bld::internal_log(bld::Log_type::ERR, "Failed to build target: " + current_target);
            }
            build_failed = true;
            cv.notify_all();
            return;
          }
        }
      }
      else
      {
        process_completed_target(current_target, ready_targets, queue_mutex, cv);
        completed_targets++;
        active_builds--;
        cv.notify_all();
      }
    }
  };

  std::vector<std::thread> workers;
  for (size_t i = 0; i < thread_count; ++i) workers.emplace_back(worker);

  for (auto &t : workers)
    if (t.joinable())
      t.join();

  return !build_failed;
}

bool bld::Dep_graph::prepare_build_graph(const std::string &target, std::queue<std::string> &ready_targets)
{
  auto it = nodes.find(target);
  if (it == nodes.end())
  {
    if (std::filesystem::exists(target))
    {
      if (checked_sources.find(target) == checked_sources.end())
      {
        bld::internal_log(bld::Log_type::INFO, "Using existing source file: " + target);
        checked_sources.insert(target);
      }
      return true;
    }
    bld::internal_log(bld::Log_type::ERR, "Target not found: " + target);
    return false;
  }

  auto node = it->second.get();
  if (node->visited)
    return true;
  node->visited = true;

  // Process dependencies
  for (const auto &dep : node->dependencies)
  {
    if (!prepare_build_graph(dep, ready_targets))
      return false;

    // Only track node dependencies that actually need rebuilding
    if (nodes.find(dep) != nodes.end() && needs_rebuild(nodes[dep].get()))
      node->waiting_on.push_back(dep);
  }

  // Only add to ready queue if NEEDS rebuild and dependencies are met
  if (node->waiting_on.empty() && needs_rebuild(node))
    ready_targets.push(target);

  return true;
}

void bld::Dep_graph::process_completed_target(const std::string &target, std::queue<std::string> &ready_targets, std::mutex &queue_mutex,
                                              std::condition_variable &cv)
{
  std::lock_guard<std::mutex> lock(queue_mutex);
  nodes[target]->checked = true;
  nodes[target]->in_progress = false;

  // Find all nodes that were waiting on this target
  for (const auto &node_pair : nodes)
  {
    auto &node = node_pair.second;
    if (!node->checked && !node->in_progress)
    {
      auto &waiting = node->waiting_on;
      waiting.erase(std::remove(waiting.begin(), waiting.end(), target), waiting.end());

      // If no more dependencies, add to ready queue
      if (waiting.empty())
        ready_targets.push(node_pair.first);
    }
  }
}

bool bld::Dep_graph::build_all_parallel(size_t thread_count)
{
  std::vector<std::string> root_targets;
  for (const auto &node : nodes)
  {
    bool is_dependency = false;
    for (const auto &other : nodes)
    {
      if (std::find(other.second->dep.dependencies.begin(), other.second->dep.dependencies.end(), node.first) !=
          other.second->dep.dependencies.end())
      {
        is_dependency = true;
        break;
      }
    }
    if (!is_dependency)
      root_targets.push_back(node.first);
  }

  // Create a master phony target that depends on all root targets
  add_phony("__master_target__", root_targets);
  bool result = build_parallel("__master_target__", thread_count);

  // Clean up the master target
  nodes.erase("__master_target__");
  return result;
}

std::string bld::str::trim(const std::string &str)
{
  {
    const auto begin = str.find_first_not_of(" \t\n\r\f\v");
    if (begin == std::string::npos)
      return "";  // No non-space characters
    const auto end = str.find_last_not_of(" \t\n\r\f\v");
    return str.substr(begin, end - begin + 1);
  }
}

std::string bld::str::trim_left(const std::string &str)
{
  if (str.size() == 0)
    return "";

  const auto begin = str.find_first_not_of(" \t\n\r\f\v");
  if (begin == std::string::npos)
    return "";  // No non-space characters
  const auto end = str.size();
  return str.substr(begin, end - begin + 1);
}

std::string bld::str::trim_right(const std::string &str)
{
  if (str.size() == 0)
    return "";

  const auto begin = 0;
  if (begin == std::string::npos)
    return "";  // No non-space characters
  const auto end = str.find_last_not_of(" \t\n\r\f\v");
  return str.substr(begin, end - begin + 1);
}

std::string bld::str::to_lower(const std::string &str)
{
  std::string result = str;
  std::transform(result.begin(), result.end(), result.begin(), ::tolower);
  return result;
}

std::string bld::str::to_upper(const std::string &str)
{
  std::string result = str;
  std::transform(result.begin(), result.end(), result.begin(), ::toupper);
  return result;
}

std::string bld::str::replace(std::string str, const std::string &from, const std::string &to)
{
  if (str.size() == 0)
  {
    if (from == "")
      return to;
    else
      return str;
  }

  size_t start_pos = 0;
  while ((start_pos = str.find(from, start_pos)) != std::string::npos)
  {
    str.replace(start_pos, from.length(), to);
    start_pos += to.length();
  }
  return str;
}

bool bld::str::starts_with(const std::string &str, const std::string &prefix) { return str.find(prefix) == 0; }

bool bld::str::ends_with(const std::string &str, const std::string &suffix)
{
  if (str.length() < suffix.length())
    return false;
  return str.compare(str.length() - suffix.length(), suffix.length(), suffix) == 0;
}

std::string bld::str::join(const std::vector<std::string> &strings, const std::string &delimiter)
{
  if (strings.size() == 0)
    return "";

  std::ostringstream oss;
  for (size_t i = 0; i < strings.size(); ++i)
  {
    if (i != 0)
      oss << delimiter;
    oss << strings[i];
  }
  return oss.str();
}

std::string bld::str::trim_till(const std::string &str, char delimiter)
{
  if (str.size() == 0 || str.size() == 1)
    return "";

  const auto pos = str.find(delimiter);
  if (pos == std::string::npos)
    return str;  // Delimiter not found, return the whole string
  return str.substr(pos + 1);
}

bool bld::str::equal_ignorecase(const std::string &str1, const std::string &str2)
{
  if (str1.size() != str2.size())
    return false;

  return std::equal(str1.begin(), str1.end(), str2.begin(), [](char c1, char c2) { return std::tolower(c1) == std::tolower(c2); });
}

std::vector<std::string> bld::str::chop_by_delimiter(const std::string &s, const std::string &delimiter)
{
  if (delimiter.size() == 0)
    return {s};
  // Estimate number of splits to reduce vector reallocations
  std::vector<std::string> res;
  res.reserve(std::count(s.begin(), s.end(), delimiter[0]) + 1);

  size_t pos_start = 0, pos_end, delim_len = delimiter.length();

  while ((pos_end = s.find(delimiter, pos_start)) != std::string::npos)
  {
    res.push_back(s.substr(pos_start, pos_end - pos_start));
    pos_start = pos_end + delim_len;
  }

  // Handle last segment, even if it's an empty string after trailing delimiter
  res.push_back(s.substr(pos_start));

  return res;
}

std::string bld::str::remove_duplicates(const std::string &str)
{
  // Early exit for empty or single-character strings
  if (str.size() <= 1)
    return str;

  // Use a character set to track seen characters
  std::unordered_set<char> seen;
  std::string result;
  result.reserve(str.size());  // Preallocate to avoid reallocations

  for (char c : str)
  {
    // Only insert if not previously seen
    if (seen.insert(c).second)
      result += c;
  }

  return result;
}

std::string bld::str::remove_duplicates_case_insensitive(const std::string &str)
{
  // Early exit for empty or single-character strings
  if (str.size() <= 1)
    return str;

  // Use a character set to track seen characters, converted to lowercase
  std::unordered_set<char> seen;
  std::string result;
  result.reserve(str.size());  // Preallocate to avoid reallocations

  for (char c : str)
  {
    // Convert to lowercase for comparison, but preserve original case
    char lower = std::tolower(c);
    if (seen.insert(lower).second)
      result += c;
  }

  return result;
}

bool bld::str::is_numeric(const std::string &str)
{
  if (str.empty())
    return false;

  // Track if we've seen a decimal point
  bool decimal_point_seen = false;

  // Start index to skip potential sign
  size_t start = (str[0] == '-' || str[0] == '+') ? 1 : 0;

  for (size_t i = start; i < str.length(); ++i)
  {
    // Check for decimal point
    if (str[i] == '.')
    {
      // Only one decimal point allowed
      if (decimal_point_seen)
        return false;
      decimal_point_seen = true;
      continue;
    }

    // Must be a digit
    if (!std::isdigit(str[i]))
      return false;
  }

  return true;
}

std::string bld::str::replace_all(const std::string &str, const std::string &from, const std::string &to)
{
  if (from.empty())
    return str;

  std::string result = str;
  size_t start_pos = 0;

  while ((start_pos = result.find(from, start_pos)) != std::string::npos)
  {
    result.replace(start_pos, from.length(), to);
    start_pos += to.length();  // Move past the replacement
  }

  return result;
}

#endif  // B_LDR_IMPLEMENTATION
