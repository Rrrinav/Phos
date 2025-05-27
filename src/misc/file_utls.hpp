#include <optional>
#include <fstream>
#include <string>
#include <string_view>
#include <print>

namespace utl
{
  inline std::optional<std::string> read_entire_file(std::string_view path_)
  {
    std::ifstream file(path_.data());
    if (!file.is_open())
    {
      std::println("Could not open file: {}", path_);
      return std::nullopt;
    }
    int size = 0;
    file.seekg(0, std::ios::end);
    size = file.tellg();
    if (size == 0)
    {
      std::println("File is empty: {}", path_);
      return std::nullopt;
    }
    file.seekg(0, std::ios::beg);

    std::string bytes(size, ' ');
    file.read(&bytes[0], size);
    return bytes;
  }
} //namespace  utl
