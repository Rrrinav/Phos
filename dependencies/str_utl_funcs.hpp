/*
Copyright Dec 2024, Rinav (github: rrrinav)

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

// USAGE:
// Use in a simple STB style.
// Download and import like this
// #define STR_IMPLEMENTATION
// #include "./str_utl_funcs.hpp"

// POINT OF LIBRARY:
// This just contains some string functions I am tired of implementing again and again, so yeah!
// They might not be most efficient or best but they work for me and if they work for you ass well, go ahead use them!

#pragma once

#include <string>
#include <vector>

namespace utl
{
  /* @brief remove leading and trailing whitespace from a string
   * @param str: the string to trim
   * @return the trimmed string
   * @details This function will remove leading and trailing whitespace from a string including: ' ', '\t', '\n', '\r', '\f', '\v'
   */
  inline std::string trim(const std::string &str);

  /*
   * @brief Trim leading whitespace from a string
   * @param str: the string to trim
   * @return the trimmed string
   */
  std::string trim_left(const std::string &str);

  /*
   * @brief Trim trailing whitespace from a string
   * @param str: the string to trim
   * @return the trimmed string
   */
  std::string trim_right(const std::string &str);

  /* @brief Convert string to lowercase
   * @param str: the string to convert
   * @return the string in lowercase
   */
  inline std::string to_lower(const std::string &str);

  /* @brief Convert string to uppercase
   * @param str: the string to convert
   * @return the string in uppercase
   */
  inline std::string to_upper(const std::string &str);

  /* @brief Replace all occurrences of a substring with another substring
   * @param str: the string to search
   * @param from: the substring to search for
   * @param to: the substring to replace with
   * @return the string with all occurrences of 'from' replaced with 'to'
   * @details This function will replace all occurrences of 'from' with 'to' in the string 'str'
   */
  inline std::string replace(std::string str, const std::string &from, const std::string &to);

  /* @brief Check if a string starts with a prefix
   * @param str: the string to check
   * @param prefix: the prefix to check for
   * @return true if the string starts with the prefix, false otherwise
   */
  bool starts_with(const std::string &str, const std::string &prefix);

  /*
   * @brief Check if a string ends with a suffix
   * @param str: the string to check
   * @param suffix: the suffix to check for
   * @return true if the string ends with the suffix, false otherwise
   */
  bool ends_with(const std::string &str, const std::string &suffix);

  /*
   * @brief Join a vector of strings into a single string with a delimiter
   * @param strs: the vector of strings to join
   * @param delimiter: the delimiter to separate the strings
   * @return a single string with all the elements of the vector joined by the delimiter
   * @details This function will join all the elements of the vector 'strs' into a single string separated by the 'delimiter'
   */
  std::string join(const std::vector<std::string> &strs, const std::string &delimiter);

  /*
   * @brief Trim a string till a delimiter
   * @param str: the string to trim
   * @param delimiter: the delimiter to trim till
   * @return the trimmed string
   * @details This function will trim the string 'str' till the first occurrence of the 'delimiter'
   */
  std::string trim_till(const std::string &str, char delimiter);

  /* @brief Check if two strings are equal ignoring case
   * @param str1: the first string to compare
   * @param str2: the second string to compare
   * @return true if the strings are equal ignoring case, false otherwise
   * @details This function will compare the two strings 'str1' and 'str2' ignoring the case of the characters
   */
  bool equal_ignorecase(const std::string &str1, const std::string &str2);

  /*
   * @breif Split a string by a delimiter
   * @param s: the string to split
   * @param delimiter: the delimiter to split by
   * @return a vector of strings split by the delimiter
   * @details This function will split the string 's' by the delimiter 'delimiter' and return a vector of the split
   */
  std::vector<std::string> chop_by_delimiter(const std::string &s, const std::string &delimiter);

  /* @brief Remove duplicate characters from a string
   * @param str: the string to remove duplicates from
   * @return a string with all duplicates removed
   */
  std::string remove_duplicates(const std::string &str);

  /* @brief Remove duplicate characters (case insensitively) from a string
   * @param str: the string to remove duplicates from
   * @return a string with all duplicates removed
   * @details  Uses std::tolower() for comparison. Preserves the case of the first occurrence of each character. Removes subsequent duplicates regardless of case
   */
  std::string remove_duplicates_case_insensitive(const std::string &str);

  /* @breif check if a string is a number or not
   * @param string to check
   * @return true if number, false if not
   * @details Handles integers and floating-point numbers. Supports positive and negative numbers. Allows only one decimal point. Rejects strings with non-numeric characters
   */
  bool is_numeric(const std::string &str);

  /* @breif replace all occurences of a substring with something
   * @param str: main string to perform replace function on
   * @param from: sub-string to replace in main string 'str'
   * @param to: sub-string to replace 'from' with in 'str'
   * @details Replaces all occurrences of a substring. Handles multiple replacements in a single string. Works with empty strings and edge cases. Returns original string if substring not found. 
   */
  std::string replace_all(const std::string &str, const std::string &from, const std::string &to);
}  // namespace utl

#ifdef STR_IMPLEMENTATION
#include <algorithm>
#include <sstream>
#include <unordered_set>

std::string utl::trim(const std::string &str)
{
  {
    const auto begin = str.find_first_not_of(" \t\n\r\f\v");
    if (begin == std::string::npos)
      return "";  // No non-space characters
    const auto end = str.find_last_not_of(" \t\n\r\f\v");
    return str.substr(begin, end - begin + 1);
  }
}

std::string utl::trim_left(const std::string &str)
{
  if (str.size() == 0)
    return "";

  const auto begin = str.find_first_not_of(" \t\n\r\f\v");
  if (begin == std::string::npos)
    return "";  // No non-space characters
  const auto end = str.size();
  return str.substr(begin, end - begin + 1);
}

std::string utl::trim_right(const std::string &str)
{
  if (str.size() == 0)
    return "";

  const auto begin = 0;
  if (begin == std::string::npos)
    return "";  // No non-space characters
  const auto end = str.find_last_not_of(" \t\n\r\f\v");
  return str.substr(begin, end - begin + 1);
}

std::string utl::to_lower(const std::string &str)
{
  std::string result = str;
  std::transform(result.begin(), result.end(), result.begin(), ::tolower);
  return result;
}

std::string utl::to_upper(const std::string &str)
{
  std::string result = str;
  std::transform(result.begin(), result.end(), result.begin(), ::toupper);
  return result;
}

std::string utl::replace(std::string str, const std::string &from, const std::string &to)
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

bool utl::starts_with(const std::string &str, const std::string &prefix) { return str.find(prefix) == 0; }

bool utl::ends_with(const std::string &str, const std::string &suffix)
{
  if (str.length() < suffix.length())
    return false;
  return str.compare(str.length() - suffix.length(), suffix.length(), suffix) == 0;
}

std::string utl::join(const std::vector<std::string> &strings, const std::string &delimiter)
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

std::string utl::trim_till(const std::string &str, char delimiter)
{
  if (str.size() == 0 || str.size() == 1)
    return "";

  const auto pos = str.find(delimiter);
  if (pos == std::string::npos)
    return str;  // Delimiter not found, return the whole string
  return str.substr(pos + 1);
}

bool utl::equal_ignorecase(const std::string &str1, const std::string &str2)
{
  if (str1.size() != str2.size())
    return false;

  return std::equal(str1.begin(), str1.end(), str2.begin(), [](char c1, char c2) { return std::tolower(c1) == std::tolower(c2); });
}

std::vector<std::string> utl::chop_by_delimiter(const std::string &s, const std::string &delimiter)
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

std::string utl::remove_duplicates(const std::string &str)
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

std::string utl::remove_duplicates_case_insensitive(const std::string &str)
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

bool utl::is_numeric(const std::string &str)
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

std::string utl::replace_all(const std::string &str, const std::string &from, const std::string &to)
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
#endif  // str_funcs
