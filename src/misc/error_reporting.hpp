#pragma once

#include <string_view>
#include <print>

bool HAD_ERROR = false;

void report(int line, std::string_view where, std::string_view message)
{
  std::println(stderr, "[ line {}:{} ] ERROR: {} ", line, where, message);
  HAD_ERROR = true;
}

void report(int line, std::string_view message) { report(line, "", message); }
