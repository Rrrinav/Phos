cmake_minimum_required(VERSION 3.20)
project(phos CXX)

# Cross-platform build path (Windows/MSVC or MinGW, macOS, Linux).
# Linux developers may keep using ./bld; this file exists so the project
# builds with standard tooling everywhere. UNTESTED on Windows.
set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

file(GLOB_RECURSE PHOS_SOURCES CONFIGURE_DEPENDS
    src/*.cpp
    src/core/*.cpp
    src/core/*/*.cpp
    src/backend/*.cpp
    src/frontend/*/*.cpp
    src/frontend/*/*/*.cpp
    src/virtual_machine/*.cpp
    src/virtual_machine/*/*.cpp
    src/utility/*.cpp
)

add_executable(phos ${PHOS_SOURCES})

target_include_directories(phos PRIVATE src)

if(MSVC)
    target_compile_options(phos PRIVATE /std:c++latest /utf-8)
else()
    target_compile_options(phos PRIVATE -std=c++26 -pthread)
endif()

set_target_properties(phos PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
)
