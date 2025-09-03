cmake_minimum_required(VERSION 3.20)
project(c2math LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# You need LLVM/Clang installed with CMake config files.
# Example on Linux/macOS: -DLLVM_DIR=/usr/lib/cmake/llvm -DClang_DIR=/usr/lib/cmake/clang
find_package(LLVM REQUIRED CONFIG)
find_package(Clang REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
message(STATUS "Using ClangConfig.cmake in: ${Clang_DIR}")

# Common LLVM/Clang setup
include_directories(SYSTEM ${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_executable(c2math src/main.cpp)

# Link only what we need for “syntax-only” parsing
target_link_libraries(c2math
  PRIVATE
    clangTooling
    clangFrontend
    clangAST
    clangBasic
    clangLex
    LLVMOption
)

# Silence MSVC’s min/max macro warnings, etc.
if (MSVC)
  target_compile_definitions(c2math PRIVATE _CRT_SECURE_NO_WARNINGS NOMINMAX)
endif()

