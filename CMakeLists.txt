#
# Copyright 2025 e-soul.org
# All rights reserved.
# Redistribution and use in source and binary forms, with or without modification, are permitted
# provided that the following conditions are met:
# 1. Redistributions of source code must retain the above copyright notice, this list of conditions
#    and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions
#    and the following disclaimer in the documentation and/or other materials provided with the distribution.
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
# BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

cmake_minimum_required(VERSION 3.20)
project(sisyl C CXX)

# Optional clang-tidy integration (set via -DSISYL_CLANG_TIDY=clang-tidy)
set(SISYL_CLANG_TIDY "" CACHE STRING "Path to clang-tidy executable")

# Use C++23 for the project (ANTLR4 runtime requires at least std::any)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Optionally enable warnings
if (MSVC)
    add_compile_options(/W4)
else()
    add_compile_options(-Wall -Wextra -pedantic)
endif()

# -----------------------------------------------------------------------------
# Find external dependencies
#
# LLVM is required for code generation.
# On many distributions this can be installed via the package manager.
# On Windows, download from https://releases.llvm.org/ or use vcpkg.

find_package(LLVM CONFIG REQUIRED)
message(STATUS "Found LLVM version ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

# Map LLVM components to library names
llvm_map_components_to_libnames(LLVM_LIBS
    core
    support
    irreader
    bitwriter
    passes
    target
    analysis
)

# Use FetchContent for external dependencies
include(FetchContent)

# -----------------------------------------------------------------------------
# ANTLR4 integration
#
# The SiSyL parser is defined in grammar/SiSyL.g4.  We fetch the ANTLR4
# C++ runtime and use a custom function to generate the parser/lexer.
# The ANTLR tool JAR is downloaded automatically if Java is available.

set(ANTLR4_VERSION "4.13.2")

# Fetch ANTLR4 C++ runtime
FetchContent_Declare(
    antlr4_runtime
    URL https://github.com/antlr/antlr4/archive/refs/tags/${ANTLR4_VERSION}.zip
    DOWNLOAD_EXTRACT_TIMESTAMP TRUE
    SOURCE_SUBDIR runtime/Cpp
)

# Only build the runtime, not the entire ANTLR project
set(ANTLR4_INSTALL OFF CACHE BOOL "" FORCE)
set(ANTLR_BUILD_CPP_TESTS OFF CACHE BOOL "" FORCE)
set(ANTLR_BUILD_SHARED OFF CACHE BOOL "" FORCE)

# ANTLR4 generated directory
set(ANTLR4_GENERATED_DIR ${CMAKE_CURRENT_BINARY_DIR}/generated)
file(MAKE_DIRECTORY ${ANTLR4_GENERATED_DIR})

# Download ANTLR4 JAR for code generation
set(ANTLR4_JAR_LOCATION ${CMAKE_CURRENT_BINARY_DIR}/antlr-${ANTLR4_VERSION}-complete.jar)
if(NOT EXISTS ${ANTLR4_JAR_LOCATION})
    message(STATUS "Downloading ANTLR4 tool JAR...")
    file(DOWNLOAD
        https://www.antlr.org/download/antlr-${ANTLR4_VERSION}-complete.jar
        ${ANTLR4_JAR_LOCATION}
        SHOW_PROGRESS
        STATUS ANTLR_DOWNLOAD_STATUS
    )
    list(GET ANTLR_DOWNLOAD_STATUS 0 ANTLR_DOWNLOAD_RESULT)
    if(NOT ANTLR_DOWNLOAD_RESULT EQUAL 0)
        message(FATAL_ERROR "Failed to download ANTLR4 JAR")
    endif()
endif()

# Find Java - required to run ANTLR4 tool
find_package(Java COMPONENTS Runtime REQUIRED)

# Generate lexer and parser from grammar
set(ANTLR4_GRAMMAR ${CMAKE_CURRENT_SOURCE_DIR}/grammar/SiSyL.g4)
set(ANTLR4_GEN_FILES
    ${ANTLR4_GENERATED_DIR}/SiSyLLexer.cpp
    ${ANTLR4_GENERATED_DIR}/SiSyLLexer.h
    ${ANTLR4_GENERATED_DIR}/SiSyLParser.cpp
    ${ANTLR4_GENERATED_DIR}/SiSyLParser.h
    ${ANTLR4_GENERATED_DIR}/SiSyLBaseVisitor.cpp
    ${ANTLR4_GENERATED_DIR}/SiSyLBaseVisitor.h
    ${ANTLR4_GENERATED_DIR}/SiSyLVisitor.cpp
    ${ANTLR4_GENERATED_DIR}/SiSyLVisitor.h
)
set_source_files_properties(${ANTLR4_GEN_FILES} PROPERTIES SKIP_LINTING ON)

add_custom_command(
    OUTPUT ${ANTLR4_GEN_FILES}
    COMMAND ${Java_JAVA_EXECUTABLE} -jar ${ANTLR4_JAR_LOCATION}
            -Dlanguage=Cpp
            -visitor
            -no-listener
            -o ${ANTLR4_GENERATED_DIR}
            ${ANTLR4_GRAMMAR}
    DEPENDS ${ANTLR4_GRAMMAR}
    COMMENT "Generating ANTLR4 parser from SiSyL.g4"
    VERBATIM
)

# Custom target to ensure generation happens
add_custom_target(generate_parser DEPENDS ${ANTLR4_GEN_FILES})

# Make ANTLR4 runtime available
FetchContent_MakeAvailable(antlr4_runtime)

# Workaround (MSVC): some ANTLR4 C++ runtime releases ship with
# ProfilingATNSimulator.cpp missing #include <chrono>, which breaks
# std::chrono::high_resolution_clock usage.
if (MSVC)
    set(PROFILING_FILE "${antlr4_runtime_SOURCE_DIR}/runtime/Cpp/runtime/src/atn/ProfilingATNSimulator.cpp")
    if (EXISTS "${PROFILING_FILE}")
        file(READ "${PROFILING_FILE}" PROFILING_CONTENT)
        string(FIND "${PROFILING_CONTENT}" "#include <chrono>" HAS_CHRONO)
        if(HAS_CHRONO EQUAL -1)
            string(REPLACE "using namespace std::chrono;" "#include <chrono>\nusing namespace std::chrono;" PROFILING_CONTENT "${PROFILING_CONTENT}")
            file(WRITE "${PROFILING_FILE}" "${PROFILING_CONTENT}")
            message(STATUS "Patched ANTLR4 ProfilingATNSimulator.cpp to include <chrono> (MSVC workaround)")
        endif()
    endif()
endif()

# Include directories for ANTLR4
set(ANTLR4_INCLUDE_DIRS
    ${antlr4_runtime_SOURCE_DIR}/runtime/Cpp/runtime/src
    ${ANTLR4_GENERATED_DIR}
)

# -----------------------------------------------------------------------------
# GoogleTest integration
#
# For unit tests we use GoogleTest.  This can be downloaded via
# FetchContent or added as a submodule.  The tests directory contains
# skeleton test files.

FetchContent_Declare(
    googletest
    URL https://github.com/google/googletest/archive/refs/tags/v1.15.2.zip
    DOWNLOAD_EXTRACT_TIMESTAMP TRUE
)
# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)
enable_testing()

# Build a library from the compiler sources (excluding main.cpp) for testing
file(GLOB SISYL_LIB_SOURCES
    src/ast.cpp
    src/semantic_analyzer.cpp
    src/code_generator.cpp
    src/parser_driver.cpp
    src/ast_builder.cpp
)
add_library(sisyl_lib STATIC ${SISYL_LIB_SOURCES} ${ANTLR4_GEN_FILES})
add_dependencies(sisyl_lib generate_parser)
target_include_directories(sisyl_lib PUBLIC include ${ANTLR4_INCLUDE_DIRS})
target_link_libraries(sisyl_lib antlr4_static)
target_link_libraries(sisyl_lib ${LLVM_LIBS})

function(enable_sisyl_clang_tidy target_name)
    if (SISYL_CLANG_TIDY)
        set_target_properties(${target_name} PROPERTIES CXX_CLANG_TIDY "${SISYL_CLANG_TIDY};--header-filter=^${CMAKE_SOURCE_DIR}/(include|src)/")
    endif()
endfunction()

enable_sisyl_clang_tidy(sisyl_lib)

# Test executable - semantic tests
add_executable(sisyl_tests tests/semantic_tests.cpp)
target_include_directories(sisyl_tests PRIVATE include)
target_link_libraries(sisyl_tests sisyl_lib gtest_main)
include(GoogleTest)
gtest_discover_tests(sisyl_tests)
enable_sisyl_clang_tidy(sisyl_tests)

# Test executable - parser tests
add_executable(parser_tests tests/parser_tests.cpp)
target_include_directories(parser_tests PRIVATE include ${ANTLR4_INCLUDE_DIRS})
target_link_libraries(parser_tests sisyl_lib gtest_main)
gtest_discover_tests(parser_tests)
enable_sisyl_clang_tidy(parser_tests)

# Test executable - code generator tests
add_executable(codegen_tests tests/codegen_tests.cpp)
target_include_directories(codegen_tests PRIVATE include)
target_link_libraries(codegen_tests sisyl_lib gtest_main)
gtest_discover_tests(codegen_tests)
enable_sisyl_clang_tidy(codegen_tests)

# -----------------------------------------------------------------------------
# Compiler sources
#
file(GLOB SISYL_SOURCES
    src/*.cpp
)

# Define the sisylc compiler executable.
add_executable(sisylc ${SISYL_SOURCES} ${ANTLR4_GEN_FILES})
add_dependencies(sisylc generate_parser)

# Public include directory for our headers
target_include_directories(sisylc PUBLIC include ${ANTLR4_INCLUDE_DIRS})

# Link against ANTLR4 runtime
target_link_libraries(sisylc antlr4_static)

# Link against LLVM components (required).
target_link_libraries(sisylc ${LLVM_LIBS})
enable_sisyl_clang_tidy(sisylc)
