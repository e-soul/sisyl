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

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

option(SISYL_ENABLE_CLANG_TIDY "Run clang-tidy" OFF)

# Configure clang-tidy for a target
function(sisyl_configure_target targetName)
    if (NOT SISYL_ENABLE_CLANG_TIDY)
        return()
    endif()

    find_program(_sisyl_clang_tidy_exe NAMES clang-tidy-21 clang-tidy)
    if (NOT _sisyl_clang_tidy_exe)
        message(WARNING "SISYL_ENABLE_CLANG_TIDY is ON but clang-tidy was not found; skipping clang-tidy for target '${targetName}'.")
        return()
    endif()

    set(_sisyl_tidy_args "${_sisyl_clang_tidy_exe}" "--header-filter=^${CMAKE_SOURCE_DIR}/(include|src|tests)/")
    if (MSVC)
        list(APPEND _sisyl_tidy_args "--extra-arg=/EHsc")
    endif()
    set_target_properties(${targetName} PROPERTIES CXX_CLANG_TIDY "${_sisyl_tidy_args}")
endfunction()

# LLVM is required for code generation.
find_package(LLVM CONFIG REQUIRED)
message(STATUS "Found LLVM version ${LLVM_PACKAGE_VERSION}")
set(SISYL_LLVM_LIBS "")
if (TARGET LLVM)
    set(SISYL_LLVM_LIBS LLVM)
else()
    if (COMMAND llvm_map_components_to_libnames)
        llvm_map_components_to_libnames(SISYL_LLVM_LIBS core support analysis)
    else()
        message(FATAL_ERROR "LLVMConfig.cmake did not provide llvm_map_components_to_libnames() and no monolithic LLVM target was found.")
    endif()
endif()
message(STATUS "Using LLVM libraries: ${SISYL_LLVM_LIBS}")

# Java is required to run ANTLR4 tool
find_package(Java COMPONENTS Runtime REQUIRED)

# Use FetchContent for external dependencies
include(FetchContent)

# ANTLR4 integration
set(ANTLR4_VERSION "4.13.2")

# Only build the runtime, not the entire ANTLR project
set(ANTLR4_INSTALL OFF CACHE BOOL "" FORCE)
set(ANTLR_BUILD_CPP_TESTS OFF CACHE BOOL "" FORCE)
set(ANTLR_BUILD_SHARED OFF CACHE BOOL "" FORCE)

set(ANTLR4_GENERATED_DIR ${CMAKE_CURRENT_BINARY_DIR}/generated)

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

set(ANTLR4_GRAMMAR ${CMAKE_CURRENT_SOURCE_DIR}/grammar/SiSyL.g4)

# ANTLR4 JAR for code generation
set(ANTLR4_JAR_LOCATION ${CMAKE_CURRENT_BINARY_DIR}/antlr-${ANTLR4_VERSION}-complete.jar)

# Fetch ANTLR4 C++ runtime
FetchContent_Declare(
    antlr4_runtime
    URL https://github.com/antlr/antlr4/archive/refs/tags/${ANTLR4_VERSION}.zip
    DOWNLOAD_EXTRACT_TIMESTAMP TRUE
    SOURCE_SUBDIR runtime/Cpp
)

if(NOT EXISTS ${ANTLR4_JAR_LOCATION})
    message(STATUS "Downloading ANTLR4 JAR...")
    file(DOWNLOAD
        https://www.antlr.org/download/antlr-${ANTLR4_VERSION}-complete.jar
        ${ANTLR4_JAR_LOCATION}
        STATUS ANTLR_DOWNLOAD_STATUS
    )
    list(GET ANTLR_DOWNLOAD_STATUS 0 ANTLR_DOWNLOAD_RESULT)
    if(NOT ANTLR_DOWNLOAD_RESULT EQUAL 0)
        message(FATAL_ERROR "Failed to download ANTLR4 JAR")
    endif()
    message(STATUS "ANTLR4 JAR downloaded successfully to ${ANTLR4_JAR_LOCATION}")
endif()

add_custom_command(
    OUTPUT ${ANTLR4_GEN_FILES}
    COMMAND ${CMAKE_COMMAND} -E make_directory ${ANTLR4_GENERATED_DIR}
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

# Generated lexer/parser sources live in their own target so we can opt them out
# of clang-tidy even when the rest of the project enables it.
add_library(sisyl_parser STATIC ${ANTLR4_GEN_FILES})
target_include_directories(sisyl_parser SYSTEM PUBLIC ${antlr4_runtime_SOURCE_DIR}/runtime/Cpp/runtime/src ${ANTLR4_GENERATED_DIR})
target_link_libraries(sisyl_parser PUBLIC antlr4_static)

# GoogleTest integration
FetchContent_Declare(
    googletest
    URL https://github.com/google/googletest/archive/refs/tags/v1.17.0.zip
    DOWNLOAD_EXTRACT_TIMESTAMP TRUE
)
FetchContent_MakeAvailable(googletest)
enable_testing()

# Build a library from the compiler sources (excluding main.cpp) for testing
set(SISYL_LIB_SOURCES 
    src/ast.cpp
    src/semantic_analyzer.cpp
    src/code_generator.cpp
    src/parser_driver.cpp
    src/ast_builder.cpp
)
add_library(sisyl_lib STATIC ${SISYL_LIB_SOURCES})
add_dependencies(sisyl_lib generate_parser)
target_include_directories(sisyl_lib PUBLIC include)
target_include_directories(sisyl_lib SYSTEM PUBLIC ${LLVM_INCLUDE_DIRS})
target_link_libraries(sisyl_lib PUBLIC ${SISYL_LLVM_LIBS} sisyl_parser)
sisyl_configure_target(sisyl_lib)

# Test executable
add_executable(sisyl_tests
    tests/semantic_tests.cpp
    tests/parser_tests.cpp
    tests/codegen_tests.cpp
)
target_link_libraries(sisyl_tests sisyl_lib gtest_main)
include(GoogleTest)
gtest_discover_tests(sisyl_tests)
sisyl_configure_target(sisyl_tests)

# Compiler executable
add_executable(sisylc src/main.cpp)
target_link_libraries(sisylc PRIVATE sisyl_lib)
sisyl_configure_target(sisylc)
