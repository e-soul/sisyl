# Copilot coding agent instructions (sisyl)

## What this repo is
- **SiSyL** is a small compiler for a "simple systems language".
- Pipeline: **ANTLR4 parser** → **AST** → **semantic analysis (types + ownership/move)** → **LLVM IR generation**.
- Primary deliverables:
  - `sisylc` command-line compiler (emits LLVM IR with `--emit-ir`).
  - GoogleTest-based unit tests for parser/semantics/codegen.

## High-level repo facts
- Project type: **CMake-based** project.
- Languages: **C++23**, **ANTLR4 grammar** (`.g4`), CMake.
- Typical size/shape: small, few directories; most work happens in `include/` + `src/` + `tests/`.

## Key dependencies (and what is required)
- **Required to configure/build:**
  - CMake (>= 3.20).
  - A C++23-capable toolchain (**Visual Studio 2022** on Windows).
  - **Java runtime** (required at CMake configure time): CMake runs ANTLR’s tool JAR to generate the parser.
- **Downloaded automatically at configure time (needs internet on first configure):**
  - ANTLR4 C++ runtime (FetchContent) and the ANTLR tool JAR.
  - GoogleTest (FetchContent).
- **Required:** LLVM (IR generation).
  - CMake uses `find_package(LLVM CONFIG REQUIRED)`.

## Golden-path build / test / run (Windows PowerShell)

### Clean configure (recommended)

```powershell
Remove-Item -Recurse -Force build-clangcl -ErrorAction SilentlyContinue
cmake --preset ninja-clangcl-release
```
Notes:
- First configure will download dependencies (ANTLR runtime + GoogleTest + ANTLR tool JAR).
- CMake applies a small patch inside the fetched ANTLR runtime (`ProfilingATNSimulator.cpp` adds `#include <chrono>`) to avoid an MSVC build issue.

### Build
```powershell
cmake --build --preset ninja-clangcl-release > build-out.txt
```
Notes:
- Build output is redirected to `build-out.txt` for easier analysis, as the output can be very lengthy. It is recommended to use tools to search the file for errors, such as `Select-String -Path build-out.txt -Pattern "error"`.

### Test
```powershell
ctest --preset ninja-clangcl-release
```

### Run (smoke test)
```powershell
.\build\Release\sisylc.exe --help
.\build\Release\sisylc.exe --emit-ir .\tests\test_program.sisyl
```

## If builds fail (common pitfalls)
- **No Java / Java not found**: CMake configure fails because ANTLR codegen requires Java.
  - Fix by installing Java and re-running configure.
- **Offline / restricted network**: configure fails because FetchContent can’t download ANTLR/GoogleTest/JAR.
  - Fix by allowing network access or pre-populating CMake’s download cache.
- **LLVM issues**:
  - If LLVM is missing or incompatible, configure/build will fail.
  - Fix by installing a compatible LLVM and/or pointing `LLVM_DIR` at its `lib/cmake/llvm`.

## Project layout (where to change things)
- Root build definition: [CMakeLists.txt](../CMakeLists.txt)
  - Fetches ANTLR4 runtime + GoogleTest, downloads the ANTLR tool JAR, defines the `generate_parser` target.
  - Generated parser/lexer sources are written under the build tree: `build_*/generated/`.
- Language grammar:
  - [grammar/SiSyL.g4](../grammar/SiSyL.g4)
- Public headers (AST + compiler phases):
  - [include/ast.h](../include/ast.h)
  - [include/ast_builder.h](../include/ast_builder.h)
  - [include/parser_driver.h](../include/parser_driver.h)
  - [include/semantic_analyzer.h](../include/semantic_analyzer.h)
  - [include/code_generator.h](../include/code_generator.h)
- Main implementation:
  - [src/main.cpp](../src/main.cpp) (CLI driver)
  - [src/ast.cpp](../src/ast.cpp) (AST node accept methods)
  - [src/ast_builder.cpp](../src/ast_builder.cpp) (parse-tree visitor constructing AST)
  - [src/parser_driver.cpp](../src/parser_driver.cpp) (ANTLR parse → AST)
  - [src/semantic_analyzer.cpp](../src/semantic_analyzer.cpp)
  - [src/code_generator.cpp](../src/code_generator.cpp)
- Tests (GoogleTest + CTest integration via `gtest_discover_tests`):
  - [tests/semantic_tests.cpp](../tests/semantic_tests.cpp)
  - [tests/parser_tests.cpp](../tests/parser_tests.cpp)
  - [tests/codegen_tests.cpp](../tests/codegen_tests.cpp)
  - [tests/test_program.sisyl](../tests/test_program.sisyl) (sample input)

## Repo conventions (to avoid rejected PRs)
- Prefer minimal, focused changes; avoid reformatting (no repo-wide formatter config is present).
- After changing grammar or AST shape, always run:
  - configure (if needed) → build → `ctest`.
- Trust this file's commands first; only search the repo if these instructions are incomplete or become outdated.

## Root contents (quick reference)
- [CMakeLists.txt](../CMakeLists.txt) — build + dependencies + code generation
- [README.md](../README.md) — overview
- `grammar/`, `include/`, `src/`, `tests/`
