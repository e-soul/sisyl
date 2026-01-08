# SiSyL: A Simple Systems Language

[![CI](https://github.com/e-soul/sisyl/actions/workflows/ci.yml/badge.svg)](https://github.com/e-soul/sisyl/actions/workflows/ci.yml)

SiSyL (Simple Systems Language) is a minimal safe systems programming
language designed to demonstrate that memory safety can be achieved
without a garbage collector, reference counting or a borrow checker. It
features a Java‑like syntax, strict single ownership move semantics and
generates efficient native code through LLVM.

## Motivation

SiSyL takes a more minimalistic approach: each resource has exactly one owner,
ownership can be moved but never duplicated and resources are automatically
freed when their owner goes out of scope.
By enforcing these rules at compile time the language
prevents use‑after‑free, double free and leaks without a runtime collector.

### Grammar

The language syntax is defined using ANTLR4. SiSyL supports:

- Primitive types `Int64`, `Bool` and `Str`.
- Class definitions with typed fields.
- Variable declarations and assignments.
- Free‑standing functions with typed parameters and return values.
- `if`/`else` and `while` control flow.
- Expressions with arithmetic, logical and relational operators.
- Function calls and object allocation with `new`.

### Compiler Architecture

SiSyL's compiler follows a traditional pipeline:

1. **Parsing (front‑end)** – Source text is parsed with ANTLR4 into a parse tree
   and then converted into an AST. This AST is the "compiler's view" of the
   program: it encodes structure (functions, blocks, statements, expressions)
   without depending on parser details.

2. **Semantic analysis (middle‑end)** – The compiler checks meaning, not just
   syntax, including:
   name resolution (what each identifier refers to),
   type checking (whether operators and assignments make sense),
   and enforcing SiSyL's single‑ownership "move" model for non‑primitive values.
   Errors are reported as human‑readable diagnostics.

3. **Code generation (back‑end)** – The compiler lowers the validated AST into
   LLVM IR. This step turns structured control flow, expressions, and data
   definitions into an LLVM module that can be printed as text and then fed
   to LLVM/Clang tooling to produce native code.

## Building

Generating the parser requires ANTLR4 and a Java 11+ runtime.
To build the compiler and tests you need a C++ toolchain and the LLVM development libraries.

- On Windows, install MSVC + Clang, then download and extract [clang+llvm-21.1.8-x86_64-pc-windows-msvc.tar.xz](https://github.com/llvm/llvm-project/releases/download/llvmorg-21.1.8/clang+llvm-21.1.8-x86_64-pc-windows-msvc.tar.xz). Set an env var pointing to the extracted content, e.g. `set LLVM_DIR=C:\clang+llvm-21.1.8-x86_64-pc-windows-msvc`
- On POSIX, see the [build workflow](.github/workflows/ci.yml)

Then run from the repo root:

```
cmake --preset ninja-clangcl-release
cmake --build --preset ninja-clangcl-release
ctest --preset ninja-clangcl-release
```

or

```
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release
cmake --build build --config Release
ctest --test-dir build -C Release --output-on-failure
```

This will produce the `sisylc` compiler executable.

## Usage

The `sisylc` compiler currently emits LLVM IR (text). To produce a native
executable, compile that IR with a tool like `clang`.

### Compile a `.sisyl` file to an executable on Windows.

```sh
# 1) Emit LLVM IR
build/Release/sisylc.exe --emit-ir -o program.ll program.sisyl 

# 2a) Compile LLVM IR to a native executable with statically linked MSVC CRT
clang program.ll -O2 -o program.exe
# 2b) Same as above but link dynamically with clang-cl
clang-cl /MD /O2 /Fe:program.exe program.ll /link msvcrt.lib
# 2c) Same as 2a) but with clang-cl
clang-cl /O2 /Fe:program.exe program.ll /link libcmt.lib

# 3) Run
./program.exe
```
