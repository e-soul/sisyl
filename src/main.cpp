/*
   Copyright 2025 e-soul.org
   All rights reserved.
   Redistribution and use in source and binary forms, with or without modification, are permitted
   provided that the following conditions are met:
   1. Redistributions of source code must retain the above copyright notice, this list of conditions
      and the following disclaimer.
   2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions
      and the following disclaimer in the documentation and/or other materials provided with the distribution.
   THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR IMPLIED
   WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
   FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
   FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
   BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/*
 * main.cpp
 *
 * Entry point for the SiSyL compiler.  This driver wires together parsing,
 * semantic analysis and code generation.  It accepts a source file and
 * optional flags for controlling output.
 *
 * Usage: sisylc [options] <source_file>
 *   -o <file>    Output IR to specified file (default: stdout)
 *   --emit-ir    Output LLVM IR (text format)
 *   -h, --help   Show usage information
 */

#include "parser_driver.h"
#include "semantic_analyzer.h"
#include "code_generator.h"
#include <iostream>
#include <filesystem>
#include <string>
#include <cstring>

using namespace sisyl;

void printUsage(const char *programName) {
    std::cerr << "Usage: " << programName << " [options] <source_file>\n"
              << "Options:\n"
              << "  -o <file>    Output IR to specified file (default: stdout)\n"
              << "  --emit-ir    Output LLVM IR (text format)\n"
              << "  -h, --help   Show this help message\n";
}

int main(int argc, char **argv) {
    std::string inputFile;
    std::string outputFile;
    bool emitIR = false;
    
    // Parse command line arguments
    for (int i = 1; i < argc; ++i) {
        if (std::strcmp(argv[i], "-h") == 0 || std::strcmp(argv[i], "--help") == 0) {
            printUsage(argv[0]);
            return 0;
        } else if (std::strcmp(argv[i], "-o") == 0) {
            if (i + 1 < argc) {
                outputFile = argv[++i];
            } else {
                std::cerr << "Error: -o requires an output file argument\n";
                return 1;
            }
        } else if (std::strcmp(argv[i], "--emit-ir") == 0) {
            emitIR = true;
        } else if (argv[i][0] == '-') {
            std::cerr << "Unknown option: " << argv[i] << "\n";
            printUsage(argv[0]);
            return 1;
        } else {
            if (inputFile.empty()) {
                inputFile = argv[i];
            } else {
                std::cerr << "Error: Multiple input files not supported\n";
                return 1;
            }
        }
    }
    
    ParserDriver parser;
    std::expected<std::shared_ptr<Program>, std::vector<std::string>> programResult;

    if (!inputFile.empty()) {
        programResult = parser.parseFile(std::filesystem::path{inputFile});
    } else {
        // Read from stdin
        std::istreambuf_iterator<char> begin(std::cin), end;
        std::string input(begin, end);
        programResult = parser.parseString(input);
    }

    if (!programResult) {
        std::cerr << "Parse errors detected:" << std::endl;
        for (const auto &err : programResult.error()) {
            std::cerr << "  " << err << std::endl;
        }
        return 1;
    }

    const std::shared_ptr<Program> &program = *programResult;

    // Run semantic analysis
    SemanticAnalyzer sem;
    if (auto semResult = sem.analyze(program); !semResult) {
        std::cerr << "Semantic errors detected:" << std::endl;
        for (const auto &diag : semResult.error()) {
            std::cerr << "  " << diag << std::endl;
        }
        return 1;
    }

    // Run code generation
    CodeGenerator codegen;
    auto irResult = codegen.generateIR(program);
    if (!irResult) {
        std::cerr << "Code generation failed:" << std::endl;
        for (const auto &diag : irResult.error()) {
            std::cerr << "  " << diag << std::endl;
        }
        return 1;
    }

    const std::string &irText = *irResult;

    // Output the generated IR
    if (emitIR || !outputFile.empty()) {
        if (!outputFile.empty()) {
            auto writeResult = CodeGenerator::writeIRToFile(std::filesystem::path{outputFile}, irText);
            if (!writeResult) {
                std::cerr << writeResult.error() << std::endl;
                return 1;
            }
            std::cout << "IR written to: " << outputFile << std::endl;
        } else {
            std::cout << irText;
        }
    } else {
        std::cout << "Compilation finished successfully." << std::endl;
    }
    
    return 0;
}