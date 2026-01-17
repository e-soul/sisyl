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
#include "parser_driver.h"
#include "ast_builder.h"

#include "SiSyLLexer.h"
#include "SiSyLParser.h"
#include "antlr4-runtime.h"

#include <any>
#include <filesystem>
#include <fstream>
#include <sstream>

using namespace sisyl;

/**
 * Custom error listener that collects parse errors into a vector.
 */
class ErrorCollector : public antlr4::BaseErrorListener {
  public:
    std::vector<std::string> errors;

    void syntaxError(antlr4::Recognizer * /*recognizer*/, antlr4::Token * /*offendingSymbol*/, size_t line, size_t charPositionInLine, const std::string &msg,
                     std::exception_ptr /*e*/) override {
        std::ostringstream oss;
        oss << "line " << line << ":" << charPositionInLine << " " << msg;
        errors.push_back(oss.str());
    }
};

std::expected<std::shared_ptr<Program>, std::vector<std::string>> ParserDriver::parseString(std::string_view source) {
    std::vector<std::string> errors;

    const std::string sourceStr{source};

    antlr4::ANTLRInputStream input(sourceStr);

    SiSyLLexer lexer(&input);
    ErrorCollector lexerErrors;
    lexer.removeErrorListeners();
    lexer.addErrorListener(&lexerErrors);

    antlr4::CommonTokenStream tokens(&lexer);

    SiSyLParser parser(&tokens);
    ErrorCollector parserErrors;
    parser.removeErrorListeners();
    parser.addErrorListener(&parserErrors);

    SiSyLParser::ProgramContext *tree = parser.program();

    errors.insert(errors.end(), lexerErrors.errors.begin(), lexerErrors.errors.end());
    errors.insert(errors.end(), parserErrors.errors.begin(), parserErrors.errors.end());

    if (!errors.empty()) {
        return std::unexpected(std::move(errors));
    }

    AstBuilder builder;
    auto result = builder.visit(tree);
    return std::any_cast<std::shared_ptr<Program>>(result);
}

std::expected<std::shared_ptr<Program>, std::vector<std::string>> ParserDriver::parseFile(const std::filesystem::path &path) {
    std::ifstream sourceStream(path);
    if (!sourceStream) {
        return std::unexpected(std::vector<std::string>{"Failed to open file: " + path.string()});
    }
    std::ostringstream buf;
    buf << sourceStream.rdbuf();
    return parseString(buf.str());
}
