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
 * parser_driver.h
 *
 * Declares a thin wrapper around the ANTLR4 generated parser for SiSyL.  The
 * ParserDriver exposes a simple parse() method that takes source code as a
 * string or file and returns an AST built by the AstBuilder visitor.  It
 * encapsulates the ANTLR lexer, parser and error listeners so that the rest
 * of the compiler does not depend directly on ANTLR headers.
 */

#ifndef SISYL_PARSER_DRIVER_H
#define SISYL_PARSER_DRIVER_H

#include "ast.h"

#include <expected>
#include <filesystem>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

namespace sisyl {

class ParserDriver {
public:
    ParserDriver();
    
    // Parse the given source string and return a Program AST.  On error
    // returns std::unexpected(errors).
    [[nodiscard]] std::expected<std::shared_ptr<Program>, std::vector<std::string>> parseString(std::string_view source);
    
    // Parse a file from disk.  Convenience wrapper around parseString.
    [[nodiscard]] std::expected<std::shared_ptr<Program>, std::vector<std::string>> parseFile(const std::filesystem::path &path);
};

} // namespace sisyl

#endif // SISYL_PARSER_DRIVER_H