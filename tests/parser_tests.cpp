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
 * parser_tests.cpp
 *
 * Unit tests for the SiSyL parser.
 */

#include <gtest/gtest.h>

#include "parser_driver.h"
#include "semantic_analyzer.h"
#include "type.h"

using namespace sisyl;

TEST(Parser, EmptyProgram) {
    ParserDriver parser;
    auto result = parser.parseString("");
    ASSERT_TRUE(result);
    const auto prog = *result;
    ASSERT_NE(prog, nullptr);
    EXPECT_TRUE(prog->classes.empty());
    EXPECT_TRUE(prog->functions.empty());
}

TEST(Parser, SimpleFunction) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        Void main() {
        }
    )");
    ASSERT_TRUE(result);
    const auto prog = *result;
    ASSERT_EQ(prog->functions.size(), 1u);
    EXPECT_EQ(prog->functions[0]->name, "main");
    EXPECT_EQ(toString(prog->functions[0]->returnType), "Void");
    EXPECT_TRUE(prog->functions[0]->params.empty());
}

TEST(Parser, FunctionWithParams) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        Int64 add(Int64 a, Int64 b) {
            return a + b;
        }
    )");
    ASSERT_TRUE(result);
    const auto prog = *result;
    ASSERT_EQ(prog->functions.size(), 1u);

    const auto &func = prog->functions[0];
    EXPECT_EQ(func->name, "add");
    EXPECT_EQ(toString(func->returnType), "Int64");
    ASSERT_EQ(func->params.size(), 2u);
    EXPECT_EQ(toString(func->params[0].type), "Int64");
    EXPECT_EQ(func->params[0].name, "a");
    EXPECT_EQ(toString(func->params[1].type), "Int64");
    EXPECT_EQ(func->params[1].name, "b");
}

TEST(Parser, ClassDeclaration) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        class Point {
            Int64 x;
            Int64 y;
        }
    )");
    ASSERT_TRUE(result);
    const auto prog = *result;
    ASSERT_EQ(prog->classes.size(), 1u);

    const auto &classDecl = prog->classes[0];
    EXPECT_EQ(classDecl->name, "Point");
    ASSERT_EQ(classDecl->fields.size(), 2u);
    EXPECT_EQ(toString(classDecl->fields[0].first), "Int64");
    EXPECT_EQ(classDecl->fields[0].second, "x");
    EXPECT_EQ(toString(classDecl->fields[1].first), "Int64");
    EXPECT_EQ(classDecl->fields[1].second, "y");
}

TEST(Parser, FieldAccessAssignment) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        class Point { Int64 x; Int64 y; }

        Void test() {
            Point p = new Point();
            p.x = 10;
            p.y = 20;
        }
    )");
    ASSERT_TRUE(result);
}

TEST(Parser, FieldAccessInExpression) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        class Point { Int64 x; Int64 y; }

        Int64 main() {
            Point p = new Point();
            p.x = 4;
            p.y = 3;
            Int64 sum = p.x + p.y;
            return sum;
        }
    )");
    ASSERT_TRUE(result);
}

TEST(Parser, SyntaxError) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        Void test() {
            Int64 x = ;
        }
    )");
    EXPECT_FALSE(result);
    ASSERT_TRUE(result.error().size() >= 1u);
}

///////////////////////////////////////////////////////////////////////////////
// Additional parser tests (migrated from legacy suite)
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Literal Parsing Tests
///////////////////////////////////////////////////////////////////////////////

// Test parsing integer literals
TEST(ParserLegacy, IntLiterals) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        Void test() {
            Int64 a = 0;
            Int64 b = 42;
            Int64 c = 123456;
        }
    )");
    ASSERT_TRUE(result);
}

// Test parsing boolean literals
TEST(ParserLegacy, BoolLiterals) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        Void test() {
            Bool a = true;
            Bool b = false;
        }
    )");
    ASSERT_TRUE(result);
}

// Test parsing string literals
TEST(ParserLegacy, StringLiterals) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        Void test() {
            Str s = "hello world";
            Str empty = "";
        }
    )");
    ASSERT_TRUE(result);
}

///////////////////////////////////////////////////////////////////////////////
// Expression Parsing Tests
///////////////////////////////////////////////////////////////////////////////

// Test arithmetic expressions
TEST(ParserLegacy, ArithmeticExpressions) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        Void test() {
            Int64 a = 1 + 2;
            Int64 b = 3 - 4;
            Int64 c = 5 * 6;
            Int64 d = 7 / 8;
            Int64 e = 1 + 2 * 3;
            Int64 f = (1 + 2) * 3;
        }
    )");
    ASSERT_TRUE(result);
}

// Test comparison expressions
TEST(ParserLegacy, ComparisonExpressions) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        Void test() {
            Bool a = 1 < 2;
            Bool b = 1 > 2;
            Bool c = 1 <= 2;
            Bool d = 1 >= 2;
            Bool e = 1 == 2;
            Bool f = 1 != 2;
        }
    )");
    ASSERT_TRUE(result);
}

// Test logical expressions
TEST(ParserLegacy, LogicalExpressions) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        Void test() {
            Bool a = true && false;
            Bool b = true || false;
            Bool c = !true;
            Bool d = true && false || true;
        }
    )");
    ASSERT_TRUE(result);
}

// Test unary expressions
TEST(ParserLegacy, UnaryExpressions) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        Void test() {
            Int64 a = -1;
            Int64 b = -(-1);
            Bool c = !true;
            Bool d = !!false;
        }
    )");
    ASSERT_TRUE(result);
}

// Test function call expressions
TEST(ParserLegacy, FunctionCallExpressions) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        Int64 getOne() { return 1; }
        Int64 add(Int64 a, Int64 b) { return a + b; }
        
        Void test() {
            Int64 a = getOne();
            Int64 b = add(1, 2);
            Int64 c = add(getOne(), add(1, 2));
        }
    )");
    ASSERT_TRUE(result);
}

// Test new expressions
TEST(ParserLegacy, NewExpressions) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        class Point { Int64 x; Int64 y; }
        
        Void test() {
            Point p = new Point();
        }
    )");
    ASSERT_TRUE(result);
}

///////////////////////////////////////////////////////////////////////////////
// Statement Parsing Tests
///////////////////////////////////////////////////////////////////////////////

// Test variable declarations
TEST(ParserLegacy, VarDeclarations) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        Void test() {
            Int64 x;
            Int64 y = 42;
            Bool flag = true;
            Str name = "test";
        }
    )");
    ASSERT_TRUE(result);
}

// Test assignment statements
TEST(ParserLegacy, AssignmentStatements) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        Void test() {
            Int64 x = 1;
            x = 2;
            x = x + 1;
        }
    )");
    ASSERT_TRUE(result);
}

// Test if statements
TEST(ParserLegacy, IfStatements) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        Void test() {
            Int64 x = 1;
            if (x > 0) {
                x = 0;
            }
            if (x < 0) {
                x = 1;
            } else {
                x = 2;
            }
        }
    )");
    ASSERT_TRUE(result);
}

// Test while statements
TEST(ParserLegacy, WhileStatements) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        Void test() {
            Int64 x = 10;
            while (x > 0) {
                x = x - 1;
            }
        }
    )");
    ASSERT_TRUE(result);
}

// Test return statements
TEST(ParserLegacy, ReturnStatements) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        Void doNothing() {
            return;
        }
        Int64 getOne() {
            return 1;
        }
        Int64 sum(Int64 a, Int64 b) {
            return a + b;
        }
    )");
    ASSERT_TRUE(result);
}

// Test nested blocks
TEST(ParserLegacy, NestedBlocks) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        Void test() {
            Int64 x = 1;
            {
                Int64 y = 2;
                {
                    Int64 z = 3;
                }
            }
        }
    )");
    ASSERT_TRUE(result);
}

///////////////////////////////////////////////////////////////////////////////
// Field Access Parsing Tests
///////////////////////////////////////////////////////////////////////////////

// Test field access in assignments
TEST(ParserLegacy, FieldAccess) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        class Point { Int64 x; Int64 y; }
        
        Void test() {
            Point p = new Point();
            p.x = 10;
            p.y = 20;
        }
    )");
    ASSERT_TRUE(result);
}

///////////////////////////////////////////////////////////////////////////////
// Comment Parsing Tests
///////////////////////////////////////////////////////////////////////////////

// Test that comments are ignored
TEST(ParserLegacy, Comments) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        // This is a comment
        Void main() {
            // Another comment
            Int64 x = 42; // inline comment
        }
    )");
    ASSERT_TRUE(result);
}

///////////////////////////////////////////////////////////////////////////////
// Error Handling Tests
///////////////////////////////////////////////////////////////////////////////

// Test syntax error detection
TEST(ParserLegacy, SyntaxError) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        Void test() {
            Int64 x = ;
        }
    )");
    EXPECT_FALSE(result);
    ASSERT_TRUE(result.error().size() >= 1u);
}

// Test missing semicolon
TEST(ParserLegacy, MissingSemicolon) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        Void test() {
            Int64 x = 1
        }
    )");
    EXPECT_FALSE(result);
    ASSERT_TRUE(result.error().size() >= 1u);
}

// Test missing brace
TEST(ParserLegacy, MissingBrace) {
    ParserDriver parser;
    auto result = parser.parseString(R"(
        Void test() {
            Int64 x = 1;
    )");
    EXPECT_FALSE(result);
    ASSERT_TRUE(result.error().size() >= 1u);
}

///////////////////////////////////////////////////////////////////////////////
// Integration Tests (Parser + Semantic)
///////////////////////////////////////////////////////////////////////////////

// Test that parsed code can be analyzed semantically
TEST(ParserLegacyIntegration, ParseAndAnalyze) {
    ParserDriver parser;
    auto parseResult = parser.parseString(R"(
        Int64 factorial(Int64 n) {
            if (n <= 1) {
                return 1;
            } else {
                return n * factorial(n - 1);
            }
        }
        
        Void main() {
            Int64 result = factorial(5);
        }
    )");
    ASSERT_TRUE(parseResult);
    
    // Run semantic analysis on parsed AST
    SemanticAnalyzer sem;
    EXPECT_TRUE(sem.analyze(*parseResult));
}

// Test ownership tracking works with parsed code
TEST(ParserLegacyIntegration, OwnershipTracking) {
    ParserDriver parser;
    auto parseResult = parser.parseString(R"(
        class Node { Int64 value; }
        
        Void consume(Node n) {
        }
        
        Void test() {
            Node n = new Node();
            consume(n);
        }
    )");
    ASSERT_TRUE(parseResult);
    
    SemanticAnalyzer sem;
    EXPECT_TRUE(sem.analyze(*parseResult));
}

// Test use-after-move detected in parsed code
TEST(ParserLegacyIntegration, UseAfterMoveDetected) {
    ParserDriver parser;
    auto parseResult = parser.parseString(R"(
        class Node { Int64 value; }
        
        Void consume(Node n) {
        }
        
        Void test() {
            Node n = new Node();
            consume(n);
            consume(n);
        }
    )");
    ASSERT_TRUE(parseResult);
    
    SemanticAnalyzer sem;
    auto semResult = sem.analyze(*parseResult);
    EXPECT_FALSE(semResult);
    ASSERT_GE(semResult.error().size(), 1u);
    EXPECT_NE(semResult.error()[0].find("moved"), std::string::npos);
}
