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
 * semantic_tests.cpp
 *
 * Unit tests for the semantic analysis phase of the SiSyL compiler.
 *
 * These tests build ASTs directly (no parser required) and validate
 * std::expected-based diagnostics from SemanticAnalyzer.
 */

#include <gtest/gtest.h>

#include "semantic_analyzer.h"

#include <string_view>

using namespace sisyl;

static Type T(std::string_view s) {
    return parseType(s);
}

static Type S(std::string_view s) {
    return Type{std::string{s}};
}

static std::shared_ptr<FuncDecl> makeFunc(Type retType,
                                          std::string name,
                                          std::vector<Parameter> params,
                                          std::shared_ptr<BlockStmt> body) {
    return std::make_shared<FuncDecl>(std::move(retType), std::move(name), std::move(params), std::move(body));
}

TEST(Semantic, EmptyProgram) {
    auto prog = std::make_shared<Program>();
    SemanticAnalyzer sem;
    EXPECT_TRUE(sem.analyze(prog));
}

TEST(Semantic, RedeclarationSameScope) {
    auto block = std::make_shared<BlockStmt>();
    block->statements.push_back(std::make_shared<VarDeclStmt>(T("Int64"), "x"));
    block->statements.push_back(std::make_shared<VarDeclStmt>(T("Int64"), "x"));

    auto func = makeFunc(T("Void"), "f", {}, block);
    auto prog = std::make_shared<Program>();
    prog->functions.push_back(func);

    SemanticAnalyzer sem;
    auto result = sem.analyze(prog);
    EXPECT_FALSE(result);
    ASSERT_GE(result.error().size(), 1u);
}

TEST(Ownership, UseAfterMove) {
    // class Point { Int64 x; }
    auto pointClass = std::make_shared<ClassDecl>("Point",
        std::vector<std::pair<Type, std::string>>{{T("Int64"), "x"}});

    auto block = std::make_shared<BlockStmt>();
    block->statements.push_back(std::make_shared<VarDeclStmt>(S("Point"), "a", std::make_shared<NewExpr>("Point")));
    block->statements.push_back(std::make_shared<VarDeclStmt>(S("Point"), "b", std::make_shared<VarRef>("a")));
    block->statements.push_back(std::make_shared<VarDeclStmt>(S("Point"), "c", std::make_shared<VarRef>("a")));

    auto func = makeFunc(T("Void"), "test", {}, block);
    auto prog = std::make_shared<Program>();
    prog->classes.push_back(pointClass);
    prog->functions.push_back(func);

    SemanticAnalyzer sem;
    auto result = sem.analyze(prog);
    EXPECT_FALSE(result);
    ASSERT_GE(result.error().size(), 1u);
    EXPECT_NE(result.error()[0].find("moved"), std::string::npos);
}

TEST(Ownership, Int64IsCopiedNotMoved) {
    auto block = std::make_shared<BlockStmt>();
    block->statements.push_back(std::make_shared<VarDeclStmt>(T("Int64"), "a", std::make_shared<IntLiteral>(42)));
    block->statements.push_back(std::make_shared<VarDeclStmt>(T("Int64"), "b", std::make_shared<VarRef>("a")));
    block->statements.push_back(std::make_shared<VarDeclStmt>(T("Int64"), "c", std::make_shared<VarRef>("a")));

    auto func = makeFunc(T("Void"), "test", {}, block);
    auto prog = std::make_shared<Program>();
    prog->functions.push_back(func);

    SemanticAnalyzer sem;
    EXPECT_TRUE(sem.analyze(prog));
}

TEST(Ownership, MoveViaAssignment) {
    auto pointClass = std::make_shared<ClassDecl>("Point",
        std::vector<std::pair<Type, std::string>>{{T("Int64"), "x"}});

    auto block = std::make_shared<BlockStmt>();
    block->statements.push_back(std::make_shared<VarDeclStmt>(S("Point"), "a", std::make_shared<NewExpr>("Point")));
    block->statements.push_back(std::make_shared<VarDeclStmt>(S("Point"), "b"));
    block->statements.push_back(std::make_shared<AssignStmt>(std::make_shared<VarRef>("b"), std::make_shared<VarRef>("a")));
    block->statements.push_back(std::make_shared<VarDeclStmt>(S("Point"), "c", std::make_shared<VarRef>("a")));

    auto func = makeFunc(T("Void"), "test", {}, block);
    auto prog = std::make_shared<Program>();
    prog->classes.push_back(pointClass);
    prog->functions.push_back(func);

    SemanticAnalyzer sem;
    EXPECT_FALSE(sem.analyze(prog));
}

TEST(TypeCheck, InitializationMismatch) {
    auto block = std::make_shared<BlockStmt>();
    block->statements.push_back(std::make_shared<VarDeclStmt>(T("Int64"), "x", std::make_shared<BoolLiteral>(true)));

    auto func = makeFunc(T("Void"), "test", {}, block);
    auto prog = std::make_shared<Program>();
    prog->functions.push_back(func);

    SemanticAnalyzer sem;
    auto result = sem.analyze(prog);
    EXPECT_FALSE(result);
    ASSERT_GE(result.error().size(), 1u);
}

TEST(TypeCheck, AssignmentMismatch) {
    auto block = std::make_shared<BlockStmt>();
    block->statements.push_back(std::make_shared<VarDeclStmt>(T("Int64"), "x", std::make_shared<IntLiteral>(5)));
    block->statements.push_back(std::make_shared<AssignStmt>(std::make_shared<VarRef>("x"), std::make_shared<BoolLiteral>(false)));

    auto func = makeFunc(T("Void"), "test", {}, block);
    auto prog = std::make_shared<Program>();
    prog->functions.push_back(func);

    SemanticAnalyzer sem;
    EXPECT_FALSE(sem.analyze(prog));
}

TEST(TypeCheck, ReturnTypeMismatch) {
    auto block = std::make_shared<BlockStmt>();
    block->statements.push_back(std::make_shared<ReturnStmt>(std::make_shared<BoolLiteral>(false)));

    auto func = makeFunc(T("Int64"), "getInt", {}, block);
    auto prog = std::make_shared<Program>();
    prog->functions.push_back(func);

    SemanticAnalyzer sem;
    auto result = sem.analyze(prog);
    EXPECT_FALSE(result);
    ASSERT_GE(result.error().size(), 1u);
}

TEST(TypeCheck, ReturnTypeCorrect) {
    auto block = std::make_shared<BlockStmt>();
    block->statements.push_back(std::make_shared<ReturnStmt>(std::make_shared<IntLiteral>(42)));

    auto func = makeFunc(T("Int64"), "getInt", {}, block);
    auto prog = std::make_shared<Program>();
    prog->functions.push_back(func);

    SemanticAnalyzer sem;
    EXPECT_TRUE(sem.analyze(prog));
}

TEST(TypeCheck, ReturnValueFromVoid) {
    auto block = std::make_shared<BlockStmt>();
    block->statements.push_back(std::make_shared<ReturnStmt>(std::make_shared<IntLiteral>(1)));

    auto func = makeFunc(T("Void"), "doNothing", {}, block);
    auto prog = std::make_shared<Program>();
    prog->functions.push_back(func);

    SemanticAnalyzer sem;
    EXPECT_FALSE(sem.analyze(prog));
}

TEST(TypeCheck, BareReturnInVoidOk) {
    auto block = std::make_shared<BlockStmt>();
    block->statements.push_back(std::make_shared<ReturnStmt>(nullptr));

    auto func = makeFunc(T("Void"), "doNothing", {}, block);
    auto prog = std::make_shared<Program>();
    prog->functions.push_back(func);

    SemanticAnalyzer sem;
    EXPECT_TRUE(sem.analyze(prog));
}

TEST(TypeCheck, BareReturnInNonVoidErrors) {
    auto block = std::make_shared<BlockStmt>();
    block->statements.push_back(std::make_shared<ReturnStmt>(nullptr));

    auto func = makeFunc(T("Int64"), "getInt", {}, block);
    auto prog = std::make_shared<Program>();
    prog->functions.push_back(func);

    SemanticAnalyzer sem;
    EXPECT_FALSE(sem.analyze(prog));
}

TEST(Ownership, StrIsCopiedNotMoved) {
    auto block = std::make_shared<BlockStmt>();
    block->statements.push_back(
        std::make_shared<VarDeclStmt>(T("Str"), "s1", std::make_shared<StringLiteral>("hello")));
    block->statements.push_back(
        std::make_shared<VarDeclStmt>(T("Str"), "s2", std::make_shared<VarRef>("s1")));
    block->statements.push_back(
        std::make_shared<VarDeclStmt>(T("Str"), "s3", std::make_shared<VarRef>("s1")));

    auto func = makeFunc(T("Void"), "test", {}, block);
    auto prog = std::make_shared<Program>();
    prog->functions.push_back(func);

    SemanticAnalyzer sem;
    EXPECT_TRUE(sem.analyze(prog));
}

///////////////////////////////////////////////////////////////////////////////
// Literal Type Tests
///////////////////////////////////////////////////////////////////////////////

TEST(TypeCheck, LiteralTypes) {
    auto block = std::make_shared<BlockStmt>();
    block->statements.push_back(std::make_shared<VarDeclStmt>(T("Int64"), "a", std::make_shared<IntLiteral>(42)));
    block->statements.push_back(std::make_shared<VarDeclStmt>(T("Bool"), "b", std::make_shared<BoolLiteral>(true)));
    block->statements.push_back(std::make_shared<VarDeclStmt>(T("Str"), "c", std::make_shared<StringLiteral>("hello")));

    auto func = makeFunc(T("Void"), "test", {}, block);
    auto prog = std::make_shared<Program>();
    prog->functions.push_back(func);

    SemanticAnalyzer sem;
    EXPECT_TRUE(sem.analyze(prog));
}

TEST(TypeCheck, WrongLiteralType) {
    auto block = std::make_shared<BlockStmt>();
    block->statements.push_back(
        std::make_shared<VarDeclStmt>(T("Int64"), "a", std::make_shared<StringLiteral>("hello")));

    auto func = makeFunc(T("Void"), "test", {}, block);
    auto prog = std::make_shared<Program>();
    prog->functions.push_back(func);

    SemanticAnalyzer sem;
    auto result = sem.analyze(prog);
    EXPECT_FALSE(result);
    ASSERT_GE(result.error().size(), 1u);
}

