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

#include <gtest/gtest.h>
#include "code_generator.h"

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

TEST(CodeGen, EmptyProgram) {
    auto prog = std::make_shared<Program>();
    CodeGenerator codegen;
    auto ir = codegen.generateIR(prog);
    EXPECT_TRUE(ir);
    EXPECT_FALSE(ir->empty());
}

TEST(CodeGen, SimpleInt64Return) {
    auto block = std::make_shared<BlockStmt>();
    block->statements.push_back(std::make_shared<ReturnStmt>(std::make_shared<IntLiteral>(42)));

    auto func = makeFunc(T("Int64"), "getAnswer", {}, block);
    auto prog = std::make_shared<Program>();
    prog->functions.push_back(func);

    CodeGenerator codegen;
    auto irResult = codegen.generateIR(prog);
    ASSERT_TRUE(irResult);

    const std::string &ir = *irResult;
    EXPECT_NE(ir.find("define i64 @getAnswer("), std::string::npos);
    EXPECT_NE(ir.find("ret i64 42"), std::string::npos);
}

TEST(CodeGen, FunctionWithParams) {
    auto block = std::make_shared<BlockStmt>();
    auto addExpr = std::make_shared<BinaryOp>(
        std::make_shared<VarRef>("a"),
        "+",
        std::make_shared<VarRef>("b"));
    block->statements.push_back(std::make_shared<ReturnStmt>(addExpr));

    std::vector<Parameter> params = {{T("Int64"), "a"}, {T("Int64"), "b"}};
    auto func = makeFunc(T("Int64"), "add", std::move(params), block);
    auto prog = std::make_shared<Program>();
    prog->functions.push_back(func);

    CodeGenerator codegen;
    auto irResult = codegen.generateIR(prog);
    ASSERT_TRUE(irResult);

    const std::string &ir = *irResult;
    EXPECT_NE(ir.find("define i64 @add("), std::string::npos);
    EXPECT_NE(ir.find("add i64"), std::string::npos);
}

TEST(CodeGen, Int64VariableDeclarationAndLoad) {
    auto block = std::make_shared<BlockStmt>();
    block->statements.push_back(std::make_shared<VarDeclStmt>(T("Int64"), "x", std::make_shared<IntLiteral>(10)));
    block->statements.push_back(std::make_shared<ReturnStmt>(std::make_shared<VarRef>("x")));

    auto func = makeFunc(T("Int64"), "getX", {}, block);
    auto prog = std::make_shared<Program>();
    prog->functions.push_back(func);

    CodeGenerator codegen;
    auto irResult = codegen.generateIR(prog);
    ASSERT_TRUE(irResult);

    const std::string &ir = *irResult;
    EXPECT_NE(ir.find("alloca i64"), std::string::npos);
    EXPECT_NE(ir.find("store i64 10"), std::string::npos);
}

TEST(CodeGen, IfStatement) {
    auto thenBlock = std::make_shared<BlockStmt>();
    thenBlock->statements.push_back(std::make_shared<ReturnStmt>(std::make_shared<IntLiteral>(1)));

    auto elseBlock = std::make_shared<BlockStmt>();
    elseBlock->statements.push_back(std::make_shared<ReturnStmt>(std::make_shared<IntLiteral>(0)));

    auto body = std::make_shared<BlockStmt>();
    body->statements.push_back(std::make_shared<IfStmt>(std::make_shared<BoolLiteral>(true), thenBlock, elseBlock));

    auto func = makeFunc(T("Int64"), "testIf", {}, body);
    auto prog = std::make_shared<Program>();
    prog->functions.push_back(func);

    CodeGenerator codegen;
    auto irResult = codegen.generateIR(prog);
    ASSERT_TRUE(irResult);

    const std::string &ir = *irResult;
    EXPECT_NE(ir.find("br i1"), std::string::npos);
    EXPECT_NE(ir.find("then:"), std::string::npos);
    EXPECT_NE(ir.find("else:"), std::string::npos);
}

TEST(CodeGen, WhileLoop) {
    auto body = std::make_shared<BlockStmt>();
    body->statements.push_back(std::make_shared<VarDeclStmt>(T("Int64"), "i", std::make_shared<IntLiteral>(0)));

    auto loopBody = std::make_shared<BlockStmt>();
    loopBody->statements.push_back(std::make_shared<AssignStmt>(
        std::make_shared<VarRef>("i"),
        std::make_shared<BinaryOp>(std::make_shared<VarRef>("i"), "+", std::make_shared<IntLiteral>(1))));

    body->statements.push_back(std::make_shared<WhileStmt>(
        std::make_shared<BinaryOp>(std::make_shared<VarRef>("i"), "<", std::make_shared<IntLiteral>(10)),
        loopBody));
    body->statements.push_back(std::make_shared<ReturnStmt>(std::make_shared<VarRef>("i")));

    auto func = makeFunc(T("Int64"), "testWhile", {}, body);
    auto prog = std::make_shared<Program>();
    prog->functions.push_back(func);

    CodeGenerator codegen;
    auto irResult = codegen.generateIR(prog);
    ASSERT_TRUE(irResult);

    const std::string &ir = *irResult;
    EXPECT_NE(ir.find("whilecond:"), std::string::npos);
    EXPECT_NE(ir.find("whilebody:"), std::string::npos);
    EXPECT_NE(ir.find("whileend:"), std::string::npos);
}

TEST(CodeGen, StructTypeAndNewUsesMalloc) {
    auto prog = std::make_shared<Program>();
    prog->classes.push_back(std::make_shared<ClassDecl>(
        "Point",
        std::vector<std::pair<Type, std::string>>{{T("Int64"), "x"}, {T("Int64"), "y"}}));

    auto body = std::make_shared<BlockStmt>();
    body->statements.push_back(std::make_shared<VarDeclStmt>(S("Point"), "p", std::make_shared<NewExpr>("Point")));
    body->statements.push_back(std::make_shared<ReturnStmt>(std::make_shared<IntLiteral>(0)));
    prog->functions.push_back(makeFunc(T("Int64"), "main", {}, body));

    CodeGenerator codegen;
    auto irResult = codegen.generateIR(prog);
    ASSERT_TRUE(irResult);

    const std::string &ir = *irResult;
    EXPECT_NE(ir.find("%Point = type"), std::string::npos);
    EXPECT_NE(ir.find("{ i64, i64 }"), std::string::npos);
    EXPECT_NE(ir.find("@malloc"), std::string::npos);
}

TEST(CodeGen, BinaryOperations) {
    auto block = std::make_shared<BlockStmt>();
    block->statements.push_back(std::make_shared<VarDeclStmt>(T("Int64"), "a", std::make_shared<IntLiteral>(1)));
    block->statements.push_back(std::make_shared<VarDeclStmt>(T("Int64"), "b", std::make_shared<IntLiteral>(2)));
    block->statements.push_back(std::make_shared<VarDeclStmt>(T("Int64"), "c", std::make_shared<IntLiteral>(3)));
    block->statements.push_back(std::make_shared<VarDeclStmt>(T("Int64"), "d", std::make_shared<IntLiteral>(4)));

    auto expr = std::make_shared<BinaryOp>(
        std::make_shared<BinaryOp>(
            std::make_shared<BinaryOp>(std::make_shared<VarRef>("a"), "+", std::make_shared<VarRef>("b")),
            "*",
            std::make_shared<BinaryOp>(std::make_shared<VarRef>("c"), "-", std::make_shared<VarRef>("d"))),
        "/",
        std::make_shared<VarRef>("a"));
    block->statements.push_back(std::make_shared<ReturnStmt>(expr));

    auto prog = std::make_shared<Program>();
    prog->functions.push_back(makeFunc(T("Int64"), "testArith", {}, block));

    CodeGenerator codegen;
    auto irResult = codegen.generateIR(prog);
    ASSERT_TRUE(irResult);

    const std::string &ir = *irResult;
    EXPECT_NE(ir.find("add i64"), std::string::npos);
    EXPECT_NE(ir.find("sub i64"), std::string::npos);
    EXPECT_NE(ir.find("mul i64"), std::string::npos);
    EXPECT_NE(ir.find("sdiv i64"), std::string::npos);
}

TEST(CodeGen, ComparisonOperations) {
    auto block = std::make_shared<BlockStmt>();
    block->statements.push_back(std::make_shared<VarDeclStmt>(T("Int64"), "a", std::make_shared<IntLiteral>(5)));
    block->statements.push_back(std::make_shared<VarDeclStmt>(T("Int64"), "b", std::make_shared<IntLiteral>(10)));

    auto expr = std::make_shared<BinaryOp>(std::make_shared<VarRef>("a"), "<", std::make_shared<VarRef>("b"));
    block->statements.push_back(std::make_shared<VarDeclStmt>(T("Bool"), "result", expr));
    block->statements.push_back(std::make_shared<ReturnStmt>(std::make_shared<IntLiteral>(0)));

    auto prog = std::make_shared<Program>();
    prog->functions.push_back(makeFunc(T("Int64"), "testCompare", {}, block));

    CodeGenerator codegen;
    auto irResult = codegen.generateIR(prog);
    ASSERT_TRUE(irResult);

    const std::string &ir = *irResult;
    EXPECT_NE(ir.find("icmp slt"), std::string::npos);
}

TEST(CodeGen, UnaryOperations) {
    auto block = std::make_shared<BlockStmt>();
    block->statements.push_back(std::make_shared<VarDeclStmt>(T("Int64"), "x", std::make_shared<IntLiteral>(42)));
    block->statements.push_back(std::make_shared<ReturnStmt>(
        std::make_shared<UnaryOp>("-", std::make_shared<VarRef>("x"))));

    auto prog = std::make_shared<Program>();
    prog->functions.push_back(makeFunc(T("Int64"), "testNeg", {}, block));

    CodeGenerator codegen;
    auto irResult = codegen.generateIR(prog);
    ASSERT_TRUE(irResult);

    const std::string &ir = *irResult;
    const bool hasNegViaSub =
        (ir.find("sub i64 0") != std::string::npos) ||
        (ir.find("sub nsw i64 0") != std::string::npos);
    EXPECT_TRUE(hasNegViaSub);
}

TEST(CodeGen, FunctionCalls) {
    auto prog = std::make_shared<Program>();

    auto helperBlock = std::make_shared<BlockStmt>();
    helperBlock->statements.push_back(std::make_shared<ReturnStmt>(
        std::make_shared<BinaryOp>(std::make_shared<VarRef>("x"), "*", std::make_shared<IntLiteral>(2))));
    prog->functions.push_back(makeFunc(T("Int64"), "helper", {{T("Int64"), "x"}}, helperBlock));

    auto mainBlock = std::make_shared<BlockStmt>();
    mainBlock->statements.push_back(std::make_shared<ReturnStmt>(
        std::make_shared<FuncCall>("helper", std::vector<std::shared_ptr<Expression>>{std::make_shared<IntLiteral>(21)})));
    prog->functions.push_back(makeFunc(T("Int64"), "main", {}, mainBlock));

    CodeGenerator codegen;
    auto irResult = codegen.generateIR(prog);
    ASSERT_TRUE(irResult);

    const std::string &ir = *irResult;
    EXPECT_NE(ir.find("define i64 @helper"), std::string::npos);
    EXPECT_NE(ir.find("call i64 @helper("), std::string::npos);
}

TEST(CodeGen, VoidFunction) {
    auto block = std::make_shared<BlockStmt>();
    block->statements.push_back(std::make_shared<ReturnStmt>(nullptr));

    auto prog = std::make_shared<Program>();
    prog->functions.push_back(makeFunc(T("Void"), "doNothing", {}, block));

    CodeGenerator codegen;
    auto irResult = codegen.generateIR(prog);
    ASSERT_TRUE(irResult);

    const std::string &ir = *irResult;
    EXPECT_NE(ir.find("define void @doNothing"), std::string::npos);
    EXPECT_NE(ir.find("ret void"), std::string::npos);
}

TEST(CodeGen, NewExpressionUsesMalloc) {
    auto prog = std::make_shared<Program>();
    prog->classes.push_back(std::make_shared<ClassDecl>(
        "Node",
        std::vector<std::pair<Type, std::string>>{{T("Int64"), "value"}}));

    auto block = std::make_shared<BlockStmt>();
    block->statements.push_back(std::make_shared<VarDeclStmt>(S("Node"), "n", std::make_shared<NewExpr>("Node")));
    block->statements.push_back(std::make_shared<ReturnStmt>(std::make_shared<IntLiteral>(0)));
    prog->functions.push_back(makeFunc(T("Int64"), "createNode", {}, block));

    CodeGenerator codegen;
    auto irResult = codegen.generateIR(prog);
    ASSERT_TRUE(irResult);

    const std::string &ir = *irResult;
    EXPECT_NE(ir.find("@malloc"), std::string::npos);
}

TEST(CodeGen, StringLiteralEmitsConstant) {
    auto block = std::make_shared<BlockStmt>();
    block->statements.push_back(
        std::make_shared<VarDeclStmt>(T("Str"), "msg", std::make_shared<StringLiteral>("Hello, World!")));
    block->statements.push_back(std::make_shared<ReturnStmt>(std::make_shared<IntLiteral>(0)));

    auto prog = std::make_shared<Program>();
    prog->functions.push_back(makeFunc(T("Int64"), "main", {}, block));

    CodeGenerator codegen;
    auto irResult = codegen.generateIR(prog);
    ASSERT_TRUE(irResult);

    const std::string &ir = *irResult;
    EXPECT_NE(ir.find("Hello, World!"), std::string::npos);
}

TEST(CodeGen, BoolLiterals) {
    auto block = std::make_shared<BlockStmt>();
    block->statements.push_back(std::make_shared<VarDeclStmt>(T("Bool"), "yes", std::make_shared<BoolLiteral>(true)));
    block->statements.push_back(std::make_shared<VarDeclStmt>(T("Bool"), "no", std::make_shared<BoolLiteral>(false)));
    block->statements.push_back(std::make_shared<ReturnStmt>(std::make_shared<IntLiteral>(0)));

    auto prog = std::make_shared<Program>();
    prog->functions.push_back(makeFunc(T("Int64"), "testBool", {}, block));

    CodeGenerator codegen;
    auto irResult = codegen.generateIR(prog);
    ASSERT_TRUE(irResult);

    const std::string &ir = *irResult;
    EXPECT_NE(ir.find("store i1 true"), std::string::npos);
    EXPECT_NE(ir.find("store i1 false"), std::string::npos);
}

TEST(CodeGen, IROutputNonEmpty) {
    auto block = std::make_shared<BlockStmt>();
    block->statements.push_back(std::make_shared<ReturnStmt>(std::make_shared<IntLiteral>(0)));

    auto prog = std::make_shared<Program>();
    prog->functions.push_back(makeFunc(T("Int64"), "main", {}, block));

    CodeGenerator codegen;
    auto irResult = codegen.generateIR(prog);
    ASSERT_TRUE(irResult);
    EXPECT_FALSE(irResult->empty());

    EXPECT_NE(irResult->find("sisyl_module"), std::string::npos);
}
