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
 * ast_builder.h
 *
 * Defines the AstBuilder class, an ANTLR4 visitor that traverses the parse
 * tree generated from SiSyL source code and constructs the corresponding AST.
 * The visitor pattern maps each grammar rule to a visit method that creates
 * AST nodes using the classes defined in ast.h.
 */

#ifndef SISYL_AST_BUILDER_H
#define SISYL_AST_BUILDER_H

#include "ast.h"
#include "SiSyLBaseVisitor.h"

#include <memory>
#include <string>
#include <vector>

namespace sisyl {

/**
 * AstBuilder visits the ANTLR4 parse tree and produces an AST.
 *
 * Each visit method returns an antlrcpp::Any containing the constructed
 * AST node (typically a shared_ptr).  Top level visitProgram returns
 * a shared_ptr<Program>.
 */
class AstBuilder : public SiSyLBaseVisitor {
public:
    AstBuilder() = default;

    // Program: (classDecl | funcDecl)* EOF
    antlrcpp::Any visitProgram(SiSyLParser::ProgramContext *ctx) override;

    // Class declaration
    antlrcpp::Any visitClassDecl(SiSyLParser::ClassDeclContext *ctx) override;
    antlrcpp::Any visitClassFields(SiSyLParser::ClassFieldsContext *ctx) override;

    // Function declaration
    antlrcpp::Any visitFuncDecl(SiSyLParser::FuncDeclContext *ctx) override;
    antlrcpp::Any visitParamList(SiSyLParser::ParamListContext *ctx) override;
    antlrcpp::Any visitParam(SiSyLParser::ParamContext *ctx) override;

    // Block and statements
    antlrcpp::Any visitBlock(SiSyLParser::BlockContext *ctx) override;
    antlrcpp::Any visitStatement(SiSyLParser::StatementContext *ctx) override;
    antlrcpp::Any visitBlockStmt(SiSyLParser::BlockStmtContext *ctx) override;
    antlrcpp::Any visitVarDecl(SiSyLParser::VarDeclContext *ctx) override;
    antlrcpp::Any visitAssignStmt(SiSyLParser::AssignStmtContext *ctx) override;
    antlrcpp::Any visitIfStmt(SiSyLParser::IfStmtContext *ctx) override;
    antlrcpp::Any visitWhileStmt(SiSyLParser::WhileStmtContext *ctx) override;
    antlrcpp::Any visitReturnStmt(SiSyLParser::ReturnStmtContext *ctx) override;
    antlrcpp::Any visitExprStmt(SiSyLParser::ExprStmtContext *ctx) override;

    // Expressions
    antlrcpp::Any visitExpression(SiSyLParser::ExpressionContext *ctx) override;
    antlrcpp::Any visitLogicalOrExpr(SiSyLParser::LogicalOrExprContext *ctx) override;
    antlrcpp::Any visitLogicalAndExpr(SiSyLParser::LogicalAndExprContext *ctx) override;
    antlrcpp::Any visitEqualityExpr(SiSyLParser::EqualityExprContext *ctx) override;
    antlrcpp::Any visitRelationalExpr(SiSyLParser::RelationalExprContext *ctx) override;
    antlrcpp::Any visitAddExpr(SiSyLParser::AddExprContext *ctx) override;
    antlrcpp::Any visitMulExpr(SiSyLParser::MulExprContext *ctx) override;
    antlrcpp::Any visitUnaryExpr(SiSyLParser::UnaryExprContext *ctx) override;
    antlrcpp::Any visitPrimary(SiSyLParser::PrimaryContext *ctx) override;

    // Location and type
    antlrcpp::Any visitLocation(SiSyLParser::LocationContext *ctx) override;
    antlrcpp::Any visitType(SiSyLParser::TypeContext *ctx) override;
    antlrcpp::Any visitArgList(SiSyLParser::ArgListContext *ctx) override;

private:
    // Helper to extract text and strip quotes from string literals
    std::string unquoteString(const std::string &quoted);
};

} // namespace sisyl

#endif // SISYL_AST_BUILDER_H
