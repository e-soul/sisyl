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
#pragma once

#include "SiSyLBaseVisitor.h"
#include "ast.h"

namespace sisyl {

/**
 * Visits the ANTLR4 parse tree to construct an AST.
 * Each visit method returns an std::any holding a shared_ptr to an AST node.
 */
class AstBuilder : public SiSyLBaseVisitor {
  public:
    AstBuilder() = default;

    std::any visitProgram(SiSyLParser::ProgramContext *ctx) override;

    // Class and functioun
    std::any visitClassDecl(SiSyLParser::ClassDeclContext *ctx) override;
    std::any visitClassFields(SiSyLParser::ClassFieldsContext *ctx) override;
    std::any visitFuncDecl(SiSyLParser::FuncDeclContext *ctx) override;
    std::any visitParamList(SiSyLParser::ParamListContext *ctx) override;
    std::any visitParam(SiSyLParser::ParamContext *ctx) override;

    // Block and statements
    std::any visitBlock(SiSyLParser::BlockContext *ctx) override;
    std::any visitStatement(SiSyLParser::StatementContext *ctx) override;
    std::any visitBlockStmt(SiSyLParser::BlockStmtContext *ctx) override;
    std::any visitVarDecl(SiSyLParser::VarDeclContext *ctx) override;
    std::any visitAssignStmt(SiSyLParser::AssignStmtContext *ctx) override;
    std::any visitIfStmt(SiSyLParser::IfStmtContext *ctx) override;
    std::any visitWhileStmt(SiSyLParser::WhileStmtContext *ctx) override;
    std::any visitReturnStmt(SiSyLParser::ReturnStmtContext *ctx) override;
    std::any visitExprStmt(SiSyLParser::ExprStmtContext *ctx) override;

    // Expressions
    std::any visitExpression(SiSyLParser::ExpressionContext *ctx) override;
    std::any visitLogicalOrExpr(SiSyLParser::LogicalOrExprContext *ctx) override;
    std::any visitLogicalAndExpr(SiSyLParser::LogicalAndExprContext *ctx) override;
    std::any visitEqualityExpr(SiSyLParser::EqualityExprContext *ctx) override;
    std::any visitRelationalExpr(SiSyLParser::RelationalExprContext *ctx) override;
    std::any visitAddExpr(SiSyLParser::AddExprContext *ctx) override;
    std::any visitMulExpr(SiSyLParser::MulExprContext *ctx) override;
    std::any visitUnaryExpr(SiSyLParser::UnaryExprContext *ctx) override;
    std::any visitPrimary(SiSyLParser::PrimaryContext *ctx) override;

    // Location and type
    std::any visitLocation(SiSyLParser::LocationContext *ctx) override;
    std::any visitType(SiSyLParser::TypeContext *ctx) override;
    std::any visitArgList(SiSyLParser::ArgListContext *ctx) override;
};

} // namespace sisyl
