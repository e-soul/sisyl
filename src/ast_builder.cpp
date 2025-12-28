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
 * ast_builder.cpp
 *
 * Implementation of the AstBuilder visitor.  Each visit method traverses
 * ANTLR4 parse tree nodes and constructs the corresponding AST nodes.
 */

#include "ast_builder.h"

#include <any>
#include <stdexcept>

using namespace sisyl;

//-----------------------------------------------------------------------------
// Helper functions
//-----------------------------------------------------------------------------

std::string AstBuilder::unquoteString(const std::string &quoted) {
    // Remove surrounding quotes from a string literal
    if (quoted.size() >= 2 && quoted.front() == '"' && quoted.back() == '"') {
        return quoted.substr(1, quoted.size() - 2);
    }
    return quoted;
}

//-----------------------------------------------------------------------------
// Program level
//-----------------------------------------------------------------------------

antlrcpp::Any AstBuilder::visitProgram(SiSyLParser::ProgramContext *ctx) {
    auto program = std::make_shared<Program>();

    for (auto *classCtx : ctx->classDecl()) {
        auto classDecl = std::any_cast<std::shared_ptr<ClassDecl>>(visit(classCtx));
        program->classes.push_back(classDecl);
    }

    for (auto *funcCtx : ctx->funcDecl()) {
        auto funcDecl = std::any_cast<std::shared_ptr<FuncDecl>>(visit(funcCtx));
        program->functions.push_back(funcDecl);
    }

    return program;
}

//-----------------------------------------------------------------------------
// Class declaration
//-----------------------------------------------------------------------------

antlrcpp::Any AstBuilder::visitClassDecl(SiSyLParser::ClassDeclContext *ctx) {
    std::string name = ctx->IDENT()->getText();
    auto fields = std::any_cast<std::vector<std::pair<Type, std::string>>>(
        visit(ctx->classFields()));
    return std::make_shared<ClassDecl>(name, fields);
}

antlrcpp::Any AstBuilder::visitClassFields(SiSyLParser::ClassFieldsContext *ctx) {
    std::vector<std::pair<Type, std::string>> fields;
    
    // Grammar: (type IDENT ';')*
    // The children alternate: type, IDENT, ';', type, IDENT, ';', ...
    auto types = ctx->type();
    auto idents = ctx->IDENT();
    
    for (size_t i = 0; i < types.size(); ++i) {
        Type typeName = std::any_cast<Type>(visit(types[i]));
        std::string fieldName = idents[i]->getText();
        fields.emplace_back(std::move(typeName), std::move(fieldName));
    }
    
    return fields;
}

//-----------------------------------------------------------------------------
// Function declaration
//-----------------------------------------------------------------------------

antlrcpp::Any AstBuilder::visitFuncDecl(SiSyLParser::FuncDeclContext *ctx) {
    Type returnType = std::any_cast<Type>(visit(ctx->type()));
    std::string name = ctx->IDENT()->getText();
    
    std::vector<Parameter> params;
    if (ctx->paramList()) {
        params = std::any_cast<std::vector<Parameter>>(visit(ctx->paramList()));
    }
    
    auto body = std::any_cast<std::shared_ptr<BlockStmt>>(visit(ctx->block()));
    
    return std::make_shared<FuncDecl>(std::move(returnType), name, params, body);
}

antlrcpp::Any AstBuilder::visitParamList(SiSyLParser::ParamListContext *ctx) {
    std::vector<Parameter> params;
    for (auto *paramCtx : ctx->param()) {
        params.push_back(std::any_cast<Parameter>(visit(paramCtx)));
    }
    return params;
}

antlrcpp::Any AstBuilder::visitParam(SiSyLParser::ParamContext *ctx) {
    Parameter p;
    p.type = std::any_cast<Type>(visit(ctx->type()));
    p.name = ctx->IDENT()->getText();
    return p;
}

//-----------------------------------------------------------------------------
// Block and statements
//-----------------------------------------------------------------------------

antlrcpp::Any AstBuilder::visitBlock(SiSyLParser::BlockContext *ctx) {
    auto block = std::make_shared<BlockStmt>();
    for (auto *stmtCtx : ctx->statement()) {
        auto stmt = std::any_cast<std::shared_ptr<Statement>>(visit(stmtCtx));
        block->statements.push_back(stmt);
    }
    return block;
}

antlrcpp::Any AstBuilder::visitStatement(SiSyLParser::StatementContext *ctx) {
    // Dispatch to the appropriate statement type
    if (ctx->varDecl()) {
        return visit(ctx->varDecl());
    } else if (ctx->assignStmt()) {
        return visit(ctx->assignStmt());
    } else if (ctx->ifStmt()) {
        return visit(ctx->ifStmt());
    } else if (ctx->whileStmt()) {
        return visit(ctx->whileStmt());
    } else if (ctx->returnStmt()) {
        return visit(ctx->returnStmt());
    } else if (ctx->exprStmt()) {
        return visit(ctx->exprStmt());
    } else if (ctx->blockStmt()) {
        return visit(ctx->blockStmt());
    }
    throw std::runtime_error("Unknown statement type");
}

antlrcpp::Any AstBuilder::visitBlockStmt(SiSyLParser::BlockStmtContext *ctx) {
    // A block statement wraps a block as a statement
    auto block = std::any_cast<std::shared_ptr<BlockStmt>>(visit(ctx->block()));
    return std::static_pointer_cast<Statement>(block);
}

antlrcpp::Any AstBuilder::visitVarDecl(SiSyLParser::VarDeclContext *ctx) {
    Type typeName = std::any_cast<Type>(visit(ctx->type()));
    std::string name = ctx->IDENT()->getText();
    
    std::shared_ptr<Expression> initExpr;
    if (ctx->expression()) {
        initExpr = std::any_cast<std::shared_ptr<Expression>>(visit(ctx->expression()));
    }
    
    return std::static_pointer_cast<Statement>(
        std::make_shared<VarDeclStmt>(std::move(typeName), name, initExpr));
}

antlrcpp::Any AstBuilder::visitAssignStmt(SiSyLParser::AssignStmtContext *ctx) {
    auto location = std::any_cast<std::shared_ptr<Expression>>(visit(ctx->location()));
    auto expr = std::any_cast<std::shared_ptr<Expression>>(visit(ctx->expression()));
    
    return std::static_pointer_cast<Statement>(
        std::make_shared<AssignStmt>(location, expr));
}

antlrcpp::Any AstBuilder::visitIfStmt(SiSyLParser::IfStmtContext *ctx) {
    auto condition = std::any_cast<std::shared_ptr<Expression>>(visit(ctx->expression()));
    
    auto blocks = ctx->block();
    auto thenBranch = std::any_cast<std::shared_ptr<BlockStmt>>(visit(blocks[0]));
    
    std::shared_ptr<BlockStmt> elseBranch;
    if (blocks.size() > 1) {
        elseBranch = std::any_cast<std::shared_ptr<BlockStmt>>(visit(blocks[1]));
    }
    
    return std::static_pointer_cast<Statement>(
        std::make_shared<IfStmt>(condition, thenBranch, elseBranch));
}

antlrcpp::Any AstBuilder::visitWhileStmt(SiSyLParser::WhileStmtContext *ctx) {
    auto condition = std::any_cast<std::shared_ptr<Expression>>(visit(ctx->expression()));
    auto body = std::any_cast<std::shared_ptr<BlockStmt>>(visit(ctx->block()));
    
    return std::static_pointer_cast<Statement>(
        std::make_shared<WhileStmt>(condition, body));
}

antlrcpp::Any AstBuilder::visitReturnStmt(SiSyLParser::ReturnStmtContext *ctx) {
    std::shared_ptr<Expression> expr;
    if (ctx->expression()) {
        expr = std::any_cast<std::shared_ptr<Expression>>(visit(ctx->expression()));
    }
    
    return std::static_pointer_cast<Statement>(
        std::make_shared<ReturnStmt>(expr));
}

antlrcpp::Any AstBuilder::visitExprStmt(SiSyLParser::ExprStmtContext *ctx) {
    auto expr = std::any_cast<std::shared_ptr<Expression>>(visit(ctx->expression()));
    
    return std::static_pointer_cast<Statement>(
        std::make_shared<ExprStmt>(expr));
}

//-----------------------------------------------------------------------------
// Expressions
//-----------------------------------------------------------------------------

antlrcpp::Any AstBuilder::visitExpression(SiSyLParser::ExpressionContext *ctx) {
    return visit(ctx->logicalOrExpr());
}

antlrcpp::Any AstBuilder::visitLogicalOrExpr(SiSyLParser::LogicalOrExprContext *ctx) {
    auto operands = ctx->logicalAndExpr();
    auto result = std::any_cast<std::shared_ptr<Expression>>(visit(operands[0]));
    
    for (size_t i = 1; i < operands.size(); ++i) {
        auto right = std::any_cast<std::shared_ptr<Expression>>(visit(operands[i]));
        result = std::make_shared<BinaryOp>(result, "||", right);
    }
    
    return result;
}

antlrcpp::Any AstBuilder::visitLogicalAndExpr(SiSyLParser::LogicalAndExprContext *ctx) {
    auto operands = ctx->equalityExpr();
    auto result = std::any_cast<std::shared_ptr<Expression>>(visit(operands[0]));
    
    for (size_t i = 1; i < operands.size(); ++i) {
        auto right = std::any_cast<std::shared_ptr<Expression>>(visit(operands[i]));
        result = std::make_shared<BinaryOp>(result, "&&", right);
    }
    
    return result;
}

antlrcpp::Any AstBuilder::visitEqualityExpr(SiSyLParser::EqualityExprContext *ctx) {
    auto operands = ctx->relationalExpr();
    auto result = std::any_cast<std::shared_ptr<Expression>>(visit(operands[0]));

    // Grammar shape: relationalExpr (('==' | '!=') relationalExpr)*
    // Children layout: operand0, op0, operand1, op1, operand2, ...
    for (size_t i = 1; i < operands.size(); ++i) {
        const size_t opIndex = 2 * i - 1;
        auto *opNode = (opIndex < ctx->children.size()) ? ctx->children[opIndex] : nullptr;
        const std::string op = opNode ? opNode->getText() : std::string{};
        auto right = std::any_cast<std::shared_ptr<Expression>>(visit(operands[i]));
        result = std::make_shared<BinaryOp>(result, op, right);
    }
    
    return result;
}

antlrcpp::Any AstBuilder::visitRelationalExpr(SiSyLParser::RelationalExprContext *ctx) {
    auto operands = ctx->addExpr();
    auto result = std::any_cast<std::shared_ptr<Expression>>(visit(operands[0]));

    // Grammar shape: addExpr (('<' | '>' | '<=' | '>=') addExpr)*
    for (size_t i = 1; i < operands.size(); ++i) {
        const size_t opIndex = 2 * i - 1;
        auto *opNode = (opIndex < ctx->children.size()) ? ctx->children[opIndex] : nullptr;
        const std::string op = opNode ? opNode->getText() : std::string{};
        auto right = std::any_cast<std::shared_ptr<Expression>>(visit(operands[i]));
        result = std::make_shared<BinaryOp>(result, op, right);
    }
    
    return result;
}

antlrcpp::Any AstBuilder::visitAddExpr(SiSyLParser::AddExprContext *ctx) {
    auto operands = ctx->mulExpr();
    auto result = std::any_cast<std::shared_ptr<Expression>>(visit(operands[0]));

    // Grammar shape: mulExpr (('+' | '-') mulExpr)*
    for (size_t i = 1; i < operands.size(); ++i) {
        const size_t opIndex = 2 * i - 1;
        auto *opNode = (opIndex < ctx->children.size()) ? ctx->children[opIndex] : nullptr;
        const std::string op = opNode ? opNode->getText() : std::string{};
        auto right = std::any_cast<std::shared_ptr<Expression>>(visit(operands[i]));
        result = std::make_shared<BinaryOp>(result, op, right);
    }
    
    return result;
}

antlrcpp::Any AstBuilder::visitMulExpr(SiSyLParser::MulExprContext *ctx) {
    auto operands = ctx->unaryExpr();
    auto result = std::any_cast<std::shared_ptr<Expression>>(visit(operands[0]));

    // Grammar shape: unaryExpr (('*' | '/') unaryExpr)*
    for (size_t i = 1; i < operands.size(); ++i) {
        const size_t opIndex = 2 * i - 1;
        auto *opNode = (opIndex < ctx->children.size()) ? ctx->children[opIndex] : nullptr;
        const std::string op = opNode ? opNode->getText() : std::string{};
        auto right = std::any_cast<std::shared_ptr<Expression>>(visit(operands[i]));
        result = std::make_shared<BinaryOp>(result, op, right);
    }
    
    return result;
}

antlrcpp::Any AstBuilder::visitUnaryExpr(SiSyLParser::UnaryExprContext *ctx) {
    if (ctx->primary()) {
        return visit(ctx->primary());
    }
    
    // Unary operator: ('!' | '-') unaryExpr
    std::string op;
    if (ctx->children[0]->getText() == "!") {
        op = "!";
    } else {
        op = "-";
    }
    
    auto operand = std::any_cast<std::shared_ptr<Expression>>(visit(ctx->unaryExpr()));
    return std::static_pointer_cast<Expression>(
        std::make_shared<UnaryOp>(op, operand));
}

antlrcpp::Any AstBuilder::visitPrimary(SiSyLParser::PrimaryContext *ctx) {
    // INT_LIT
    if (ctx->INT_LIT()) {
        std::int64_t value = std::stoll(ctx->INT_LIT()->getText());
        return std::static_pointer_cast<Expression>(
            std::make_shared<IntLiteral>(value));
    }
    
    // BOOL_LIT
    if (ctx->BOOL_LIT()) {
        bool value = (ctx->BOOL_LIT()->getText() == "true");
        return std::static_pointer_cast<Expression>(
            std::make_shared<BoolLiteral>(value));
    }
    
    // STRING_LIT
    if (ctx->STRING_LIT()) {
        std::string value = unquoteString(ctx->STRING_LIT()->getText());
        return std::static_pointer_cast<Expression>(
            std::make_shared<StringLiteral>(value));
    }
    
    // 'new' IDENT '(' ')'
    if (ctx->NEW()) {
        std::string typeName = ctx->IDENT()->getText();
        return std::static_pointer_cast<Expression>(
            std::make_shared<NewExpr>(typeName));
    }
    
    // Parenthesized expression
    if (ctx->expression()) {
        return visit(ctx->expression());
    }

    // Location (variable or field access)
    if (ctx->location()) {
        return visit(ctx->location());
    }
    
    // Function call: IDENT '(' argList? ')'
    if (ctx->argList() || 
        (ctx->IDENT() && ctx->children.size() > 1 && 
         ctx->children[1]->getText() == "(")) {
        std::string callee = ctx->IDENT()->getText();
        std::vector<std::shared_ptr<Expression>> args;
        
        if (ctx->argList()) {
            args = std::any_cast<std::vector<std::shared_ptr<Expression>>>(
                visit(ctx->argList()));
        }
        
        return std::static_pointer_cast<Expression>(
            std::make_shared<FuncCall>(callee, args));
    }
    
    throw std::runtime_error("Unknown primary expression type");
}

antlrcpp::Any AstBuilder::visitArgList(SiSyLParser::ArgListContext *ctx) {
    std::vector<std::shared_ptr<Expression>> args;
    for (auto *exprCtx : ctx->expression()) {
        args.push_back(std::any_cast<std::shared_ptr<Expression>>(visit(exprCtx)));
    }
    return args;
}

antlrcpp::Any AstBuilder::visitLocation(SiSyLParser::LocationContext *ctx) {
    auto idents = ctx->IDENT();
    
    if (idents.size() == 1) {
        // Simple variable reference
        return std::static_pointer_cast<Expression>(
            std::make_shared<VarRef>(idents[0]->getText()));
    }
    
    // Field access path (a.b.c)
    std::vector<std::string> path;
    for (auto *ident : idents) {
        path.push_back(ident->getText());
    }
    return std::static_pointer_cast<Expression>(
        std::make_shared<FieldAccess>(path));
}

antlrcpp::Any AstBuilder::visitType(SiSyLParser::TypeContext *ctx) {
    return parseType(ctx->getText());
}
