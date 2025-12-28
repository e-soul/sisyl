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
 * ast.h
 *
 * This header defines the abstract syntax tree for the SiSyL language.  Each
 * node class represents a syntactic construct in the language.  The AST is
 * built by visiting the ANTLR parse tree and constructing appropriate C++
 * objects.  Semantic analysis and code generation operate on this AST.
 *
 * The design loosely follows the visitor pattern: each node inherits from
 * AstNode and implements an accept() function that dispatches to a visitor.
 * For the purposes of this MVP we keep the visitor interface simple and
 * concentrated in SemanticAnalyzer and CodeGenerator.  Additional visitors
 * could be added later (e.g. pretty printing).
 */

#ifndef SISYL_AST_H
#define SISYL_AST_H

#include "type.h"

#include <cstdint>
#include <string>
#include <vector>
#include <memory>
#include <optional>

namespace sisyl {

// Forward declarations of visitors
class SemanticAnalyzer;
class CodeGenerator;

/* Base class for all AST nodes.  Every node has a source location (optional)
 * and implements accept methods for visitors.  The destructor is virtual to
 * allow deletion via base pointer.  We use std::shared_ptr to manage
 * ownership of nodes within the AST. */
class AstNode {
public:
    virtual ~AstNode() = default;
    // Accept methods to be overridden in derived classes
    virtual void accept(SemanticAnalyzer &visitor) = 0;
    virtual void accept(CodeGenerator &visitor) = 0;
};

// Base class for expressions
class Expression : public AstNode {
public:
    // Expressions may have a deduced or declared type attached after
    // semantic analysis.
    std::optional<Type> type;
    virtual ~Expression() = default;
};

// Base class for statements
class Statement : public AstNode {
public:
    virtual ~Statement() = default;
};

// Literal expressions
class IntLiteral : public Expression {
public:
    std::int64_t value;
    explicit IntLiteral(std::int64_t v) : value(v) {}
    void accept(SemanticAnalyzer &visitor) override;
    void accept(CodeGenerator &visitor) override;
};

class BoolLiteral : public Expression {
public:
    bool value;
    explicit BoolLiteral(bool v) : value(v) {}
    void accept(SemanticAnalyzer &visitor) override;
    void accept(CodeGenerator &visitor) override;
};

class StringLiteral : public Expression {
public:
    std::string value;
    explicit StringLiteral(std::string v) : value(std::move(v)) {}
    void accept(SemanticAnalyzer &visitor) override;
    void accept(CodeGenerator &visitor) override;
};

// Variable reference.  May refer to a local variable, parameter or global.
class VarRef : public Expression {
public:
    std::string name;
    explicit VarRef(std::string n) : name(std::move(n)) {}
    void accept(SemanticAnalyzer &visitor) override;
    void accept(CodeGenerator &visitor) override;
};

// Field access (e.g. a.b.c).  Represented as a sequence of identifiers.
class FieldAccess : public Expression {
public:
    std::vector<std::string> path;
    explicit FieldAccess(std::vector<std::string> p) : path(std::move(p)) {}
    void accept(SemanticAnalyzer &visitor) override;
    void accept(CodeGenerator &visitor) override;
};

// Unary operator expression
class UnaryOp : public Expression {
public:
    std::string op;
    std::shared_ptr<Expression> operand;
    UnaryOp(std::string op, std::shared_ptr<Expression> operand)
        : op(std::move(op)), operand(std::move(operand)) {}
    void accept(SemanticAnalyzer &visitor) override;
    void accept(CodeGenerator &visitor) override;
};

// Binary operator expression
class BinaryOp : public Expression {
public:
    std::string op;
    std::shared_ptr<Expression> left;
    std::shared_ptr<Expression> right;
    BinaryOp(std::shared_ptr<Expression> l, std::string op, std::shared_ptr<Expression> r)
        : op(std::move(op)), left(std::move(l)), right(std::move(r)) {}
    void accept(SemanticAnalyzer &visitor) override;
    void accept(CodeGenerator &visitor) override;
};

// Function call expression
class FuncCall : public Expression {
public:
    std::string callee;
    std::vector<std::shared_ptr<Expression>> args;
    FuncCall(std::string c, std::vector<std::shared_ptr<Expression>> a)
        : callee(std::move(c)), args(std::move(a)) {}
    void accept(SemanticAnalyzer &visitor) override;
    void accept(CodeGenerator &visitor) override;
};

// Object allocation expression (new Type())
class NewExpr : public Expression {
public:
    std::string typeName;
    explicit NewExpr(std::string t) : typeName(std::move(t)) {}
    void accept(SemanticAnalyzer &visitor) override;
    void accept(CodeGenerator &visitor) override;
};

// Statement classes

// A block of statements introduces a new scope.  It owns a list of
// statements executed sequentially.
class BlockStmt : public Statement {
public:
    std::vector<std::shared_ptr<Statement>> statements;
    explicit BlockStmt(std::vector<std::shared_ptr<Statement>> stmts = {})
        : statements(std::move(stmts)) {}
    void accept(SemanticAnalyzer &visitor) override;
    void accept(CodeGenerator &visitor) override;
};

// Declaration of a local variable.  If initExpr is non‑null the variable
// is initialised with that expression.  For non‑primitive types ownership
// is transferred from the expression to the variable.
class VarDeclStmt : public Statement {
public:
    Type typeName;
    std::string name;
    std::shared_ptr<Expression> initExpr;
    VarDeclStmt(Type t, std::string n,
                std::shared_ptr<Expression> init = nullptr)
        : typeName(std::move(t)), name(std::move(n)), initExpr(std::move(init)) {}
    void accept(SemanticAnalyzer &visitor) override;
    void accept(CodeGenerator &visitor) override;
};

// Assignment statement.  Assigns the result of expr to location.
class AssignStmt : public Statement {
public:
    std::shared_ptr<Expression> location;
    std::shared_ptr<Expression> expr;
    AssignStmt(std::shared_ptr<Expression> loc, std::shared_ptr<Expression> expr)
        : location(std::move(loc)), expr(std::move(expr)) {}
    void accept(SemanticAnalyzer &visitor) override;
    void accept(CodeGenerator &visitor) override;
};

// If/else statement.  The else branch may be null.
class IfStmt : public Statement {
public:
    std::shared_ptr<Expression> condition;
    std::shared_ptr<BlockStmt> thenBranch;
    std::shared_ptr<BlockStmt> elseBranch;
    IfStmt(std::shared_ptr<Expression> cond,
           std::shared_ptr<BlockStmt> thenBlk,
           std::shared_ptr<BlockStmt> elseBlk = nullptr)
        : condition(std::move(cond)), thenBranch(std::move(thenBlk)), elseBranch(std::move(elseBlk)) {}
    void accept(SemanticAnalyzer &visitor) override;
    void accept(CodeGenerator &visitor) override;
};

// While statement
class WhileStmt : public Statement {
public:
    std::shared_ptr<Expression> condition;
    std::shared_ptr<BlockStmt> body;
    WhileStmt(std::shared_ptr<Expression> cond, std::shared_ptr<BlockStmt> body)
        : condition(std::move(cond)), body(std::move(body)) {}
    void accept(SemanticAnalyzer &visitor) override;
    void accept(CodeGenerator &visitor) override;
};

// Return statement.  Returns an optional expression.
class ReturnStmt : public Statement {
public:
    std::shared_ptr<Expression> expr;
    explicit ReturnStmt(std::shared_ptr<Expression> e = nullptr)
        : expr(std::move(e)) {}
    void accept(SemanticAnalyzer &visitor) override;
    void accept(CodeGenerator &visitor) override;
};

// Expression used as a statement.  Allows function calls or other
// expressions whose value is ignored.
class ExprStmt : public Statement {
public:
    std::shared_ptr<Expression> expr;
    explicit ExprStmt(std::shared_ptr<Expression> e) : expr(std::move(e)) {}
    void accept(SemanticAnalyzer &visitor) override;
    void accept(CodeGenerator &visitor) override;
};

// Top level class declaration
class ClassDecl : public AstNode {
public:
    std::string name;
    // Each field is a pair (type, name)
    std::vector<std::pair<Type, std::string>> fields;
    ClassDecl(std::string n, std::vector<std::pair<Type, std::string>> f)
        : name(std::move(n)), fields(std::move(f)) {}
    void accept(SemanticAnalyzer &visitor) override;
    void accept(CodeGenerator &visitor) override;
};

// Function parameter
struct Parameter {
    Type type;
    std::string name;
};

// Top level function declaration.  Holds the return type, name,
// parameters and the body block.
class FuncDecl : public AstNode {
public:
    Type returnType;
    std::string name;
    std::vector<Parameter> params;
    std::shared_ptr<BlockStmt> body;
    FuncDecl(Type retType, std::string name,
             std::vector<Parameter> params,
             std::shared_ptr<BlockStmt> body)
        : returnType(std::move(retType)), name(std::move(name)),
          params(std::move(params)), body(std::move(body)) {}
    void accept(SemanticAnalyzer &visitor) override;
    void accept(CodeGenerator &visitor) override;
};

// The top level AST represents a program: a list of class and function
// declarations.
class Program : public AstNode {
public:
    std::vector<std::shared_ptr<ClassDecl>> classes;
    std::vector<std::shared_ptr<FuncDecl>> functions;
    void accept(SemanticAnalyzer &visitor) override;
    void accept(CodeGenerator &visitor) override;
};

} // namespace sisyl

#endif // SISYL_AST_H