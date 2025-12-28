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
 * semantic_analyzer.h
 *
 * Defines the semantic analysis pass for the SiSyL compiler.  The job of the
 * SemanticAnalyzer is to walk the AST, construct symbol tables, resolve
 * identifiers to declarations, enforce type rules and implement the single
 * ownership semantics.  This component detects type errors, undeclared
 * identifiers, ownership violations (e.g. double moves) and other
 * compile‑time errors.  On success it annotates AST nodes with type
 * information that will be used during code generation.
 *
 * For the MVP we provide a basic skeleton.  The analyzer maintains a stack
 * of scopes, each mapping names to symbol information.  Ownership of
 * non‑primitive values is tracked by recording whether a variable currently
 * holds the only reference to its resource.  When a resource is moved
 * into another variable or returned from a function, the original variable
 * becomes invalid and further uses are flagged as errors.  Primitive types
 * (Int64, Bool, Str) are copied rather than moved.
 */

#ifndef SISYL_SEMANTIC_ANALYZER_H
#define SISYL_SEMANTIC_ANALYZER_H

#include "ast.h"
#include "type.h"

#include <expected>
#include <unordered_map>
#include <string>
#include <vector>

namespace sisyl {

using Diagnostics = std::vector<std::string>;

// Represents a symbol (variable, function or class field) in the current
// scope.  For simplicity we handle variables only; functions and classes
// are stored separately in the Program structure.
struct Symbol {
    std::string name;
    Type type;
    bool isOwned;      // true if this symbol owns its resource
    bool isMoved;      // true if ownership has been transferred away
};

// A simple symbol table representing a lexical scope.  Scopes form a stack.
class SymbolTable {
public:
    // Add a symbol.  Returns false if the name already exists in the current
    // scope; true otherwise.  Ownership defaults to true for non‑primitive
    // types and false for primitives.
    bool insert(const std::string &name, const Type &type, bool owned);
    // Look up a symbol.  Searches from innermost to outer scopes.  Returns
    // a pointer to the symbol or nullptr if not found.
    Symbol *lookup(const std::string &name);
    // Mark a symbol as moved (ownership transferred)
    void markMoved(const std::string &name);
    // Check if a symbol has been moved
    bool isMoved(const std::string &name);
    // Push/pop scopes
    void pushScope();
    void popScope();
private:
    std::vector<std::unordered_map<std::string, Symbol>> scopes;
};

/*
 * SemanticAnalyzer
 *
 * Implements the visitor interface for AST nodes.  Each visit method
 * performs appropriate checks and annotations for a specific node type.  On
 * encountering an error the analyzer records a diagnostic message.  At the
 * end of analysis the diagnostics can be queried to determine if the
 * compilation should proceed.
 */
class SemanticAnalyzer {
public:
    SemanticAnalyzer();

    // Entry point: analyze the entire program.  Returns true if
    // successful, false if any semantic errors are found.
    [[nodiscard]] std::expected<void, Diagnostics> analyze(const std::shared_ptr<Program> &program);

    // Visitor methods for each AST node type
    void visit(Program &node);
    void visit(ClassDecl &node);
    void visit(FuncDecl &node);
    void visit(BlockStmt &node);
    void visit(VarDeclStmt &node);
    void visit(AssignStmt &node);
    void visit(IfStmt &node);
    void visit(WhileStmt &node);
    void visit(ReturnStmt &node);
    void visit(ExprStmt &node);
    void visit(IntLiteral &node);
    void visit(BoolLiteral &node);
    void visit(StringLiteral &node);
    void visit(VarRef &node);
    void visit(FieldAccess &node);
    void visit(UnaryOp &node);
    void visit(BinaryOp &node);
    void visit(FuncCall &node);
    void visit(NewExpr &node);

private:
    // Helper functions
    void error(const std::string &msg);
    // Check if an expression is a simple variable reference and return its name
    // Returns empty string if not a simple variable reference
    std::string getVarNameFromExpr(const Expression *expr) const;
    // Transfer ownership from a variable (marks it as moved if non-primitive)
    void transferOwnership(const std::string &varName);

    // Symbol table for variables
    SymbolTable symTab;

    // Current function's return type (for validating return statements)
    Type currentFunctionReturnType = Primitive::Void;

    // Map of class names to their field types (used for field access
    // resolution).  Populated from ClassDecls.
    std::unordered_map<std::string, std::vector<std::pair<Type, std::string>>> classDefs;

    // Map of function names to their signatures (return type and param types).
    std::unordered_map<std::string, std::pair<Type, std::vector<Type>>> funcSigs;

    // Diagnostics collected during analysis
    Diagnostics diagnostics;
};

} // namespace sisyl

#endif // SISYL_SEMANTIC_ANALYZER_H