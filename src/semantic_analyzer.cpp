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
 * semantic_analyzer.cpp
 *
 * Implements the semantic analysis pass for the SiSyL compiler.  This
 * file contains definitions for SymbolTable methods and SemanticAnalyzer
 * visit functions.  The current implementation performs only basic checks
 * sufficient for the MVP skeleton: it builds symbol tables, detects
 * undeclared variables and collects type information.  More advanced
 * ownership checking and type inference could be added later.
 */

#include "semantic_analyzer.h"

using namespace sisyl;

///////////////////////////////////////////////////////////////////////////////
// SymbolTable implementation
///////////////////////////////////////////////////////////////////////////////

bool SymbolTable::insert(const std::string &name, const Type &type, bool owned) {
    if (scopes.empty()) {
        // Initialize global scope if not yet created
        scopes.emplace_back();
    }
    auto &current = scopes.back();
    if (current.find(name) != current.end()) {
        return false; // duplicate
    }
    current[name] = Symbol{name, type, owned, false}; // isMoved starts as false
    return true;
}

Symbol *SymbolTable::lookup(const std::string &name) {
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
        auto found = it->find(name);
        if (found != it->end()) {
            return &found->second;
        }
    }
    return nullptr;
}

void SymbolTable::pushScope() {
    scopes.emplace_back();
}

void SymbolTable::popScope() {
    if (!scopes.empty()) {
        scopes.pop_back();
    }
}

void SymbolTable::markMoved(const std::string &name) {
    Symbol *sym = lookup(name);
    if (sym) {
        sym->isMoved = true;
    }
}

bool SymbolTable::isMoved(const std::string &name) {
    Symbol *sym = lookup(name);
    return sym && sym->isMoved;
}

///////////////////////////////////////////////////////////////////////////////
// SemanticAnalyzer implementation
///////////////////////////////////////////////////////////////////////////////

SemanticAnalyzer::SemanticAnalyzer() {
    currentFunctionReturnType = Primitive::Void;
}

std::expected<void, Diagnostics> SemanticAnalyzer::analyze(const std::shared_ptr<Program> &program) {
    diagnostics.clear();
    // Collect class and function declarations first
    program->accept(*this);
    if (!diagnostics.empty()) {
        return std::unexpected(diagnostics);
    }
    return {};
}

void SemanticAnalyzer::error(const std::string &msg) {
    diagnostics.push_back(msg);
}

std::string SemanticAnalyzer::getVarNameFromExpr(const Expression *expr) const {
    if (auto *varRef = dynamic_cast<const VarRef *>(expr)) {
        return varRef->name;
    }
    return "";
}

void SemanticAnalyzer::transferOwnership(const std::string &varName) {
    if (varName.empty()) return;
    Symbol *sym = symTab.lookup(varName);
    if (sym && !isPrimitive(sym->type)) {
        symTab.markMoved(varName);
    }
}

// Program: process classes then functions
void SemanticAnalyzer::visit(Program &node) {
    // First collect class definitions
    for (auto &cd : node.classes) {
        cd->accept(*this);
    }
    // Then collect function signatures
    for (auto &fd : node.functions) {
        std::vector<Type> paramTypes;
        paramTypes.reserve(fd->params.size());
        for (auto &p : fd->params) {
            paramTypes.push_back(p.type);
        }

        if (!funcSigs.try_emplace(fd->name, std::make_pair(fd->returnType, std::move(paramTypes))).second) {
            error("Duplicate function declaration: " + fd->name);
        }
    }
    // Now analyze each function body
    for (auto &fd : node.functions) {
        fd->accept(*this);
    }
}

// Class declaration: record fields
void SemanticAnalyzer::visit(ClassDecl &node) {
    if (!classDefs.try_emplace(node.name, node.fields).second) {
        error("Duplicate class declaration: " + node.name);
    }
}

// Function declaration: push a new scope, insert parameters then analyze body
void SemanticAnalyzer::visit(FuncDecl &node) {
    symTab.pushScope();
    // Track current function's return type for validating return statements
    Type previousReturnType = currentFunctionReturnType;
    currentFunctionReturnType = node.returnType;
    // Insert parameters into the symbol table.  Primitive types are copied,
    // non‑primitives are owned by the function.  The function body must move
    // them when passed to another function or returned.
    for (const auto &param : node.params) {
        bool owned = !isPrimitive(param.type);
        if (!symTab.insert(param.name, param.type, owned)) {
            error("Duplicate parameter name: " + param.name);
        }
    }
    // Analyse function body
    if (node.body) {
        node.body->accept(*this);
    }
    symTab.popScope();
    currentFunctionReturnType = previousReturnType;
}

// Block: push scope, visit statements, pop scope
void SemanticAnalyzer::visit(BlockStmt &node) {
    symTab.pushScope();
    for (auto &stmt : node.statements) {
        stmt->accept(*this);
    }
    symTab.popScope();
}

// Variable declaration: insert variable and analyse initialiser if present
void SemanticAnalyzer::visit(VarDeclStmt &node) {
    // Disallow variables of type Void
    if (isVoid(node.typeName)) {
        error("Variable '" + node.name + "' cannot have type Void");
    }

    // Check that the declared class type exists (primitives always exist)
    if (const auto *structName = std::get_if<std::string>(&node.typeName)) {
        if (classDefs.find(*structName) == classDefs.end()) {
            error("Unknown type in variable declaration: " + *structName);
        }
    }

    bool owned = !isPrimitive(node.typeName);
    if (!symTab.insert(node.name, node.typeName, owned)) {
        error("Redeclaration of variable: " + node.name);
    }
    if (node.initExpr) {
        node.initExpr->accept(*this);
        // Type check: initializer type must match declared type
        if (node.initExpr->type && !isSameType(*node.initExpr->type, node.typeName)) {
            error("Type mismatch in initialization of '" + node.name +
                  "': expected " + toString(node.typeName) + ", got " + toString(*node.initExpr->type));
        }
        // Handle ownership transfer: if initializing from a variable with a
        // non-primitive type, that variable is moved
        std::string sourceVar = getVarNameFromExpr(node.initExpr.get());
        if (!sourceVar.empty() && !isPrimitive(node.typeName)) {
            transferOwnership(sourceVar);
        }
    }
}

// Assignment: analyse both sides and ensure the destination is declared
void SemanticAnalyzer::visit(AssignStmt &node) {
    // For the location (LHS), we need special handling: if it's a simple variable
    // that was moved, we should NOT report use-after-move since we're reassigning it.
    // We only need to verify the variable exists and get its type.
    std::string destVar;
    if (node.location) {
        destVar = getVarNameFromExpr(node.location.get());
        if (!destVar.empty()) {
            // Simple variable assignment - check it exists but allow moved variables
            Symbol *sym = symTab.lookup(destVar);
            if (!sym) {
                error("Use of undeclared variable: " + destVar);
                node.location->type.reset();
            } else {
                node.location->type = sym->type;
            }
        } else {
            // Field access or other complex location - use normal analysis
            node.location->accept(*this);
        }
    }
    
    if (node.expr) {
        node.expr->accept(*this);
    }
    
    // Type check: RHS type must match LHS type
    if (node.location && node.expr) {
        if (node.location->type && node.expr->type && !isSameType(*node.location->type, *node.expr->type)) {
            error("Type mismatch in assignment: cannot assign " + toString(*node.expr->type) + " to " + toString(*node.location->type));
        }
        
        // Handle ownership transfer for non-primitive types
        std::string sourceVar = getVarNameFromExpr(node.expr.get());
        if (!sourceVar.empty() && node.expr->type && !isPrimitive(*node.expr->type)) {
            transferOwnership(sourceVar);
        }
        
        // If assigning to a variable that was moved, restore its validity
        if (!destVar.empty()) {
            Symbol *sym = symTab.lookup(destVar);
            if (sym && sym->isMoved) {
                sym->isMoved = false; // Variable now has a new valid value
            }
        }
    }
}

// If statement: analyse condition and both branches
void SemanticAnalyzer::visit(IfStmt &node) {
    if (node.condition) {
        node.condition->accept(*this);
    }
    if (node.thenBranch) {
        node.thenBranch->accept(*this);
    }
    if (node.elseBranch) {
        node.elseBranch->accept(*this);
    }
}

// While statement: analyse condition and body
void SemanticAnalyzer::visit(WhileStmt &node) {
    if (node.condition) {
        node.condition->accept(*this);
    }
    if (node.body) {
        node.body->accept(*this);
    }
}

// Return statement: analyse returned expression if present
void SemanticAnalyzer::visit(ReturnStmt &node) {
    if (node.expr) {
        node.expr->accept(*this);
        
        // Check for returning value from void function
        if (isVoid(currentFunctionReturnType)) {
            error("Cannot return a value from void function");
            return;
        }
        
        // Validate return type matches function's declared return type
        if (node.expr->type && !isSameType(*node.expr->type, currentFunctionReturnType)) {
            error("Return type mismatch: expected " + toString(currentFunctionReturnType) +
                  ", got " + toString(*node.expr->type));
        }
        // Handle ownership transfer for non-primitive return values
        std::string sourceVar = getVarNameFromExpr(node.expr.get());
        if (!sourceVar.empty() && node.expr->type && !isPrimitive(*node.expr->type)) {
            transferOwnership(sourceVar);
        }
    } else if (!isVoid(currentFunctionReturnType)) {
        // Missing return expression for non-void function
        error("Missing return value for non-void function returning " + toString(currentFunctionReturnType));
    }
    // Note: bare "return;" in a void function is valid - no error needed
}

// Expression statement: analyse expression
void SemanticAnalyzer::visit(ExprStmt &node) {
    if (node.expr) {
        node.expr->accept(*this);
    }
}

// Literals: set type
void SemanticAnalyzer::visit(IntLiteral &node) {
    node.type = Primitive::Int64;
}
void SemanticAnalyzer::visit(BoolLiteral &node) {
    node.type = Primitive::Bool;
}
void SemanticAnalyzer::visit(StringLiteral &node) {
    node.type = Primitive::Str;
}

// Variable reference: look up symbol and set type
void SemanticAnalyzer::visit(VarRef &node) {
    Symbol *sym = symTab.lookup(node.name);
    if (!sym) {
        error("Use of undeclared variable: " + node.name);
        node.type.reset();
    } else {
        // Check for use-after-move
        if (sym->isMoved) {
            error("Use of moved variable: " + node.name);
            node.type.reset();
        } else {
            node.type = sym->type;
        }
    }
}

// Field access: lookup base variable then check class definitions
void SemanticAnalyzer::visit(FieldAccess &node) {
    if (node.path.empty()) {
        error("Empty field access");
        return;
    }
    // Start from the first identifier
    auto it = node.path.begin();
    std::optional<Type> currentType;
    Symbol *sym = symTab.lookup(*it);
    if (!sym) {
        error("Use of undeclared variable in field access: " + *it);
        node.type.reset();
        return;
    }
    // Check for use-after-move on base variable
    if (sym->isMoved) {
        error("Use of moved variable in field access: " + *it);
        node.type.reset();
        return;
    }
    currentType = sym->type;
    ++it;
    while (it != node.path.end()) {
        // currentType should be a class; find field type
        if (!currentType || !std::holds_alternative<std::string>(*currentType)) {
            error("Field access on non-class type: " + (currentType ? toString(*currentType) : std::string{"<unknown>"}));
            node.type.reset();
            return;
        }

        const std::string &structName = std::get<std::string>(*currentType);
        auto defIt = classDefs.find(structName);
        if (defIt == classDefs.end()) {
            error("Field access on unknown class type: " + structName);
            node.type.reset();
            return;
        }
        // find field with this name
        bool found = false;
        for (const auto &field : defIt->second) {
            if (field.second == *it) {
                currentType = field.first;
                found = true;
                break;
            }
        }
        if (!found) {
            error("Unknown field '" + *it + "' in class " + structName);
            node.type.reset();
            return;
        }
        ++it;
    }
    if (currentType) {
        node.type = *currentType;
    }
}

// Unary op: analyse operand and set type based on operator
void SemanticAnalyzer::visit(UnaryOp &node) {
    if (node.operand) {
        node.operand->accept(*this);
    }
    // For a simple MVP we handle logical not and negation only for boolean and
    // integer types respectively.
    if (node.op == "!") {
        if (!node.operand || !node.operand->type || !isBool(*node.operand->type)) {
            error("Operator '!' requires Bool operand");
        }
        node.type = Primitive::Bool;
    } else if (node.op == "-") {
        if (!node.operand || !node.operand->type || !isInt(*node.operand->type)) {
            error("Unary '-' requires Int64 operand");
        }
        node.type = Primitive::Int64;
    } else {
        error("Unknown unary operator: " + node.op);
        node.type.reset();
    }
}

// Binary op: analyse operands and set result type if valid
void SemanticAnalyzer::visit(BinaryOp &node) {
    if (node.left) {
        node.left->accept(*this);
    }
    if (node.right) {
        node.right->accept(*this);
    }
    const std::optional<Type> lt = (node.left ? node.left->type : std::optional<Type>{});
    const std::optional<Type> rt = (node.right ? node.right->type : std::optional<Type>{});
    if (node.op == "+" || node.op == "-" || node.op == "*" || node.op == "/") {
        // arithmetic on ints only
        if (!lt || !rt || !isInt(*lt) || !isInt(*rt)) {
            error("Arithmetic operators require Int64 operands");
        }
        node.type = Primitive::Int64;
    } else if (node.op == "<" || node.op == ">" || node.op == "<=" || node.op == ">=") {
        if (!lt || !rt || !isInt(*lt) || !isInt(*rt)) {
            error("Relational operators require Int64 operands");
        }
        node.type = Primitive::Bool;
    } else if (node.op == "==" || node.op == "!=") {
        if (!lt || !rt || !isSameType(*lt, *rt)) {
            error("Equality operators require operands of same type");
        }
        node.type = Primitive::Bool;
    } else if (node.op == "&&" || node.op == "||") {
        if (!lt || !rt || !isBool(*lt) || !isBool(*rt)) {
            error("Logical operators require Bool operands");
        }
        node.type = Primitive::Bool;
    } else {
        error("Unknown binary operator: " + node.op);
        node.type.reset();
    }
}

// Function call: analyse arguments and set return type
void SemanticAnalyzer::visit(FuncCall &node) {
    // Look up function signature
    auto it = funcSigs.find(node.callee);
    if (it == funcSigs.end()) {
        error("Call to undeclared function: " + node.callee);
        node.type = "<error>";
        return;
    }
    const auto &sig = it->second;
    const Type &retType = sig.first;
    const auto &paramTypes = sig.second;
    if (paramTypes.size() != node.args.size()) {
        error("Function call argument count mismatch for: " + node.callee);
    }
    // Analyse arguments and handle ownership transfer
    for (size_t i = 0; i < node.args.size(); ++i) {
        node.args[i]->accept(*this);
        if (i < paramTypes.size()) {
            const Type &expected = paramTypes[i];
            const std::optional<Type> actual = node.args[i]->type;
            if (actual && !isSameType(*actual, expected)) {
                error("Type mismatch for argument " + std::to_string(i) + " of function " + node.callee +
                      ": expected " + toString(expected) + ", got " + toString(*actual));
            }
            // Transfer ownership for non-primitive arguments
            std::string argVar = getVarNameFromExpr(node.args[i].get());
            if (!argVar.empty() && !isPrimitive(expected)) {
                transferOwnership(argVar);
            }
        }
    }
    node.type = retType;
}

// Object allocation: check that type is a known class
void SemanticAnalyzer::visit(NewExpr &node) {
    if (classDefs.find(node.typeName) == classDefs.end()) {
        error("Allocation of unknown class type: " + node.typeName);
        node.type.reset();
    } else {
        node.type = Type{node.typeName};
    }
}