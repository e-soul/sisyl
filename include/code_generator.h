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

#include "ast.h"

#include <expected>
#include <filesystem>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

namespace sisyl {

using CodegenDiagnostics = std::vector<std::string>;

/**
 * Walks the semantically analysed AST and emits LLVM IR using
 * the LLVM C++ API. The resulting IR can be compiled to machine
 * code via LLVM's JIT or ahead-of-time compiler.
 */
class CodeGenerator {
  public:
    CodeGenerator();
    ~CodeGenerator() = default;

    [[nodiscard]] std::expected<std::string, CodegenDiagnostics> generateIR(const std::shared_ptr<Program> &program);

    [[nodiscard]] static std::expected<void, std::string> writeIRToFile(const std::filesystem::path &filename, std::string_view irText);

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
    std::unique_ptr<llvm::LLVMContext> context;
    std::unique_ptr<llvm::Module> module;
    std::unique_ptr<llvm::IRBuilder<>> builder;

    std::unordered_map<std::string, llvm::StructType *> classTypes;
    std::unordered_map<std::string, llvm::AllocaInst *> namedValues;
    std::unordered_map<std::string, Type> variableTypes;
    std::unordered_map<std::string, llvm::Function *> functions;
    std::unordered_map<std::string, std::vector<std::pair<Type, std::string>>> classFields;

    llvm::Value *lastValue;
    llvm::Function *currentFunction;

    llvm::Type *getLLVMType(const Type &type);

    llvm::AllocaInst *createEntryBlockAlloca(llvm::Function *func, const std::string &varName, llvm::Type *type);

    int getFieldIndex(const std::string &className, const std::string &fieldName);
};

} // namespace sisyl
