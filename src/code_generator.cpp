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
#include "code_generator.h"
#include "type.h"

#include <fstream>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Support/raw_ostream.h>

using namespace sisyl;

CodeGenerator::CodeGenerator() {
    lastValue = nullptr;
    currentFunction = nullptr;
}

std::expected<std::string, CodegenDiagnostics> CodeGenerator::generateIR(const std::shared_ptr<Program> &program) {
    if (!program) {
        return std::unexpected(CodegenDiagnostics{"Internal error: null program"});
    }

    // Re-initialize module state per invocation.
    context = std::make_unique<llvm::LLVMContext>();
    module = std::make_unique<llvm::Module>("sisyl_module", *context);
    builder = std::make_unique<llvm::IRBuilder<>>(*context);
    lastValue = nullptr;
    currentFunction = nullptr;

    // Clear any per-run caches that may hold pointers into the previous module/context.
    classTypes.clear();
    namedValues.clear();
    variableTypes.clear();
    functions.clear();
    classFields.clear();

    program->accept(*this);

    std::string irStr;
    llvm::raw_string_ostream stream(irStr);
    module->print(stream, nullptr);
    return stream.str();
}

std::expected<void, std::string> CodeGenerator::writeIRToFile(const std::filesystem::path &filename, std::string_view irText) {
    std::ofstream file(filename, std::ios::binary);
    if (!file) {
        return std::unexpected(std::string{"Failed to open output file: "} + filename.string());
    }
    file.write(irText.data(), static_cast<std::streamsize>(irText.size()));
    if (!file) {
        return std::unexpected(std::string{"Failed to write output file: "} + filename.string());
    }
    return {};
}

llvm::Type *CodeGenerator::getLLVMType(const Type &type) {
    if (const auto *prim = std::get_if<Primitive>(&type)) {
        switch (*prim) {
        case Primitive::Int64:
            return llvm::Type::getInt64Ty(*context);
        case Primitive::Bool:
            return llvm::Type::getInt1Ty(*context);
        case Primitive::Str:
            return llvm::PointerType::get(*context, 0);
        case Primitive::Void:
            return llvm::Type::getVoidTy(*context);
        }
    }

    // User-defined class type - use opaque pointer.
    const std::string &structName = std::get<std::string>(type);
    if (classTypes.find(structName) != classTypes.end()) {
        return llvm::PointerType::get(*context, 0);
    }

    // Unknown type - default to i64 to stay consistent with Int64.
    return llvm::Type::getInt64Ty(*context);
}

llvm::AllocaInst *CodeGenerator::createEntryBlockAlloca(llvm::Function *func, const std::string &varName, llvm::Type *type) {
    llvm::IRBuilder<> tmpBuilder(&func->getEntryBlock(), func->getEntryBlock().begin());
    return tmpBuilder.CreateAlloca(type, nullptr, varName);
}

int CodeGenerator::getFieldIndex(const std::string &className, const std::string &fieldName) {
    auto it = classFields.find(className);
    if (it == classFields.end())
        return -1;

    const auto &fields = it->second;
    for (size_t i = 0; i < fields.size(); ++i) {
        if (fields[i].second == fieldName) {
            return static_cast<int>(i);
        }
    }
    return -1;
}

void CodeGenerator::visit(Program &node) {
    // First pass: declare all class types
    for (auto &c : node.classes) {
        c->accept(*this);
    }

    // Second pass: declare all function prototypes
    for (auto &f : node.functions) {
        // Create function type
        std::vector<llvm::Type *> paramTypes;
        for (const auto &param : f->params) {
            paramTypes.push_back(getLLVMType(param.type));
        }
        llvm::Type *returnType = getLLVMType(f->returnType);
        llvm::FunctionType *funcType = llvm::FunctionType::get(returnType, paramTypes, false);

        llvm::Function *func = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, f->name, module.get());

        // Name the parameters
        unsigned idx = 0;
        for (auto &arg : func->args()) {
            arg.setName(f->params[idx++].name);
        }

        functions[f->name] = func;
    }

    // Third pass: generate function bodies
    for (auto &f : node.functions) {
        f->accept(*this);
    }
}

void CodeGenerator::visit(ClassDecl &node) {
    std::vector<llvm::Type *> fieldTypes;
    std::vector<std::pair<Type, std::string>> fieldInfo;

    for (const auto &field : node.fields) {
        llvm::Type *fieldType;
        const Type &type = field.first;
        const std::string &fieldName = field.second;

        if (const auto *prim = std::get_if<Primitive>(&type)) {
            switch (*prim) {
            case Primitive::Int64:
                fieldType = llvm::Type::getInt64Ty(*context);
                break;
            case Primitive::Bool:
                fieldType = llvm::Type::getInt1Ty(*context);
                break;
            case Primitive::Str:
                fieldType = llvm::PointerType::get(*context, 0);
                break;
            case Primitive::Void:
                // Void fields are nonsensical; use i8 to keep LLVM happy.
                fieldType = llvm::Type::getInt8Ty(*context);
                break;
            }
        } else {
            // Nested class - use opaque pointer
            fieldType = llvm::PointerType::get(*context, 0);
        }

        fieldTypes.push_back(fieldType);
        fieldInfo.push_back({type, fieldName});
    }

    llvm::StructType *structType = llvm::StructType::create(*context, fieldTypes, node.name);
    classTypes[node.name] = structType;
    classFields[node.name] = fieldInfo;
}

void CodeGenerator::visit(FuncDecl &node) {
    llvm::Function *func = functions[node.name];
    if (!func)
        return;

    currentFunction = func;

    llvm::BasicBlock *entryBB = llvm::BasicBlock::Create(*context, "entry", func);
    builder->SetInsertPoint(entryBB);

    namedValues.clear();
    variableTypes.clear();

    // Create allocas for function parameters and track their types
    unsigned paramIdx = 0;
    for (auto &arg : func->args()) {
        llvm::AllocaInst *alloca = createEntryBlockAlloca(func, std::string(arg.getName()), arg.getType());
        builder->CreateStore(&arg, alloca);
        namedValues[std::string(arg.getName())] = alloca;
        // Track the SiSyL type
        if (paramIdx < node.params.size()) {
            variableTypes[std::string(arg.getName())] = node.params[paramIdx].type;
        }
        ++paramIdx;
    }

    // Generate function body
    if (node.body) {
        node.body->accept(*this);
    }

    // If the function doesn't end with a return, add one
    if (!builder->GetInsertBlock()->getTerminator()) {
        if (func->getReturnType()->isVoidTy()) {
            builder->CreateRetVoid();
        } else {
            // Return default value
            builder->CreateRet(llvm::Constant::getNullValue(func->getReturnType()));
        }
    }

    currentFunction = nullptr;
}

void CodeGenerator::visit(BlockStmt &node) {
    for (auto &stmt : node.statements) {
        stmt->accept(*this);
        if (builder->GetInsertBlock()->getTerminator()) {
            break;
        }
    }
}

void CodeGenerator::visit(VarDeclStmt &node) {
    llvm::Type *varType = getLLVMType(node.typeName);
    llvm::AllocaInst *alloca = createEntryBlockAlloca(currentFunction, node.name, varType);
    namedValues[node.name] = alloca;
    variableTypes[node.name] = node.typeName;

    if (node.initExpr) {
        node.initExpr->accept(*this);
        if (lastValue) {
            builder->CreateStore(lastValue, alloca);
        }
    }
}

void CodeGenerator::visit(AssignStmt &node) {
    // Evaluate the RHS expression
    if (node.expr) {
        node.expr->accept(*this);
    }
    llvm::Value *value = lastValue;

    // Handle the LHS (location)
    if (auto *varRef = dynamic_cast<VarRef *>(node.location.get())) {
        auto it = namedValues.find(varRef->name);
        if (it != namedValues.end()) {
            builder->CreateStore(value, it->second);
        }
    } else if (auto *fieldAccess = dynamic_cast<FieldAccess *>(node.location.get())) {
        // Handle field access assignment
        if (fieldAccess->path.size() >= 2) {
            std::string varName = fieldAccess->path[0];
            auto it = namedValues.find(varName);
            auto typeIt = variableTypes.find(varName);

            if (it != namedValues.end() && typeIt != variableTypes.end()) {
                // Load the class instance pointer
                llvm::AllocaInst *alloca = it->second;
                Type currentType = typeIt->second;

                // Check if this is a class pointer type
                const auto *currentStructName = std::get_if<std::string>(&currentType);
                if (!currentStructName) {
                    return; // Not a class type
                }

                auto structIt = classTypes.find(*currentStructName);
                if (structIt == classTypes.end()) {
                    return; // Unknown class type
                }

                llvm::Value *ptr = builder->CreateLoad(llvm::PointerType::get(*context, 0), alloca, varName);

                // Navigate through intermediate fields
                llvm::StructType *currentStruct = structIt->second;
                for (size_t i = 1; i < fieldAccess->path.size(); ++i) {
                    int fieldIdx = getFieldIndex(*currentStructName, fieldAccess->path[i]);
                    if (fieldIdx < 0)
                        break;

                    llvm::Value *gep = builder->CreateStructGEP(currentStruct, ptr, static_cast<unsigned>(fieldIdx), fieldAccess->path[i]);

                    if (i == fieldAccess->path.size() - 1) {
                        // Last field - store the value
                        builder->CreateStore(value, gep);
                    } else {
                        // Get the type of this field for next iteration
                        auto fieldIt = classFields.find(*currentStructName);
                        if (fieldIt != classFields.end() && fieldIdx < static_cast<int>(fieldIt->second.size())) {
                            currentType = fieldIt->second[fieldIdx].first;
                            currentStructName = std::get_if<std::string>(&currentType);
                            if (!currentStructName)
                                return;

                            auto nextStructIt = classTypes.find(*currentStructName);
                            if (nextStructIt == classTypes.end())
                                return;

                            currentStruct = nextStructIt->second;
                            ptr = builder->CreateLoad(llvm::PointerType::get(*context, 0), gep);
                        }
                    }
                }
            }
        }
    }
}

void CodeGenerator::visit(IfStmt &node) {
    node.condition->accept(*this);
    llvm::Value *condValue = lastValue;

    if (!condValue->getType()->isIntegerTy(1)) {
        condValue = builder->CreateICmpNE(condValue, llvm::Constant::getNullValue(condValue->getType()), "ifcond");
    }

    llvm::Function *func = currentFunction;
    llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(*context, "then", func);
    llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(*context, "else");
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*context, "ifcont");

    if (node.elseBranch) {
        builder->CreateCondBr(condValue, thenBB, elseBB);
    } else {
        builder->CreateCondBr(condValue, thenBB, mergeBB);
    }

    // Generate then branch
    builder->SetInsertPoint(thenBB);
    if (node.thenBranch) {
        node.thenBranch->accept(*this);
    }
    if (!builder->GetInsertBlock()->getTerminator()) {
        builder->CreateBr(mergeBB);
    }

    // Generate else branch
    if (node.elseBranch) {
        func->insert(func->end(), elseBB);
        builder->SetInsertPoint(elseBB);
        node.elseBranch->accept(*this);
        if (!builder->GetInsertBlock()->getTerminator()) {
            builder->CreateBr(mergeBB);
        }
    }

    // Continue at merge block
    func->insert(func->end(), mergeBB);
    builder->SetInsertPoint(mergeBB);
}

void CodeGenerator::visit(WhileStmt &node) {
    llvm::Function *func = currentFunction;
    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*context, "whilecond", func);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*context, "whilebody");
    llvm::BasicBlock *afterBB = llvm::BasicBlock::Create(*context, "whileend");

    // Jump to condition block
    builder->CreateBr(condBB);

    // Generate condition
    builder->SetInsertPoint(condBB);
    node.condition->accept(*this);
    llvm::Value *condValue = lastValue;

    // Convert to bool if needed
    if (!condValue->getType()->isIntegerTy(1)) {
        condValue = builder->CreateICmpNE(condValue, llvm::Constant::getNullValue(condValue->getType()), "whilecond");
    }

    builder->CreateCondBr(condValue, bodyBB, afterBB);

    // Generate body
    func->insert(func->end(), bodyBB);
    builder->SetInsertPoint(bodyBB);
    if (node.body) {
        node.body->accept(*this);
    }
    if (!builder->GetInsertBlock()->getTerminator()) {
        builder->CreateBr(condBB);
    }

    // Continue after loop
    func->insert(func->end(), afterBB);
    builder->SetInsertPoint(afterBB);
}

void CodeGenerator::visit(ReturnStmt &node) {
    if (node.expr) {
        node.expr->accept(*this);
        builder->CreateRet(lastValue);
    } else {
        builder->CreateRetVoid();
    }
}

void CodeGenerator::visit(ExprStmt &node) {
    if (node.expr) {
        node.expr->accept(*this);
    }
}

void CodeGenerator::visit(IntLiteral &node) { lastValue = llvm::ConstantInt::get(*context, llvm::APInt(64, static_cast<std::uint64_t>(node.value), true)); }

void CodeGenerator::visit(BoolLiteral &node) { lastValue = llvm::ConstantInt::get(*context, llvm::APInt(1, node.value ? 1 : 0)); }

void CodeGenerator::visit(StringLiteral &node) { lastValue = builder->CreateGlobalString(node.value, "str"); }

void CodeGenerator::visit(VarRef &node) {
    auto it = namedValues.find(node.name);
    if (it != namedValues.end()) {
        llvm::AllocaInst *alloca = it->second;
        lastValue = builder->CreateLoad(alloca->getAllocatedType(), alloca, node.name);
    } else {
        lastValue = nullptr;
    }
}

void CodeGenerator::visit(FieldAccess &node) {
    if (node.path.empty()) {
        lastValue = nullptr;
        return;
    }

    // Start with the base variable
    std::string baseName = node.path[0];
    auto it = namedValues.find(baseName);
    auto typeIt = variableTypes.find(baseName);

    if (it == namedValues.end() || typeIt == variableTypes.end()) {
        lastValue = nullptr;
        return;
    }

    llvm::AllocaInst *alloca = it->second;
    Type currentType = typeIt->second;

    // Check if this is a class pointer type
    const auto *currentStructName = std::get_if<std::string>(&currentType);
    if (!currentStructName) {
        lastValue = nullptr;
        return;
    }

    auto structIt = classTypes.find(*currentStructName);
    if (structIt == classTypes.end()) {
        lastValue = nullptr;
        return;
    }

    // Load the class instance pointer
    llvm::Value *ptr = builder->CreateLoad(llvm::PointerType::get(*context, 0), alloca, baseName);

    llvm::StructType *currentStruct = structIt->second;

    // Navigate through the rest of the path
    for (size_t i = 1; i < node.path.size(); ++i) {
        int fieldIdx = getFieldIndex(*currentStructName, node.path[i]);
        if (fieldIdx < 0) {
            lastValue = nullptr;
            return;
        }

        llvm::Value *gep = builder->CreateStructGEP(currentStruct, ptr, static_cast<unsigned>(fieldIdx), node.path[i]);

        if (i == node.path.size() - 1) {
            // Last field - load the value
            llvm::Type *fieldType = currentStruct->getElementType(static_cast<unsigned>(fieldIdx));
            lastValue = builder->CreateLoad(fieldType, gep, node.path[i] + "_val");
        } else {
            // Intermediate - get the next class type
            auto fieldIt = classFields.find(*currentStructName);
            if (fieldIt != classFields.end() && fieldIdx < static_cast<int>(fieldIt->second.size())) {
                currentType = fieldIt->second[fieldIdx].first;
                currentStructName = std::get_if<std::string>(&currentType);
                if (!currentStructName) {
                    lastValue = nullptr;
                    return;
                }

                auto nextStructIt = classTypes.find(*currentStructName);
                if (nextStructIt == classTypes.end()) {
                    lastValue = nullptr;
                    return;
                }

                currentStruct = nextStructIt->second;
                ptr = builder->CreateLoad(llvm::PointerType::get(*context, 0), gep);
            } else {
                lastValue = nullptr;
                return;
            }
        }
    }
}

void CodeGenerator::visit(UnaryOp &node) {
    node.operand->accept(*this);
    llvm::Value *operand = lastValue;

    if (node.op == "-") {
        lastValue = builder->CreateNeg(operand, "negtmp");
    } else if (node.op == "!") {
        // Logical not - compare with 0 and negate
        if (!operand->getType()->isIntegerTy(1)) {
            operand = builder->CreateICmpNE(operand, llvm::Constant::getNullValue(operand->getType()), "tobool");
        }
        lastValue = builder->CreateNot(operand, "nottmp");
    } else {
        lastValue = operand;
    }
}

void CodeGenerator::visit(BinaryOp &node) {
    // Special handling for short-circuit operators
    if (node.op == "&&" || node.op == "||") {
        node.left->accept(*this);
        llvm::Value *lhs = lastValue;

        // Convert to i1 if needed
        if (!lhs->getType()->isIntegerTy(1)) {
            lhs = builder->CreateICmpNE(lhs, llvm::Constant::getNullValue(lhs->getType()), "tobool");
        }

        llvm::Function *func = currentFunction;
        llvm::BasicBlock *rhsBB = llvm::BasicBlock::Create(*context, "rhs", func);
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*context, "merge");

        llvm::BasicBlock *startBB = builder->GetInsertBlock();

        if (node.op == "&&") {
            // Short-circuit AND: if LHS is false, skip RHS
            builder->CreateCondBr(lhs, rhsBB, mergeBB);
        } else {
            // Short-circuit OR: if LHS is true, skip RHS
            builder->CreateCondBr(lhs, mergeBB, rhsBB);
        }

        // Evaluate RHS
        builder->SetInsertPoint(rhsBB);
        node.right->accept(*this);
        llvm::Value *rhs = lastValue;
        if (!rhs->getType()->isIntegerTy(1)) {
            rhs = builder->CreateICmpNE(rhs, llvm::Constant::getNullValue(rhs->getType()), "tobool");
        }
        llvm::BasicBlock *rhsEndBB = builder->GetInsertBlock();
        builder->CreateBr(mergeBB);

        // Merge
        func->insert(func->end(), mergeBB);
        builder->SetInsertPoint(mergeBB);
        llvm::PHINode *phi = builder->CreatePHI(llvm::Type::getInt1Ty(*context), 2, "logictmp");
        phi->addIncoming(lhs, startBB);
        phi->addIncoming(rhs, rhsEndBB);
        lastValue = phi;
        return;
    }

    // Evaluate both operands
    node.left->accept(*this);
    llvm::Value *lhs = lastValue;
    node.right->accept(*this);
    llvm::Value *rhs = lastValue;

    if (!lhs || !rhs) {
        lastValue = nullptr;
        return;
    }

    // Arithmetic operators
    if (node.op == "+") {
        lastValue = builder->CreateAdd(lhs, rhs, "addtmp");
    } else if (node.op == "-") {
        lastValue = builder->CreateSub(lhs, rhs, "subtmp");
    } else if (node.op == "*") {
        lastValue = builder->CreateMul(lhs, rhs, "multmp");
    } else if (node.op == "/") {
        lastValue = builder->CreateSDiv(lhs, rhs, "divtmp");
    }
    // Comparison operators
    else if (node.op == "==") {
        lastValue = builder->CreateICmpEQ(lhs, rhs, "eqtmp");
    } else if (node.op == "!=") {
        lastValue = builder->CreateICmpNE(lhs, rhs, "netmp");
    } else if (node.op == "<") {
        lastValue = builder->CreateICmpSLT(lhs, rhs, "lttmp");
    } else if (node.op == ">") {
        lastValue = builder->CreateICmpSGT(lhs, rhs, "gttmp");
    } else if (node.op == "<=") {
        lastValue = builder->CreateICmpSLE(lhs, rhs, "letmp");
    } else if (node.op == ">=") {
        lastValue = builder->CreateICmpSGE(lhs, rhs, "getmp");
    } else {
        lastValue = nullptr;
    }
}

void CodeGenerator::visit(FuncCall &node) {
    llvm::Function *callee = module->getFunction(node.callee);

    // Handle built-in functions
    if (!callee) {
        // Check for print built-in
        if (node.callee == "print") {
            // Declare printf if not already declared
            llvm::Function *printfFunc = module->getFunction("printf");
            if (!printfFunc) {
                std::vector<llvm::Type *> printfArgs;
                printfArgs.push_back(llvm::PointerType::get(*context, 0));
                llvm::FunctionType *printfType = llvm::FunctionType::get(llvm::Type::getInt32Ty(*context), printfArgs, true);
                printfFunc = llvm::Function::Create(printfType, llvm::Function::ExternalLinkage, "printf", module.get());
            }

            // Generate arguments
            std::vector<llvm::Value *> args;
            for (auto &arg : node.args) {
                arg->accept(*this);
                if (lastValue) {
                    // If it's an integer, create format string
                    if (lastValue->getType()->isIntegerTy(64)) {
                        llvm::Value *formatStr = builder->CreateGlobalString("%lld\n", "fmt_int64");
                        args.push_back(formatStr);
                        args.push_back(lastValue);
                    } else if (lastValue->getType()->isIntegerTy(1)) {
                        // Bool - print as int
                        llvm::Value *formatStr = builder->CreateGlobalString("%lld\n", "fmt_bool");
                        llvm::Value *extended = builder->CreateZExt(lastValue, llvm::Type::getInt64Ty(*context));
                        args.push_back(formatStr);
                        args.push_back(extended);
                    } else if (lastValue->getType()->isPointerTy()) {
                        // Assume string
                        llvm::Value *formatStr = builder->CreateGlobalString("%s\n", "fmt_str");
                        args.push_back(formatStr);
                        args.push_back(lastValue);
                    }
                }
            }

            if (!args.empty()) {
                lastValue = builder->CreateCall(printfFunc, args, "printcall");
            } else {
                lastValue = nullptr;
            }
            return;
        }

        lastValue = nullptr;
        return;
    }

    // Generate argument values
    std::vector<llvm::Value *> args;
    for (auto &arg : node.args) {
        arg->accept(*this);
        if (lastValue) {
            args.push_back(lastValue);
        }
    }

    if (callee->getReturnType()->isVoidTy()) {
        builder->CreateCall(callee, args);
        lastValue = nullptr;
    } else {
        lastValue = builder->CreateCall(callee, args, "calltmp");
    }
}

void CodeGenerator::visit(NewExpr &node) {
    auto it = classTypes.find(node.typeName);
    if (it == classTypes.end()) {
        lastValue = nullptr;
        return;
    }

    llvm::StructType *structType = it->second;

    llvm::Function *mallocFunc = module->getFunction("malloc");
    if (!mallocFunc) {
        std::vector<llvm::Type *> mallocArgs;
        mallocArgs.push_back(llvm::Type::getInt64Ty(*context));
        llvm::FunctionType *mallocType = llvm::FunctionType::get(llvm::PointerType::get(*context, 0), mallocArgs, false);
        mallocFunc = llvm::Function::Create(mallocType, llvm::Function::ExternalLinkage, "malloc", module.get());
    }

    // Calculate size of class instance
    const llvm::DataLayout &dataLayout = module->getDataLayout();
    uint64_t structSize = dataLayout.getTypeAllocSize(structType);

    // Allocate memory
    llvm::Value *sizeVal = llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), structSize);
    lastValue = builder->CreateCall(mallocFunc, {sizeVal}, "malloctmp");

    // Zero-initialize the class fields using GEP
    const auto &fields = classFields[node.typeName];
    for (size_t i = 0; i < fields.size(); ++i) {
        llvm::Value *fieldPtr = builder->CreateStructGEP(structType, lastValue, static_cast<unsigned>(i));
        llvm::Type *fieldType = structType->getElementType(static_cast<unsigned>(i));
        builder->CreateStore(llvm::Constant::getNullValue(fieldType), fieldPtr);
    }
}
