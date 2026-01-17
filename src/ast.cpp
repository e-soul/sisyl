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
#include "ast.h"
#include "code_generator.h"
#include "semantic_analyzer.h"

using namespace sisyl;

#define DEFINE_ACCEPT(NODE_CLASS)                                                                                                                              \
    void NODE_CLASS::accept(SemanticAnalyzer &visitor) { visitor.visit(*this); }                                                                               \
    void NODE_CLASS::accept(CodeGenerator &visitor) { visitor.visit(*this); }

// Expressions
DEFINE_ACCEPT(IntLiteral)
DEFINE_ACCEPT(BoolLiteral)
DEFINE_ACCEPT(StringLiteral)
DEFINE_ACCEPT(VarRef)
DEFINE_ACCEPT(FieldAccess)
DEFINE_ACCEPT(UnaryOp)
DEFINE_ACCEPT(BinaryOp)
DEFINE_ACCEPT(FuncCall)
DEFINE_ACCEPT(NewExpr)

// Statements
DEFINE_ACCEPT(BlockStmt)
DEFINE_ACCEPT(VarDeclStmt)
DEFINE_ACCEPT(AssignStmt)
DEFINE_ACCEPT(IfStmt)
DEFINE_ACCEPT(WhileStmt)
DEFINE_ACCEPT(ReturnStmt)
DEFINE_ACCEPT(ExprStmt)

// Declarations and program
DEFINE_ACCEPT(ClassDecl)
DEFINE_ACCEPT(FuncDecl)
DEFINE_ACCEPT(Program)

#undef DEFINE_ACCEPT
