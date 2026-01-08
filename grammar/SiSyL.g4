//
// Copyright 2025 e-soul.org
// All rights reserved.
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
// 1. Redistributions of source code must retain the above copyright notice, this list of conditions
//    and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions
//    and the following disclaimer in the documentation and/or other materials provided with the distribution.
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR IMPLIED
// WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
// FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
// BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
// BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
// LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

// This file defines the grammar for the SiSyL language.  SiSyL is a small
// systems programming language with a Java‑like syntax, single ownership
// semantics and no garbage collector or borrow checker.  The grammar
// described here is intentionally simple, covering only:
// primitive types, classes, variables, assignments, free functions and basic control flow.

grammar SiSyL;

// We generate a C++ parser.  ANTLR will emit .cpp and .h files in
// the build directory.  See the top‑level CMakeLists.txt for details.
options {
    language=Cpp;
}

///////////////////////////////////////////////////////////////////////////////
// Parser rules
///////////////////////////////////////////////////////////////////////////////

// A program is zero or more class or function declarations followed by EOF.
program
    : (classDecl | funcDecl)* EOF
    ;

// Class declarations define a set of named fields.  For example:
//   class Point { Int64 x; Int64 y; }
classDecl
    : 'class' IDENT '{' classFields '}'
    ;

classFields
    : (type IDENT ';')*
    ;

// Function declarations consist of a return type, name, parameter list and body.
funcDecl
    : type IDENT '(' paramList? ')' block
    ;

// A comma‑separated list of parameters.  Each parameter has a type and name.
paramList
    : param (',' param)*
    ;

param
    : type IDENT
    ;

// A block of statements enclosed in braces.  Blocks introduce a new variable scope.
block
    : '{' statement* '}'
    ;

// Supported statements. An expression statement evaluates
// an expression and discards its value.  Block statements allow nested scopes.
statement
    : varDecl
    | assignStmt
    | ifStmt
    | whileStmt
    | returnStmt
    | exprStmt
    | blockStmt
    ;

// Block statement - allows nested scopes within function bodies.
blockStmt
    : block
    ;

// Variable declarations optionally include an initializer.  The type may be
// a primitive or user‑defined class.
varDecl
    : type IDENT ( '=' expression )? ';'
    ;

// Assignments assign the result of an expression to a variable or field.
assignStmt
    : location '=' expression ';'
    ;

// Standard if/else control flow.  The else branch is optional.
ifStmt
    : 'if' '(' expression ')' block ( 'else' block )?
    ;

// While loop.  The condition is evaluated before each iteration.
whileStmt
    : 'while' '(' expression ')' block
    ;

// Return statement optionally returns an expression.  A bare return
// in a void function returns nothing.
returnStmt
    : 'return' expression? ';'
    ;

// An expression followed by a semicolon.  Used for function calls
// or expressions whose value is ignored.
exprStmt
    : expression ';'
    ;

///////////////////////////////////////////////////////////////////////////////
// Expressions
///////////////////////////////////////////////////////////////////////////////

expression
    : logicalOrExpr
    ;

logicalOrExpr
    : logicalAndExpr ( '||' logicalAndExpr )*
    ;

logicalAndExpr
    : equalityExpr ( '&&' equalityExpr )*
    ;

equalityExpr
    : relationalExpr ( ('==' | '!=') relationalExpr )*
    ;

relationalExpr
    : addExpr ( ('<' | '>' | '<=' | '>=') addExpr )*
    ;

addExpr
    : mulExpr ( ('+' | '-') mulExpr )*
    ;

mulExpr
    : unaryExpr ( ('*' | '/') unaryExpr )*
    ;

unaryExpr
    : ('!' | '-') unaryExpr
    | primary
    ;

// Primary expressions.  These include literals, identifiers, function calls,
// object allocations and parenthesised subexpressions.
primary
    : INT_LIT
    | BOOL_LIT
    | STRING_LIT
    | IDENT '(' argList? ')'
    | location
    | 'new' IDENT '(' ')'
    | '(' expression ')'
    ;

// A comma‑separated list of arguments to a function call.
argList
    : expression (',' expression)*
    ;

// Locations refer to variables or fields of a class.  A dotted chain of
// identifiers accesses nested fields.
location
    : IDENT ( '.' IDENT )*
    ;

// Types available in SiSyL.  Primitive types are Int64, Bool and Str.  A user
// defined class type is referenced by its name.
type
    : 'Int64'
    | 'Bool'
    | 'Str'
    | IDENT
    ;

///////////////////////////////////////////////////////////////////////////////
// Lexer rules
///////////////////////////////////////////////////////////////////////////////

// Keywords.  We define them before IDENT so they are matched first.  ANTLR
// treats literal strings in the parser rules as tokens too, but explicit
// keyword tokens clarify intent and ease refactoring.
IF      : 'if';
ELSE    : 'else';
WHILE   : 'while';
RETURN  : 'return';
NEW     : 'new';
CLASS   : 'class';
// Built‑in type names
INT64_KW  : 'Int64';
BOOL_KW : 'Bool';
STR_KW  : 'Str';

// Identifiers: sequences of letters, digits and underscores that do not
// collide with keywords because keywords are defined above.
IDENT
    : [A-Za-z_] [A-Za-z_0-9]*
    ;

// Literals
INT_LIT
    : [0-9]+
    ;

BOOL_LIT
    : 'true'
    | 'false'
    ;

STRING_LIT
    : '"' (~["\r\n])* '"'
    ;

// Whitespace and comments are skipped.  We support C‑style single line
// comments starting with //.
WS
    : [ \t\r\n]+ -> skip
    ;

LINE_COMMENT
    : '//' ~[\r\n]* -> skip
    ;
