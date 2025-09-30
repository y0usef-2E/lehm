# Features and Semantics:
- Statically-typed with type inference: something between Odin and Rust;
- try to handle SIGNED/UNSIGNED differently;
- compile-time hashmap;
- const is ::, var is ':=';
- later, consider immutable vars;
- structs, enums, descriminated unions, and proper pattern matching; 
- no class-based inheritance; 
- C-like types (vanilla unions, cstrings, ...); 
- easy interaction with foreign code (especially C);
- reasonable amount of control over boxing, allocations, and data layout;
- no GC (if you can get away with it);
- good metaprogramming/compile-time capabilities;
- return multiple values, return constant-sized array (on the stack);
- destructuring assignment;
- opt-in bounds-checking;
- errors as values, panic when needed;
- support for runtime reflection.

# Implementation Details: How You Want Things Done:
- compile to x64 (primarily); 
- afterwards, implement portable register-based VM.

# Syntax and Grammar (Specifics)
Program = Stmt* EOF;

Stmt = Decl | Expr-Stmt

Decl = Const-Decl | Var-Decl

Const-Decl = Identifier "::" Literal

Var-Decl = Identifier ":" (Identifier)? "=" Expr

Expr-Stmt = Expr ";"

Expr = Logical-Expr "if" Expr "," Expr | Logical-Expr

Logical-Expr = Binary-Expr Connective Logical-Expr | Binary-Expr

Binary-Expr = Unary-Expr Arithmetic-Op Binary-Expr | Unary-Expr

Unary-Expr = Unary-Op Unary-Expr | Primary-Expr

Primary-Expr = "(" Expr ")" | Literal | Identifier

Literal = StringLit | IntLit | CharLit

Identifier = ?