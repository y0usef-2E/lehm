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

Maps (/Arrays):
(1) Aability to override init, get, set functions (and therefore the allocation model, et cetera). But with the same syntax. (For each specific declaration.)

Does the init routine simply take a table of keys and values or an AST subtree?

(2) Allow as a top-level stmt:
'some_const :: ? const_map_expression ?' (which disables future uses of the set routine)

# Implementation Details: How You Want Things Done:
- compile to x64 (primarily); 
- afterwards, implement portable register-based VM.

# Syntax and Grammar (Specifics)
Program = Stmt* EOF;

Stmt = Decl | Expr-Stmt | Block | Return

Return = "return" Expr ";"

Decl = Const-Decl | Var-Decl

Const-Decl = Identifier "::" Const-Value ";"

Const-Value = Literal-Token | Array-Literal | Struct-Decl | Enum-Decl | Func-Decl

Var-Decl = Identifier ":" (Identifier)? "=" Expr ";"

Expr-Stmt = Expr ";"

Expr = Logical-Expr "if" Expr "," Expr | Assignment | Logical-Expr

Assignment = Identifier "=" Expr

Logical-Expr = Binary-Expr Connective Logical-Expr | Binary-Expr

Binary-Expr = Unary-Expr Arithmetic-Op Binary-Expr | Unary-Expr

Unary-Expr = Unary-Op Unary-Expr | Primary-Expr

Primary-Expr = "(" Expr ")" | Literal-Token | Identifier

Literal-Token = String-Literal | Int-Literal | Char-Literal

Identifier = ?