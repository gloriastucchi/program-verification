module SlangAst

// The primary role of SlangAst.fs is to represent the code that is parsed from slang source files. 
// The parser will use this structure to create an internal representation of the program.

// Types for representing the data types in Slang
type Type =
    | BoolType
    | IntType
    | CustomType of string

// Variables are identifiers in Slang, represented as strings
type Variable = string

// Unary operators (e.g., negation)
type UnaryOp =
    | Negate  // For boolean negation: !
    | Minus   // For arithmetic negation: -

// Binary operators (e.g., arithmetic, comparison, logical)
type BinaryOp =
    | Add     // +
    | Sub     // -
    | Mul     // *
    | Div     // /
    | Mod     // %
    | And     // &&
    | Or      // ||
    | Less    // <
    | LessEqual   // <=
    | Greater     // >
    | GreaterEqual // >=
    | Equal       // ==
    | NotEqual    // !=
    | Implies     // ==>
    | BitShiftLeft  // <<
    | BitShiftRight // >>

// Expressions represent values, variables, and operations
type Expr =
    | BoolLiteral of bool
    | IntLiteral of int
    | Var of Variable                // Variable reference
    | UnaryOpExpr of UnaryOp * Expr  // Unary operation
    | BinaryOpExpr of BinaryOp * Expr * Expr // Binary operation
    | FunctionCall of Variable * Expr list   // Function call with arguments
    | Quantifier of string * Variable * Expr // forall or exists quantifiers
    | Old of Expr                     // Used for 'old' expression in postconditions
    | Broke                           // Represents 'broke' in loops
    | Result                          // Represents 'result' in postconditions

// Statements represent actions or commands in Slang
type Stmt =
    | VarDecl of Variable * Type * Expr option // Variable declaration with optional initialization
    | Assign of Variable * Expr               // Variable assignment
    | Assume of Expr                          // Assume statement
    | Assert of Expr                          // Assert statement
    | Return of Expr option                   // Return statement
    | Break                                   // Break loop
    | Continue                                // Continue loop
    | MethodCall of Variable * Expr list      // Method call
    | Match of (Expr * Stmt) list             // Match statement with branches
    | Loop of Expr list * Expr list * Stmt    // Loop with invariants and body
    | For of Variable * Expr * Expr * Stmt    // For loop with index variable and bounds

// Specifications for methods (requires, ensures, invariants, etc.)
type Specification =
    | Requires of Expr
    | Ensures of Expr
    | Modifies of Variable list
    | Invariant of Expr
    | Decreases of Expr

// A method in Slang
type Method = {
    Name: Variable
    Parameters: (Variable * Type) list
    ReturnType: Type option
    Specs: Specification list
    Body: Stmt option
}

// A function in Slang (only allows requires and ensures specs)
type Function = {
    Name: Variable
    Parameters: (Variable * Type) list
    ReturnType: Type
    Specs: Specification list
    Body: Expr
}

// A domain in Slang (with functions and axioms)
type DomainItem =
    | Function of Function
    | Axiom of Expr

type Domain = {
    Name: Variable
    Items: DomainItem list
}

// Top-level declarations in Slang (methods, functions, domains, and globals)
type TopLevel =
    | Global of Variable * Type
    | Method of Method
    | Function of Function
    | Domain of Domain

// A complete Slang file (list of top-level items)
type File = TopLevel list
