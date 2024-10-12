// includes all the necessary elements to represent the abstract syntax tree (AST) for intermediate verification 
// languages (IVL1, IVL0, and DSA), logical formulas, and SMT-LIB formulas. 
// This design will serve as the backbone for translating slang code into representations that can be verified 
// using the Z3 theorem prover

module verification_ast

/// Basic types used in verification languages
type Var = string      // Variable names are represented as strings
type Label = string    // Used for labeling specific parts (e.g., in DSA)
type MethodName = string  // Methods are identified by strings
type FuncName = string    // Functions are also identified by strings

/// Primitive types used in verification languages
type PrimType = 
    | IntType            // Integer type
    | BoolType           // Boolean type

/// Binary operations used in expressions and logical formulas
type BinOp =
    | Add                // +
    | Sub                // -
    | Mul                // *
    | Div                // /
    | And                // &&
    | Or                 // ||
    | Eq                 // ==
    | Neq                // !=
    | Lt                 // <
    | Le                 // <=
    | Gt                 // >
    | Ge                 // >=

/// Unary operations used in expressions and logical formulas
type UnOp =
    | Neg                // - (negation)
    | Not                // ! (logical negation)

/// Expressions in verification languages (IVL1, IVL0, DSA)
type Expr =
    | Var of Var                // Variable reference
    | ConstInt of int           // Integer constant
    | ConstBool of bool         // Boolean constant
    | BinOp of BinOp * Expr * Expr  // Binary operation (e.g., +, -, &&, ||)
    | UnOp of UnOp * Expr       // Unary operation (e.g., !, -)
    | MethodCall of MethodName * Expr list  // Call to a method with arguments
    | FuncCall of FuncName * Expr list      // Call to a function with arguments
    | If of Expr * Expr * Expr  // If-then-else expression
    | Match of Expr * (Expr * Expr) list  // Match expressions with cases

/// Statements in verification languages
type Stmt =
    | Assign of Var * Expr       // Assignment: var = expr
    | Assume of Expr             // Assume condition
    | Assert of Expr             // Assert condition
    | Return of Expr             // Return statement
    | Seq of Stmt list           // Sequence of statements
    | Loop of Expr * Stmt        // Loop with invariant (expr is the invariant)
    | Break                      // Break statement (for loop exit)
    | Continue                   // Continue statement (for skipping loop iteration)
    | MethodCallStmt of MethodName * Expr list  // Method call as a statement
    | Block of Stmt list         // Block of statements (scoped)

/// Intermediate Verification Language Level 1 (IVL1)
/// Represents the higher-level AST for the *slang* language.
type IVL1 = 
    | Method of MethodName * Var list * Stmt  // Method with parameters and body
    | GlobalVar of Var * PrimType             // Global variable declaration

/// Intermediate Verification Language Level 0 (IVL0)
/// Represents a lower-level form, closer to verification conditions.
type IVL0 = 
    | Method of MethodName * Var list * Stmt  // Method in IVL0 form
    | GlobalVar of Var * PrimType             // Global variable declaration

/// Dynamic Single Assignment (DSA) Form
/// DSA ensures that every variable is assigned exactly once.
type DSA = 
    | DsaMethod of MethodName * (Var * PrimType) list * Stmt  // Method in DSA form

/// Logical formulas for verification conditions (used with Z3)
type LogicalExpr =
    | LVar of Var                   // Logical variable
    | LConstInt of int              // Integer constant in logic
    | LConstBool of bool            // Boolean constant in logic
    | LBinOp of BinOp * LogicalExpr * LogicalExpr  // Binary operation in logic
    | LUnOp of UnOp * LogicalExpr   // Unary operation in logic
    | LForall of Var * LogicalExpr  // Universal quantifier: forall x :: P(x)
    | LExists of Var * LogicalExpr  // Existential quantifier: exists x :: P(x)
    | LImplies of LogicalExpr * LogicalExpr // Logical implication
    | LAnd of LogicalExpr * LogicalExpr  // Logical conjunction
    | LOr of LogicalExpr * LogicalExpr   // Logical disjunction
    | LNot of LogicalExpr                // Logical negation

/// SMT-LIB formulas used to interact with Z3
type SMTLibFormula = 
    | DeclareVar of Var * PrimType      // Declare a variable with its type
    | AssertFormula of LogicalExpr      // Assert a logical formula
    | CheckSat                         // SMT-LIB check for satisfiability
    | Push                             // Push current state onto SMT solver stack
    | Pop                              // Pop state from SMT solver stack

/// Helper function for formatting expressions
let rec formatExpr (expr: Expr) =
    match expr with
    | Var v -> v
    | ConstInt i -> string i
    | ConstBool b -> if b then "true" else "false"
    | BinOp (op, left, right) -> 
        let opStr = match op with
                    | Add -> "+"
                    | Sub -> "-"
                    | Mul -> "*"
                    | Div -> "/"
                    | And -> "&&"
                    | Or -> "||"
                    | Eq -> "=="
                    | Neq -> "!="
                    | Lt -> "<"
                    | Le -> "<="
                    | Gt -> ">"
                    | Ge -> ">="
        sprintf "(%s %s %s)" (formatExpr left) opStr (formatExpr right)
    | UnOp (op, e) -> 
        let opStr = match op with
                    | Neg -> "-"
                    | Not -> "!"
        sprintf "(%s %s)" opStr (formatExpr e)
    | MethodCall (name, args) -> 
        let argsStr = args |> List.map formatExpr |> String.concat ", "
        sprintf "%s(%s)" name argsStr
    | FuncCall (name, args) ->
        let argsStr = args |> List.map formatExpr |> String.concat ", "
        sprintf "%s(%s)" name argsStr
    | If (cond, thenExpr, elseExpr) ->
        sprintf "if (%s) then (%s) else (%s)" (formatExpr cond) (formatExpr thenExpr) (formatExpr elseExpr)
    | Match (e, cases) ->
        let casesStr = 
            cases 
            |> List.map (fun (pattern, body) -> sprintf "%s => %s" (formatExpr pattern) (formatExpr body))
            |> String.concat ", "
        sprintf "match %s { %s }" (formatExpr e) casesStr

/// Helper function for formatting logical expressions 
let rec formatLogicalExpr (expr: LogicalExpr) =
    match expr with
    | LVar v -> v
    | LConstInt i -> string i
    | LConstBool b -> if b then "true" else "false"
    | LBinOp (op, left, right) -> 
        let opStr = match op with
                    | Add -> "+"
                    | Sub -> "-"
                    | Mul -> "*"
                    | Div -> "/"
                    | And -> "&&"
                    | Or -> "||"
                    | Eq -> "=="
                    | Neq -> "!="
                    | Lt -> "<"
                    | Le -> "<="
                    | Gt -> ">"
                    | Ge -> ">="
        sprintf "(%s %s %s)" (formatLogicalExpr left) opStr (formatLogicalExpr right)
    | LUnOp (op, e) -> 
        let opStr = match op with
                    | Neg -> "-"
                    | Not -> "!"
        sprintf "(%s %s)" opStr (formatLogicalExpr e)
    | LForall (v, body) -> sprintf "forall %s :: %s" v (formatLogicalExpr body)
    | LExists (v, body) -> sprintf "exists %s :: %s" v (formatLogicalExpr body)
    | LImplies (l, r) -> sprintf "(%s ==> %s)" (formatLogicalExpr l) (formatLogicalExpr r)
    | LAnd (l, r) -> sprintf "(%s && %s)" (formatLogicalExpr l) (formatLogicalExpr r)
    | LOr (l, r) -> sprintf "(%s || %s)" (formatLogicalExpr l) (formatLogicalExpr r)
    | LNot e -> sprintf "!(%s)" (formatLogicalExpr e)
