// ricorda: Slang Source Code → IVL1 (higher-level IVL) → IVL0 (lower-level IVL) → DSA → SMT-LIB

module slang-to-IVL1

open slang_ast
open verification_ast

/// Translate a Slang unary operation to an IVL1 unary operation
let translateUnaryOp (op: SlangAst.UnaryOp) : VerificationAst.UnOp =
    match op with
    | SlangAst.Negate -> VerificationAst.Not
    | SlangAst.Minus -> VerificationAst.Neg

/// Translate a Slang binary operation to an IVL1 binary operation
let translateBinaryOp (op: SlangAst.BinaryOp) : VerificationAst.BinOp =
    match op with
    | SlangAst.Add -> VerificationAst.Add
    | SlangAst.Sub -> VerificationAst.Sub
    | SlangAst.Mul -> VerificationAst.Mul
    | SlangAst.Div -> VerificationAst.Div
    | SlangAst.Mod -> VerificationAst.Mod
    | SlangAst.And -> VerificationAst.And
    | SlangAst.Or -> VerificationAst.Or
    | SlangAst.Less -> VerificationAst.Lt
    | SlangAst.LessEqual -> VerificationAst.Le
    | SlangAst.Greater -> VerificationAst.Gt
    | SlangAst.GreaterEqual -> VerificationAst.Ge
    | SlangAst.Equal -> VerificationAst.Eq
    | SlangAst.NotEqual -> VerificationAst.Neq
    | SlangAst.Implies -> VerificationAst.Eq  // IVL1 handles implication differently
    | SlangAst.BitShiftLeft -> VerificationAst.BinOp (VerificationAst.Add, _, _)  // Need custom handling in IVL1
    | SlangAst.BitShiftRight -> VerificationAst.BinOp (VerificationAst.Sub, _, _) // Need custom handling

/// Translate a Slang expression into an IVL1 expression
let rec translateExpr (slangExpr: SlangAst.Expr) : VerificationAst.Expr =
    match slangExpr with
    | SlangAst.BoolLiteral b -> VerificationAst.ConstBool b
    | SlangAst.IntLiteral i -> VerificationAst.ConstInt i
    | SlangAst.Var v -> VerificationAst.Var v
    | SlangAst.UnaryOpExpr (op, expr) ->
        let translatedExpr = translateExpr expr
        VerificationAst.UnOp (translateUnaryOp op, translatedExpr)
    | SlangAst.BinaryOpExpr (op, left, right) ->
        let translatedLeft = translateExpr left
        let translatedRight = translateExpr right
        VerificationAst.BinOp (translateBinaryOp op, translatedLeft, translatedRight)
    | SlangAst.FunctionCall (name, args) ->
        let translatedArgs = args |> List.map translateExpr
        VerificationAst.FuncCall (name, translatedArgs)
    | SlangAst.Quantifier (quantifier, var, body) ->
        let translatedBody = translateExpr body
        match quantifier with
        | "forall" -> VerificationAst.LForall (var, translatedBody)
        | "exists" -> VerificationAst.LExists (var, translatedBody)
        | _ -> failwith "Unsupported quantifier in IVL1"
    | SlangAst.Old expr ->
        let translatedExpr = translateExpr expr
        VerificationAst.UnOp (VerificationAst.Neg, translatedExpr) // Example of IVL1 handling of 'old'
    | SlangAst.Broke -> VerificationAst.Var "broke"  // Broke will be represented as a variable in IVL1
    | SlangAst.Result -> VerificationAst.Var "result" // Same for result

/// Translate a Slang statement into an IVL1 statement
let rec translateStmt (slangStmt: SlangAst.Stmt) : VerificationAst.Stmt =
    match slangStmt with
    | SlangAst.VarDecl (var, typ, Some expr) ->
        let transExpr = translateExpr expr
        VerificationAst.Assign (var, transExpr)
    | SlangAst.VarDecl (var, typ, None) ->
        VerificationAst.Assign (var, VerificationAst.ConstInt 0)  // Default initialization
    | SlangAst.Assign (var, expr) -> 
        let transExpr = translateExpr expr
        VerificationAst.Assign (var, transExpr)
    | SlangAst.Assume expr ->
        let transExpr = translateExpr expr
        VerificationAst.Assume transExpr
    | SlangAst.Assert expr ->
        let transExpr = translateExpr expr
        VerificationAst.Assert transExpr
    | SlangAst.Return (Some expr) ->
        let transExpr = translateExpr expr
        VerificationAst.Return transExpr
    | SlangAst.Return None -> VerificationAst.Return (VerificationAst.ConstInt 0) // Default return value
    | SlangAst.Break -> VerificationAst.Break
    | SlangAst.Continue -> VerificationAst.Continue
    | SlangAst.MethodCall (name, args) ->
        let transArgs = args |> List.map translateExpr
        VerificationAst.MethodCallStmt (name, transArgs)
    | SlangAst.Match cases ->
        let transCases = cases |> List.map (fun (cond, stmt) ->
            let transCond = translateExpr cond
            let transStmt = translateStmt stmt
            (transCond, transStmt))
        VerificationAst.Match (Var "match_expr", transCases) // IVL1 simplification of match
    | SlangAst.Loop (invariants, conditions, body) ->
        let transInvariants = invariants |> List.map translateExpr
        let transBody = translateStmt body
        VerificationAst.Loop (VerificationAst.Seq transInvariants, transBody)
    | SlangAst.For (var, startExpr, endExpr, body) ->
        let transStart = translateExpr startExpr
        let transEnd = translateExpr endExpr
        let transBody = translateStmt body
        VerificationAst.Loop (VerificationAst.Seq [VerificationAst.Assign (var, transStart)], transBody)
    | _ -> failwith "Unsupported statement type in IVL1"

/// Translate a Slang method into an IVL1 method
let translateMethod (method: SlangAst.Method) : VerificationAst.IVL1 =
    let paramList = method.Parameters |> List.map fst
    let body = method.Body |> Option.map translateStmt |> Option.defaultValue (VerificationAst.Block [])
    VerificationAst.Method (method.Name, paramList, body)

/// Translate a Slang function into an IVL1 function
let translateFunction (func: SlangAst.Function) : VerificationAst.IVL1 =
    let paramList = func.Parameters |> List.map fst
    let body = translateExpr func.Body
    VerificationAst.Method (func.Name, paramList, VerificationAst.Return body)

/// Translate a top-level Slang declaration into an IVL1 declaration
let translateTopLevel (decl: SlangAst.TopLevel) : VerificationAst.IVL1 =
    match decl with
    | SlangAst.Global (name, typ) -> VerificationAst.GlobalVar (name, VerificationAst.IntType) // Simplified as int type for IVL1
    | SlangAst.Method method -> translateMethod method
    | SlangAst.Function func -> translateFunction func
    | SlangAst.Domain domain -> failwith "Domain translation not supported in IVL1" // Domain translation would require special handling
