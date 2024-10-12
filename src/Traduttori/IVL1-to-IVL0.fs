module ivl1-to-ivl0

open verification_ast

/// Translate an IVL1 unary operation to an IVL0 unary operation
let translateUnaryOp (op: UnOp) : UnOp =
    match op with
    | Neg -> Neg
    | Not -> Not

/// Translate an IVL1 binary operation to an IVL0 binary operation
let translateBinaryOp (op: BinOp) : BinOp =
    match op with
    | Add -> Add
    | Sub -> Sub
    | Mul -> Mul
    | Div -> Div
    | And -> And
    | Or -> Or
    | Eq -> Eq
    | Neq -> Neq
    | Lt -> Lt
    | Le -> Le
    | Gt -> Gt
    | Ge -> Ge

/// Translate an IVL1 expression to an IVL0 expression
let rec translateExprToIVL0 (expr: Expr) : Expr =
    match expr with
    | ConstInt i -> ConstInt i
    | ConstBool b -> ConstBool b
    | Var v -> Var v
    | BinOp (op, left, right) ->
        let leftTrans = translateExprToIVL0 left
        let rightTrans = translateExprToIVL0 right
        BinOp (translateBinaryOp op, leftTrans, rightTrans)
    | UnOp (op, expr) ->
        let transExpr = translateExprToIVL0 expr
        UnOp (translateUnaryOp op, transExpr)
    | MethodCall (name, args) ->
        // Simplify method calls by assuming inlining or handling with assertions
        let transArgs = args |> List.map translateExprToIVL0
        FuncCall (name, transArgs)  // Convert to FuncCall to simplify in IVL0
    | FuncCall (name, args) ->
        let transArgs = args |> List.map translateExprToIVL0
        FuncCall (name, transArgs)
    | If (cond, thenExpr, elseExpr) ->
        let transCond = translateExprToIVL0 cond
        let transThen = translateExprToIVL0 thenExpr
        let transElse = translateExprToIVL0 elseExpr
        If (transCond, transThen, transElse)
    | Match (e, cases) ->
        let transE = translateExprToIVL0 e
        let transCases = cases |> List.map (fun (pattern, body) ->
            let transPattern = translateExprToIVL0 pattern
            let transBody = translateExprToIVL0 body
            (transPattern, transBody))
        Match (transE, transCases)

/// Translate an IVL1 statement to an IVL0 statement
let rec translateStmtToIVL0 (stmt: Stmt) : Stmt =
    match stmt with
    | Assign (var, expr) ->
        let transExpr = translateExprToIVL0 expr
        Assign (var, transExpr)
    | Assert expr ->
        let transExpr = translateExprToIVL0 expr
        Assert transExpr
    | Assume expr ->
        let transExpr = translateExprToIVL0 expr
        Assume transExpr
    | Return expr ->
        let transExpr = translateExprToIVL0 expr
        Return transExpr
    | Break -> Break
    | Continue -> Continue
    | Seq stmts ->
        let transStmts = stmts |> List.map translateStmtToIVL0
        Seq transStmts
    | MethodCallStmt (name, args) ->
        let transArgs = args |> List.map translateExprToIVL0
        // Handle method calls in IVL0 as function calls or assertions
        FuncCall (name, transArgs) |> Assign ("temp_var", _)
    | Block stmts ->
        let transStmts = stmts |> List.map translateStmtToIVL0
        Block transStmts
    | Loop (invariants, body) ->
        // In IVL0, loops are transformed into sequences with invariants as assertions
        let transInvariants = invariants |> List.map translateExprToIVL0
        let transBody = translateStmtToIVL0 body
        Seq (transInvariants @ [transBody])
    | _ -> failwith "Unsupported statement type in IVL0"

/// Translate an IVL1 method into an IVL0 method
let translateMethodToIVL0 (method: IVL1) : IVL0 =
    match method with
    | Method (name, params, body) ->
        let transBody = translateStmtToIVL0 body
        Method (name, params, transBody)
    | GlobalVar (name, typ) -> GlobalVar (name, typ)

/// Translate an IVL1 top-level declaration into an IVL0 top-level declaration
let translateTopLevelToIVL0 (decl: IVL1) : IVL0 =
    match decl with
    | Method (name, params, body) -> translateMethodToIVL0 (Method (name, params, body))
    | GlobalVar (name, typ) -> GlobalVar (name, typ)
