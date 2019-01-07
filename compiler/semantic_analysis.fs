///
/// Perform semantic analysis on Ava program
///

module SemanticAnalysis
open System.Linq.Expressions

#nowarn "40" // Turn off waring for recursive objects

open Ast
open System.Collections.Generic

type SymbolScope(parent: SymbolScope option) =
    let mutable list = List.empty<VariableDecl>

    let ident_from_decl =
        function
        | ScalarDecl(_, id, _, _)
        | ArrayDecl(_, id, _, _, _)
        | ProcedureDecl(_, id, _, _, _) -> id

    /// Check if declaration has an identifier
    let declares_ident (id_ref: IdentifierRef) decl =
        (ident_from_decl decl) = id_ref.Identifier

    member x.AddDeclaration decl =
        let id = ident_from_decl

        if List.exists (fun x -> id x = id decl) list then
            printfn "Variable %s already defined" (id decl)
            // @Todo: better error
        list <- decl :: list

    member x.FindDeclaration id_ref =
        let found = List.tryFind (fun x -> declares_ident id_ref x) list

        match found with
        | Some(decl) -> Some(decl)
        | None ->
            // Check for declaration in parent scope
            match parent with
            | Some(ss) -> ss.FindDeclaration id_ref
            | None -> None

    type SymbolScopeStack() =
        let stack = new Stack<SymbolScope>()
        do stack.Push(new SymbolScope(None))

        member x.CurrentScope = stack.Peek()

        /// Push new scope and set its parent to the current scope
        member x.Push() = stack.Push(new SymbolScope(Some(stack.Peek())))

        member x.Pop() = stack.Pop()

        /// Add declaration to current scope
        member x.AddDeclaration decl = stack.Peek().AddDeclaration decl

type VariableType =
    {
        Type    : TypeSpec;
        IsArray : bool;
    }
    override x.ToString() = (if x.IsArray then "[] of " else "") + x.Type.ToString()

let scalar_type t = { Type = t; IsArray = false }

let typeof_decl =
    function
    // @Todo: None types should be an error at this point
    | Ast.ScalarDecl(_, _, Some(t), _) ->  { Type = t; IsArray = false }
    | Ast.ScalarDecl(_, _, None, _) -> { Type = Void; IsArray = false }
    | Ast.ArrayDecl(_, _, _, Some(t), _) -> { Type = t; IsArray = true }
    | Ast.ArrayDecl(_, _, _, None, _) -> { Type = Void; IsArray = true }
    // @Todo: handle procedure declarations properly
    | Ast.ProcedureDecl(_, _, _, t, _) -> { Type = t; IsArray = false }

let eval_static_array_size expr =
    match expr with
    | LiteralExpression(l) ->
        match l with
        | IntLiteral(i) -> Some(i)
        | _ -> None
    | _ -> None

let int_to_expr value =
    LiteralExpression(IntLiteral value)

type ProcedureTableEntry =
    {
        ReturnType     : TypeSpec;
        ParameterTypes : VariableType list;
    }

type ProcedureTable(program: Program) as self =
    inherit Dictionary<Identifier, ProcedureTableEntry>()

    let rec scan_decl decl =
        match decl with
        | VariableDecl(d) ->
            match d with
            | ScalarDecl(_)
            | ArrayDecl(_) ->
                ()
            | ProcedureDecl(_, id, p, t, _) ->
                if self.ContainsKey id then
                    // @Todo: Error
                    printfn "Procedure %s already defined" id
                self.Add(id, { ReturnType = t; ParameterTypes = List.map typeof_decl p; })
        | DeclNop -> ()

    do
        // Register builtin procedures
        self.Add("print",{ ReturnType = Void; ParameterTypes =
            [ { Type = String; IsArray = false } ] })

        // Scan entire ast for top level procedure declarations
        program |> List.iter scan_decl


type SymbolTable(program: Declaration list) as self =
    inherit Dictionary<IdentifierRef, VariableDecl>(HashIdentity.Reference)

    // Keep track of nested loops
    let loop_stmt_stack = Stack<WhileStatement>()
    let scope_stack = new SymbolScopeStack()

    let rec scan_decl decl =
        match decl with
        | VariableDecl(d) ->
            match d with
            | ScalarDecl(_)
            | ArrayDecl(_) ->
                scope_stack.AddDeclaration d
            | ProcedureDecl(_,a, b, c, e) ->
                // Procedure gets added to global scope
                scope_stack.AddDeclaration d
                map_identifier { Identifier = a }
                scan_proc_decl(a, b, c, e)
        | DeclNop -> ()

    /// Map identifier to its declaration
    and map_identifier (id_ref: IdentifierRef) =
        let decl = scope_stack.CurrentScope.FindDeclaration id_ref

        match decl with
        | Some(d) -> self.Add(id_ref, d)
        | None -> printfn "Undeclared identifier '%s'" id_ref.Identifier

    and scan_proc_decl (_, parameters, return_type, block) =
        let rec scan_block (statements) =
            scope_stack.Push()
            statements |> List.iter scan_statement
            scope_stack.Pop() |> ignore

        and scan_statement =
            function
            | Declaration(decl) ->
                match decl with
                | VariableDecl(d) ->
                    scope_stack.AddDeclaration d
                    match d with
                    // @Todo: Scan array expression for size
                    | ScalarDecl(_, id, _, None) -> map_identifier { Identifier = id }
                    | ScalarDecl(_, id, _, Some(e)) ->
                        scan_expression e
                        map_identifier { Identifier = id }
                    | ArrayDecl(_, id, _, _, _)  -> map_identifier { Identifier = id }
                    | _ -> ()
                | DeclNop -> ()
            | SExpression(sexpr) ->
                match sexpr with
                | Expression(e) -> scan_expression e
                | Nop -> ()
            | CompoundStatement(cs) -> scan_block cs
            | IfStatement(clauses, Some(statements)) ->
                clauses |> List.iter scan_cond_clause
                statements |> List.iter scan_statement
            | IfStatement(clauses, None) ->
                clauses |> List.iter scan_cond_clause
            | WhileStatement(expr, stmt) ->
                loop_stmt_stack.Push(expr, stmt)
                scan_expression expr
                scan_statement stmt
                loop_stmt_stack.Pop() |> ignore
            | ReturnStatement(Some(expr)) -> scan_expression expr
            | ReturnStatement(None) ->
                if return_type <> Void then
                    // @Todo: error
                    printfn "TypeError: expected 'void', got %s." (return_type.ToString())
            | BreakStatement ->
                if loop_stmt_stack.Count = 0 then
                    // @Todo: error
                    printfn "Invalid keyword 'break' outside of loop."
            | ContinueStatement ->
                if loop_stmt_stack.Count = 0 then
                    // @Todo: error
                    printfn "Invalid keyword 'continue' outside of loop."

        and scan_expression =
            function
            | ScalarAssignExpression(id, e) ->
                map_identifier id
                scan_expression e
            | ArrayAssignExpression(id, e1, e2) ->
                map_identifier id
                scan_expression e1
                scan_expression e2
            | BinaryExpression(lhs, _, rhs) ->
                scan_expression lhs
                scan_expression rhs
            | UnaryExpression(_, e) ->
                scan_expression e
            | IdentifierExpression(id) ->
                map_identifier id
            | ArrayIdentifierExpression(id, e) ->
                map_identifier id
                scan_expression e
            | ProcedureCallExpression(_, args) ->
                // @Todo map procedure identifier
                args |> List.iter scan_expression
            | ArrayCountExpression(id) ->
                map_identifier id
            | LiteralExpression(x) -> ()

        and scan_cond_clause(expr, statements) =
            scan_expression expr
            statements |> List.iter scan_statement

        scope_stack.Push() // Push procedure scope
        parameters |> List.iter scope_stack.AddDeclaration
        scan_block block
        scope_stack.Pop() |> ignore // End procedure scope

    // Scan entire ast
    do program |> List.iter scan_decl

    member x.GetIdentifierType id_ref =
        if self.ContainsKey(id_ref) then
            typeof_decl self.[id_ref]
        else
            // Undefinied identifier, return void to attempt error recovery
            // and continue checking for more errors
            scalar_type Void

type ExpressionTable(program, proc_table: ProcedureTable, symbol_table: SymbolTable) as self =
    inherit Dictionary<Expression, VariableType>(HashIdentity.Reference)

    let rec is_const_expression expr =
        match expr with
        | LiteralExpression(l) -> true
        | BinaryExpression(lhs, _, rhs) ->
            is_const_expression lhs && is_const_expression rhs
        | UnaryExpression(_, e) -> is_const_expression e
        | _ -> false

    let rec scan_decl decl =
        match decl with
        | VariableDecl(d) ->
            match d with
            | ProcedureDecl(_, a, b, c, d) -> scan_proc_decl(a, b, c, d)
            | _ -> () // @Todo: Handle global expressions
        | DeclNop -> ()

    and resolve_implicit_types =
        let mutable refs  = []
        let mutable types = []

        let resolve_duplicate id_ref =
             // id_refs are created everytime a symbol is used so we update the type
             // of all references without running 'scan_expression' for each one
             let duplicate_decl = types.[List.findIndex ((=) id_ref) refs]
             types <- duplicate_decl :: types

        let infer_scalar_type id_ref e =
            if not (List.contains id_ref refs) then
                let implicit_type = scan_expression e
                Some implicit_type
            else
                resolve_duplicate id_ref
                None

        let infer_array_type id_ref a =
            if not (List.contains id_ref refs) then
                let mutable implicit_type = scalar_type Void
                for e in a do
                    let expr_type = scan_expression e

                    if implicit_type.Type <> Void && expr_type <> implicit_type then
                        // Assert all elements in the array are of the same type
                        printfn "TypeError: expected %A, got %A" implicit_type.Type expr_type.Type

                    implicit_type <- expr_type
                Some implicit_type
            else
                resolve_duplicate id_ref
                None

        for symbol in symbol_table do
            let id_ref = symbol.Key
            let decl = symbol.Value

            match decl with
            | ScalarDecl(flags, id, None, Some(e)) ->
                if flags.HasFlag ScalarFlags.CONSTANT && not (is_const_expression e) then
                    printfn "Error: Expected constant value for '%s'" id

                match infer_scalar_type id_ref e with
                | Some(i) ->
                    types <- ScalarDecl(flags, id, Some(i.Type), Some(e)) :: types
                | None -> ()
                refs <- id_ref :: refs
            | ArrayDecl(flags, id, decl_size, array_type, Some(a)) ->
                // Arrays initialized to an aggregate are implicitly marked as fixed
                let new_flags = (flags &&& ~~~ArrayFlags.NONE) ||| ArrayFlags.FIXED

                let sz =
                    match decl_size with
                    | Some(e) ->
                        // Array declared with size must be a fixed array
                        match eval_static_array_size e with
                            | Some(s) ->
                                if s <> a.Length then
                                    // @Todo: Clearer message once error system is implememted
                                    printfn "Error: %s declared with size %i will have size %i." id s a.Length
                            | None ->
                                printfn "Error: A constant value is expected %s." id
                        a.Length
                    | None ->
                        a.Length

                let size_expr = Some(int_to_expr(sz))

                match infer_array_type id_ref a with
                | Some(impl) ->
                    match array_type with
                    | Some(t) ->
                        if impl.Type <> t then
                            // Declared array type does not match aggregate type
                            printfn "TypeError: expected %s, got %s" (t.ToString()) (impl.Type.ToString())
                        types <- ArrayDecl(new_flags, id, size_expr, Some(t), Some(a)) :: types
                    | None ->
                        types <- ArrayDecl(new_flags, id, size_expr, Some(impl.Type), Some(a)) :: types
                | None -> ()
                refs <- id_ref :: refs
            | _ -> ()

        List.iter2 (fun r t -> symbol_table.[r] <- t) refs types

    and scan_proc_decl(_, _, return_type, block) =
        let rec scan_block(statements) = statements |> List.iter scan_statement

        and scan_statement =
            function
            | SExpression(sexpr) ->
                match sexpr with
                | Expression(e) -> scan_expression e |> ignore
                | Nop -> ()
            | CompoundStatement(cs) -> scan_block cs
            | IfStatement(clauses, Some(statements)) ->
                clauses |> List.iter scan_cond_clause
                statements |> List.iter scan_statement
            | IfStatement(clauses, None) ->
                clauses |> List.iter scan_cond_clause
            | WhileStatement(expr, stmt) ->
                scan_expression expr |> ignore
                scan_statement stmt
            | ReturnStatement(Some(expr)) ->
                let expr_type = scan_expression expr

                if expr_type <> scalar_type return_type then
                    // @Todo: error
                    printfn "TypeError: expected %s, got %s" (return_type.ToString()) (expr_type.ToString())
            | _ -> ()

        and scan_cond_clause(expr, statements) =
            scan_expression expr |> ignore
            statements |> List.iter scan_statement

        scan_block block

    and scan_expression expr =
        let check_index_type e =
            let index_type = scan_expression e
            // Array indexes must be integers
            if index_type <> scalar_type Int then
                // @Todo: error
                printfn "TypeError: expected %s, got %s" (Int.ToString()) (index_type.ToString())

        /// Interpret the type of expr
        let expression_type =
            match expr with
            | ScalarAssignExpression(id, e) ->
                let typeof_e  = scan_expression e
                let typeof_id = symbol_table.GetIdentifierType id

                if typeof_e <> typeof_id then
                    // @Todo: error
                    printfn "TypeError: expected %s, got %s" (typeof_id.ToString()) (typeof_e.ToString())

                match symbol_table.[id] with
                | ScalarDecl(flags, _, _, _) ->
                    if flags.HasFlag(ScalarFlags.CONSTANT) then
                        printfn "Error: Cannot reassign constant '%s'." id.Identifier
                | ArrayDecl(_, id, _, _, _) ->
                    printfn "Error: Cannot reassign array '%s'" id // @Temporary
                | ProcedureDecl(_, id, _, _, _) ->
                    printfn "Error: Cannot reassign procedure '%s'" id

                typeof_id
            | ArrayAssignExpression(id, e1, e2) ->
                check_index_type e1

                let typeof_e2 = scan_expression e2
                let typeof_id = symbol_table.GetIdentifierType id

                if not typeof_id.IsArray then
                    // @Todo: error
                    printfn "TypeError: Cannot apply indexing with '[]' to value of type %s" (typeof_id.ToString())

                // @Todo: Multidementional arrays

                if typeof_e2 <> typeof_id then
                    // @Todo: error
                    printfn "TypeError: expected %s, got %s" (typeof_id.ToString()) (typeof_e2.ToString())

                scalar_type typeof_id.Type
            | BinaryExpression(lhs, op, rhs) ->
                let typeof_lhs = scan_expression lhs
                let typeof_rhs = scan_expression rhs

                match op with
                | CondOr | CondAnd | Xor->
                    scalar_type Int // Conditional expressions resolve to int
                | Eq | NotEq ->
                    match typeof_lhs, typeof_rhs with
                    // @Todo: Fixed
                    | { Type = a; IsArray = false }, { Type = b; IsArray = false } when a = b -> ()
                    | _ -> printfn "TypeError %s cannot be applied to %s and %s" (op.ToString()) (typeof_lhs.ToString()) (typeof_rhs.ToString())
                    scalar_type Int
                | LtEq | Lt | GtEq | Gt ->
                    match typeof_lhs, typeof_rhs with
                    | { Type = Int; IsArray = false }, { Type = Int; IsArray = false }
                    | { Type = Float; IsArray = false }, { Type = Float; IsArray = false } ->
                        ()
                    | _ -> printfn "TypeError %s cannot be applied to %s and %s" (op.ToString()) (typeof_lhs.ToString()) (typeof_rhs.ToString())
                    scalar_type Int
                | Add | Sub | Mul | Div | Mod ->
                    typeof_lhs // @Temporary: casting
            | UnaryExpression(_, e) -> scan_expression e
            | IdentifierExpression(id) -> symbol_table.GetIdentifierType id
            | ArrayIdentifierExpression(id, e) ->
                let typeof_id = symbol_table.GetIdentifierType id

                if not typeof_id.IsArray then
                    // @Todo: error
                    printfn "TypeError: Cannot apply indexing with '[]' to value of type %s" (typeof_id.ToString())

                check_index_type e
                scalar_type (symbol_table.GetIdentifierType id).Type
            | ProcedureCallExpression(id, args) ->
                if not (proc_table.ContainsKey id.Identifier) then
                    // @Todo: error

                    if symbol_table.ContainsKey id then
                        // @Todo
                        printfn "'%s' is not a procedure and thus cannot be called" id.Identifier
                    else
                        printfn "Undeclared procedure: %s" (id.Identifier)

                if proc_table.ContainsKey(id.Identifier) then
                    let proc = proc_table.[id.Identifier]

                    let param_types = proc.ParameterTypes

                    if List.length args <> List.length param_types then
                        printfn "Wrong number of arguments for %s: expected %i got %i." (id.Identifier) (List.length param_types) (List.length args)

                    let arg_types = args |> List.map scan_expression

                    let param_types_match index a b =
                        if a <> b && b <> (scalar_type Void) then
                            printfn "Invalid argument for %s: expected %s got %s" (id.Identifier) (b.ToString()) (a.ToString())

                    List.iteri2 param_types_match arg_types param_types

                    scalar_type proc.ReturnType
                else
                    // If procedure is undifined return void to attempt error recovery
                    scalar_type Void
            | ArrayCountExpression(id) -> scalar_type Int
            | LiteralExpression(l) ->
                match l with
                | IntLiteral(i) -> scalar_type Int
                | FloatLiteral(f) -> scalar_type Float
                | ByteLiteral(b) -> scalar_type Byte
                | TextLiteral(t) -> scalar_type Text
                | StringLiteral(s) -> scalar_type String
                | CharLiteral(c) -> scalar_type Char

        self.Add(expr, expression_type)
        expression_type

    do resolve_implicit_types
    do program |> List.iter scan_decl

type SemanticAnalysisResult =
    {
        SymbolTable     : SymbolTable;
        ProcedureTable  : ProcedureTable;
        ExpressionTable : ExpressionTable
    }

///
/// Perform semantic analysis on program AST
///
let analyse program =
    let symbol_table = new SymbolTable(program)
    let proc_table   = new ProcedureTable(program)

    if not (proc_table.ContainsKey "main") then
        // @Todo: define entry points with keyword
        printfn "Missing program entry point."

    let expr_table = new ExpressionTable(program, proc_table, symbol_table)

    {
        SymbolTable     = symbol_table;
        ProcedureTable  = proc_table;
        ExpressionTable = expr_table;
    }

let debug_symbol_search analysis_result (search_string: string) =
    let mutable symbols = []

    for kv in analysis_result.SymbolTable do
        if kv.Key.Identifier = search_string then
            symbols <- kv.Value :: symbols

    symbols
