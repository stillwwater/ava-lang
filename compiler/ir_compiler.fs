module IRCompiler

open Ast
open System.Collections.Generic
open SemanticAnalysis

type IR =
    {
        Globals    : GlobalSymbol list
        Procedures : Proc list
    }

and Proc =
    {
        Name       : string
        ReturnType : TypeSpec
        Parameters : Parameters
        Body       : Instruction list
        Locals     : int
        EndLabel   : Label option
        IsExtern   : bool
    }

and Symbol =
    {
        Name : IdentifierRef
        Type : VariableType
    }

and GlobalSymbol =
    {
        Data   : Asm.Data
        Symbol : Symbol option
    }

and Instruction =
    | Lab of Label
    | LoadRef of Operand * Operand
    | StoreRef of Operand * Operand
    | ArrayLoad of Operand * Operand * Operand // symbol := array[index]
    | ArrayStore of Operand * Operand * Operand // array[index] := symbol
    | Goto of Label
    | Copy of Operand * Operand
    | Add of Operand * Operand * Operand
    | Sub of Operand * Operand * Operand
    | Mul of Operand * Operand * Operand
    | Div of Operand * Operand * Operand
    | Mod of Operand * Operand * Operand
    | BitOr  of Operand * Operand * Operand
    | BitXor of Operand * Operand * Operand
    | BitAnd of Operand * Operand * Operand
    | BitNot of Operand * Operand
    | Neg of Operand * Operand
    | Ceq of Operand * Operand * Operand
    | Cne of Operand * Operand * Operand
    | Clt of Operand * Operand * Operand
    | Cle of Operand * Operand * Operand
    | Cgt of Operand * Operand * Operand
    | Cge of Operand * Operand * Operand
    | IfFalse of Operand * Label
    | IfTrue of Operand * Label
    | PushParam of Operand
    | FreeParams of int
    | Call of Operand * Label
    | CallExtern of string
    | Ret of Operand
    | Malloc of Operand * Operand
    | Free of Operand

and Operand =
    | Const of int // @Todo type of constant int * Type
    | Var of Symbol
    | Ptr of Symbol
    | Nop

and Label = string

let compile (program: Program, semantics: SemanticAnalysisResult) =
    let mutable label_index = 0
    let globals = new List<GlobalSymbol>()

    let alloc_label() =
        let result: Label = sprintf "_L%i" label_index
        label_index <- label_index + 1
        result

    let alloc_global label value symbol =
        globals.Add({ Data = { Label = label; Value = value }; Symbol = symbol})

    /// @Todo: If a global constant is already defined with identical size and value
    /// return a pointer to in instead of defining a new global.
    /// This is mainly used for string literals which cannot change during
    /// runtime.

    let alloc_global_ptr value t =
        let label = alloc_label()
        let sym = { Name = { Identifier = "@" + label }; Type = t }
        alloc_global (Some ("@" + label)) value (Some sym)
        // Create pointer to value in global section
        Ptr sym

    ///
    /// Attempt to evaluate expressions composed of constants at compile time.
    ///
    let rec const_fold e =
        // @Todo: - Handle symbols declared as constants
        //        - Handle non-integer types
        let make_int_const x =
            Some(Ast.LiteralExpression(Ast.IntLiteral(x)))

        match e with
        | BinaryExpression(l, op, r) ->
            let a = const_fold l
            let b = const_fold r

            let fold_binary =
                function
                | LiteralExpression(l1), LiteralExpression(l2) ->
                    match l1, l2 with
                    | IntLiteral(i1), IntLiteral(i2) ->
                        match op with
                        | Ast.Eq    -> make_int_const(if i1 = i2 then 1 else 0)
                        | Ast.Lt    -> make_int_const(if i1 < i2 then 1 else 0)
                        | Ast.Gt    -> make_int_const(if i1 > i2 then 1 else 0)
                        | Ast.LtEq  -> make_int_const(if i1 <= i2 then 1 else 0)
                        | Ast.GtEq  -> make_int_const(if i1 >= i2 then 1 else 0)
                        | Ast.NotEq -> make_int_const(if i1 <> i2 then 1 else 0)
                        | Ast.Xor   -> make_int_const(i1 ^^^ i2)
                        | Ast.Add   -> make_int_const(i1 + i2)
                        | Ast.Sub   -> make_int_const(i1 - i2)
                        | Ast.Mul   -> make_int_const(i1 * i2)
                        | Ast.Div   -> make_int_const(i1 / i2)
                        | Ast.Mod   -> make_int_const(i1 % i2)
                        // Todo bitwise and conditionals
                        | _ -> None
                    | _ -> None
                | _ -> None

            match fold_binary(a, b) with
            | Some(c) -> c
            | None -> Ast.BinaryExpression(a, op, b)
        | UnaryExpression(op, r) ->
            let a = const_fold r

            let fold_unary =
                function
                | LiteralExpression(l) ->
                    match l with
                    | IntLiteral(i) ->
                        match op with
                        | Ast.LogicalNot -> make_int_const(if i = 0 then 1 else 0)
                        | Ast.BitwiseNot -> make_int_const(~~~i)
                        | Negate         -> make_int_const(-i)
                        | Identity       -> make_int_const(+i)
                    | _ -> None
                | _ -> None

            match fold_unary(a) with
            | Some(c) -> c
            | None -> Ast.UnaryExpression(op, a)
        | _ -> e // The expression cannot be evaluate at compile time.

    let compile_proc (flags, name, parameters, return_type, block, semantics) =
        let body = new List<Instruction>()
        let mutable locals = 0
        let mutable temp_index = 0
        let current_while_end_label = Stack<Label>()

        let alloc_temp() =
            let id_ref = { Identifier = sprintf "%%R%i" temp_index }
            temp_index <- temp_index + 1
            Var({ Name = id_ref; Type = scalar_type Int}) // @Todo: types

        let alloc_var id_ref =
            let parent_id =
                match semantics.SymbolTable.[id_ref] with
                | ScalarDecl(_, id, _, _)
                | ArrayDecl(_, id, _, _, _) -> id
                | ProcedureDecl(_) ->
                    printfn "FATAL: Procedure declarations cannot be variables."
                    id_ref

            Var { Name = parent_id; Type = semantics.SymbolTable.GetIdentifierType id_ref }

        let alloc_ptr id_ref =
            let parent_id =
                match semantics.SymbolTable.[id_ref] with
                | ScalarDecl(_, id, _, _)
                | ArrayDecl(_, id, _, _, _) -> id
                | ProcedureDecl(_) ->
                    printfn "FATAL: Procedure pointers not yet supported."
                    id_ref

            Ptr { Name = parent_id; Type = scalar_type Int }

        let alloc_local() =
            locals <- locals + 1

        let emit i = (body.Add i)

        let rec compile_binaryexpr =
            function
            | (lhs, Ast.CondOr, rhs) ->
                let result = alloc_temp()
                printfn "%A" lhs
                let a_false = alloc_label()
                let end_label = alloc_label()

                emit(Copy(result, Const(0)))

                let a = compile_expression lhs
                emit(IfFalse(a, a_false))
                emit(Copy(result, Const(1)))
                emit(Goto(end_label))
                emit(Lab(a_false))

                let b = compile_expression rhs
                emit(IfFalse(b, end_label))
                emit(Copy(result, Const(1)))
                emit(Lab(end_label))

                result
            | (lhs, Ast.CondAnd, rhs) ->
                let result = alloc_temp()
                let a_true = alloc_label()
                let end_label = alloc_label()

                emit(Copy(result, Const(1)))

                let a = compile_expression lhs
                emit(IfTrue(a, a_true))
                emit(Copy(result, Const(0)))
                emit(Goto(end_label))
                emit(Lab(a_true))

                let b = compile_expression rhs
                emit(IfTrue(b, end_label))
                emit(Copy(result, Const(0)))
                emit(Lab(end_label))

                result
            | (lhs, op, rhs) ->
                let a = compile_expression lhs
                let b = compile_expression rhs
                compile_atomic_binexpr a op b

        and compile_unaryexpr =
            function
            | (Ast.LogicalNot, rhs) ->
                let a = compile_expression rhs
                let result = alloc_temp()
                let end_label = alloc_label()

                emit(Copy(result, Const(0)))
                emit(IfTrue(a, end_label))
                emit(Copy(result, Const(1)))
                emit(Lab(end_label))

                result
            | (op, rhs) ->
                let a = compile_expression rhs
                compile_atomic_uexpr op a

        /// Atomic binary expressions can be handled with
        /// a single IR instruction
        and compile_atomic_binexpr a op b =
            let result = alloc_temp()

            match op with
            | Ast.Add   -> emit(Add(result, a, b))
            | Ast.Sub   -> emit(Sub(result, a, b))
            | Ast.Mul   -> emit(Mul(result, a, b))
            | Ast.Div   -> emit(Div(result, a, b))
            | Ast.Mod   -> emit(Mod(result, a, b))
            | Ast.Eq    -> emit(Ceq(result, a, b))
            | Ast.NotEq -> emit(Cne(result, a, b))
            | Ast.Lt    -> emit(Clt(result, a, b))
            | Ast.LtEq  -> emit(Cle(result, a, b))
            | Ast.Gt    -> emit(Cgt(result, a, b))
            | Ast.GtEq  -> emit(Cge(result, a, b))
            | Ast.Xor   -> emit(BitXor(result, a, b))
            // @Todo: other bitwise operators not supported by the parser yet
            | CondAnd
            | CondOr ->
                printfn "FATAL: Cond cannot be compiled to a single instruction"

            result

        and compile_atomic_uexpr op a =
            let result = alloc_temp()

            match op with
            | Ast.BitwiseNot -> emit(BitNot(result, a))
            | Ast.Negate     -> emit(Neg(result, a))
            | Ast.Identity   -> ()
            | Ast.LogicalNot -> // @Todo: this should probably be called CondNot
                printfn "FATAL: Cond cannot be compiled to a single instruction"

            result

        and compile_expression expr =
            let expr_type = semantics.ExpressionTable.[expr]
            let e = const_fold expr

            match e with
            | BinaryExpression(lhs, op, rhs) ->
                compile_binaryexpr(lhs, op, rhs)
            | UnaryExpression(op, rhs) ->
                compile_unaryexpr(op, rhs)
            | ScalarAssignExpression(id_ref, e) ->
                let a = alloc_var id_ref
                let b = compile_expression e
                emit(Copy(a, b))
                a
            | ArrayAssignExpression(id_ref, index, e) ->
                let a = alloc_var id_ref
                let b = compile_expression e
                let i = compile_expression index
                //let offset = alloc_temp()
                //emit(Add(offset, a, i)) @Cleanup
                //emit(StoreRef(offset, b))
                emit(ArrayStore(a, i, b))
                a
            | ProcedureCallExpression(id_ref, args) ->
                let mutable argc = 0

                args |> List.iteri (fun i x ->
                    argc <- i + 1
                    emit(PushParam(compile_expression x)))

                // @Todo: return array type
                let a = alloc_temp()
                emit(Call(a, id_ref.Identifier))
                emit(FreeParams(argc))
                a
            | LiteralExpression(l) ->
                match l with
                | IntLiteral(i) ->
                    Const i
                | StringLiteral(s) ->
                    alloc_global None (Asm.Encode.i32_to_bytes s.Length) None // String capacity
                    alloc_global None (Asm.Encode.i32_to_bytes s.Length) None // String length
                    alloc_global_ptr (Asm.Encode.str_to_utf32 s) (scalar_type String)
                | TextLiteral(s) ->
                    // @Todo: word align
                    alloc_global None (Asm.Encode.i32_to_bytes (s.Length >>> 2)) None // String capacity
                    alloc_global None (Asm.Encode.i32_to_bytes (s.Length >>> 2)) None // String length
                    alloc_global_ptr (Asm.Encode.str_to_utf8 s) (scalar_type String)
                | _ -> Nop // @Todo other types
            | IdentifierExpression(id_ref) ->
                alloc_var id_ref
            | ArrayIdentifierExpression(id_ref, e) ->
                let a = alloc_var id_ref
                let i = compile_expression e
                //let offset = alloc_temp()
                //emit(Add(offset, a, i)) // @Cleanup
                let value = alloc_temp()
                emit(ArrayLoad(value, a, i))
                value
            | _ -> Nop

        and compile_block statements =
            statements |> List.map compile_statement

        and compile_statement =
            function
            | Declaration(decl) ->
                match decl with
                | VariableDecl(d) ->
                    match d with
                    | ScalarDecl(_, id_ref, _, Some e) ->
                        let b = compile_expression e
                        let a = alloc_var id_ref
                        emit(Copy(a, b))
                        alloc_local()
                        a
                    | ScalarDecl(_, id_ref, _, None) ->
                        // Local without assignmet
                        let a = alloc_ptr id_ref
                        emit(Copy(a, Const(0)))
                        alloc_local()
                        a
                    | ArrayDecl(_, id_ref, Some e, _, None) ->
                        let a = alloc_ptr id_ref
                        let size = compile_expression e
                        emit(Malloc(a, size))
                        alloc_local()
                        Nop
                    | ArrayDecl(_, id_ref, None, _, None) ->
                        let a = alloc_ptr id_ref
                        let size = Const(4) // @Todo: cell size
                        emit(Malloc(a, size))
                        alloc_local()
                        Nop
                    | ArrayDecl(_, id_ref, _, _, Some tup) ->
                        let a = alloc_ptr id_ref
                        let size =
                            let sym = semantics.SymbolTable.[id_ref]
                            match sym with
                            | ArrayDecl(_, _, Some e, _, _) ->
                                // Handle the case where an array has its size
                                // implicitly evaluated by the semantic analysis
                                compile_expression e
                            | ArrayDecl(_) ->
                                Const(4) // @Todo: use cell size
                            | _ ->
                                printfn "FATAL: Should not be here. Expected array decl"
                                Const(4)

                        emit(Malloc(a, size))
                        alloc_local()

                        // Initialize each element in the array
                        tup |> List.iteri (fun i e ->
                            let r = compile_expression e
                            let offset =
                                if i > 0 then
                                    // offset := a[i]
                                    //let o = alloc_temp()
                                    // @Cleanup
                                    //emit(Add(o, a, Const(i)))
                                    Const(i)
                                else Const(0) // a[0] == a
                            emit(ArrayStore(a, offset, r)))
                        Nop
                    | _ -> Nop
                | DeclNop -> Nop
            | SExpression(se) ->
                match se with
                | Expression(e) -> compile_expression e
                | Ast.Nop -> Nop
            | IfStatement(clauses, else_block) ->
                let end_label = alloc_label()

                let compile_cond_clause =
                    function
                    | (expr, block) ->
                        let a = compile_expression expr
                        let next_label = alloc_label()

                        emit(IfFalse(a, next_label))
                        compile_block block |> ignore
                        emit(Goto(end_label))
                        emit(Lab(next_label))

                clauses |> List.iter compile_cond_clause

                match else_block with
                | Some(block) ->
                    compile_block block |> ignore
                | None -> ()

                emit(Lab(end_label))
                Nop
            | WhileStatement(expr, block) ->
                // @Note: While statement syntax might change
                // to using Statement lists (similar to if statement)
                // rather than CompoundStatements.
                let loop_label = alloc_label()
                let cond_label = alloc_label()
                let end_label = alloc_label()

                current_while_end_label.Push end_label

                emit(Goto(cond_label))
                emit(Lab(loop_label))
                compile_statement block |> ignore
                emit(Lab(cond_label))

                let a = compile_expression expr
                emit(IfTrue(a, loop_label))
                emit(Lab(end_label))

                current_while_end_label.Pop |> ignore
                Nop
            | ReturnStatement(expr) ->
                match expr with
                | Some(e) ->
                    let a = compile_expression e
                    emit(Ret(a))
                | None -> emit(Ret(Const(0)))
                Nop
            | BreakStatement ->
                emit(Goto(current_while_end_label.Peek()))
                Nop
            | CompoundStatement(s) ->
                compile_block s |> ignore
                Nop
            | _ -> Nop

        let is_extern = flags = (ProcFlags.EXTERN)
        let mutable end_label = None

        if not is_extern then
            compile_block block |> ignore
            let label = alloc_label()
            emit(Lab(label))
            end_label <- Some label

        {
            Name       = name
            ReturnType = return_type
            Parameters = parameters
            Body       = Seq.toList body
            Locals     = locals
            EndLabel   = end_label
            IsExtern   = is_extern
        }

    let procs = new List<Proc>()
    let mutable init_proc = List.empty<Statement>

    let compile_decl decl =
        match decl with
        | VariableDecl(d) ->
            match d with
            | ProcedureDecl(flags, id, p, t, stmt) ->
                procs.Add(compile_proc(flags, id.Identifier, p, t, stmt, semantics))
            | ScalarDecl(_, id_ref, Some(t), None) ->
                let sym = { Name = id_ref; Type = semantics.SymbolTable.GetIdentifierType id_ref }
                alloc_global (Some(id_ref.Identifier)) (Asm.Encode.i32_to_bytes 0) (Some sym)
                ()
            | ScalarDecl(_, id_ref, _, _)
            | ArrayDecl(_, id_ref, _, _, _) ->
                let sym = { Name = id_ref; Type = semantics.SymbolTable.GetIdentifierType id_ref }
                alloc_global (Some(id_ref.Identifier)) (Asm.Encode.i32_to_bytes 0) (Some sym)
                init_proc <- init_proc @ [Declaration decl]
        | DeclNop -> ()

    program |> List.iter compile_decl
    procs.Add(compile_proc(ProcFlags.PUBLIC, "__init__", [], Void, init_proc, semantics))

    {
        Globals = Seq.toList globals
        Procedures = Seq.toList procs
    }

let dump (ir: IR) =
    let instruction_tostr =
        let s =
            function
            | Const(i) -> i.ToString()
            | Var(sym) -> sym.Name.Identifier
            | Ptr(sym) -> "*" + sym.Name.Identifier
            | Nop -> "nop"

        function
        | Add(dst, op1, op2) -> sprintf "\t%s := %s + %s" (s dst) (s op1) (s op2)
        | Sub(dst, op1, op2) -> sprintf "\t%s := %s - %s" (s dst) (s op1) (s op2)
        | Mul(dst, op1, op2) -> sprintf "\t%s := %s * %s" (s dst) (s op1) (s op2)
        | Div(dst, op1, op2) -> sprintf "\t%s := %s / %s" (s dst) (s op1) (s op2)
        | Mod(dst, op1, op2) -> sprintf "\t%s := %s %% %s" (s dst) (s op1) (s op2)
        | Ceq(dst, op1, op2) -> sprintf "\t%s := %s = %s" (s dst) (s op1) (s op2)
        | Cne(dst, op1, op2) -> sprintf "\t%s := %s <> %s" (s dst) (s op1) (s op2)
        | Clt(dst, op1, op2) -> sprintf "\t%s := %s < %s" (s dst) (s op1) (s op2)
        | Cgt(dst, op1, op2) -> sprintf "\t%s := %s > %s" (s dst) (s op1) (s op2)
        | Cle(dst, op1, op2) -> sprintf "\t%s := %s <= %s" (s dst) (s op1) (s op2)
        | Cge(dst, op1, op2) -> sprintf "\t%s := %s >=%s" (s dst) (s op1) (s op2)
        | BitAnd(dst, op1, op2) -> sprintf "\t%s := %s & %s" (s dst) (s op1) (s op2)
        | BitOr(dst, op1, op2) -> sprintf "\t%s := %s | %s" (s dst) (s op1) (s op2)
        | BitXor(dst, op1, op2) -> sprintf "\t%s := %s xor %s" (s dst) (s op1) (s op2)
        | Neg(dst, op1) -> sprintf "\t%s := -%s" (s dst) (s op1)
        | BitNot(dst, op1) -> sprintf "\t%s := ~%s" (s dst) (s op1)
        | Copy(dst, op) -> sprintf "\t%s := %s" (s dst) (s op)
        | Call(dst, op) -> sprintf "\t%s := call %s" (s dst) op
        | CallExtern(dst) -> sprintf "\t[NOT SUPPORTED] %s" dst // @Todo
        | Malloc(dst, op) -> sprintf "\t%s := malloc %s" (s dst) (s op)
        | Free(op) -> sprintf "\tfree %s" (s op)
        | LoadRef(dst, op) -> sprintf "\t%s := %s^" (s dst) (s op)
        | StoreRef(dst, op) -> sprintf "\t%s^ := %s" (s dst) (s op)
        | ArrayLoad(dst, a, i) -> sprintf "\t%s := %s[%s]" (s dst) (s a) (s i)
        | ArrayStore(a, i, src) -> sprintf "\t%s[%s] := %s" (s a) (s i) (s src)
        | PushParam(op) -> sprintf "\tPushParam %s" (s op)
        | FreeParams(op) -> sprintf "\tFreeParams %i" op
        | IfFalse(op, label) -> sprintf "\tIfFalse %s %s" (s op) label
        | IfTrue(op, label) -> sprintf "\tIfTrue %s %s" (s op) label
        | Lab(label) -> sprintf "%s:" label
        | Goto(label) -> sprintf "\tgoto %s" label
        | Ret(op) -> sprintf "\tret %s" (s op)

    let mutable result = ".text\n"
    let append i = result <- sprintf "%s%s\n" result (instruction_tostr i)

    ir.Procedures |> List.iter (fun proc ->
        if proc.IsExtern then
            result <- sprintf "%sestern(%s)\n" result proc.Name
        else result <- sprintf "%s%s:\n" result proc.Name
        proc.Body |> List.iter append)

    result <- result + "\n.raw_data\n"

    ir.Globals |> List.iter (fun g ->
        let name =
            function
            | Some n -> n + ":"
            | None -> ""
        result <- sprintf "%s%s %A\n" result (name g.Data.Label) (g.Data.Value))

    result
