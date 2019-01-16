module IRCompiler

open Ast
open System.Collections.Generic
open SemanticAnalysis

type IR =
    {
        Globals    : DataSymbol list
        Procedures : Proc list
    }

and Proc =
    {
        Name       : string
        ReturnType : TypeSpec
        Parameters : Parameters
        Body       : Instruction list
        Locals     : int
    }

and Symbol =
    {
        Name : string
        Type : VariableType
    }

and DataSymbol =
    {
        Name  : Label option
        Type  : MachineType
        Value : string
    }

and MachineType =
    | BYTE
    | WORD

    override x.ToString() =
        match x with
        | BYTE -> ".byte"
        | WORD -> ".word"

and Instruction =
    | Lod of Symbol
    | Sto of Symbol
    | Lab of Label
    | LoadRef of Operand * Operand
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
    | Const of int
    | Var of Symbol
    | Temp of int
    | Ref of Symbol
    | Nop

and Label = string

and StackOffset = int

let compile (program: Program, semantics: SemanticAnalysisResult) =
    let mutable label_index = 0
    let globals = new List<DataSymbol>()

    let compile_proc (name, parameters, return_type, block, semantics: SemanticAnalysisResult) =
        let body = new List<Instruction>()
        let mutable locals = 0
        let mutable temp_index = 0
        let current_while_end_label = Stack<Label>()

        let alloc_label() =
            let result: Label = sprintf "_L%i" label_index
            label_index <- label_index + 1
            result

        let alloc_temp() =
            let result = temp_index
            temp_index <- temp_index + 1
            Temp(result)

        let alloc_var id_ref =
            Var { Name = id_ref.Identifier; Type = semantics.SymbolTable.GetIdentifierType id_ref }

        let alloc_local() =
            locals <- locals + 1

        let alloc_global label size value =
            globals.Add({ Name = label; Type = size; Value = value })

        /// @Todo: If a global constant is already defined with identical size and value
        /// return a pointer to in instead of defining a new global.
        /// This is mainly used for string literals which cannot change during
        /// runtime.

        let alloc_global_ptr size value =
            let label = alloc_label()
            alloc_global (Some ("@" + label)) size value
            // Create pointer to value in global section
            Var { Name = "@" + label; Type = { Type = Int; IsArray = false } }

        let emit i = (body.Add i)

        let rec compile_binaryexpr =
            function
            | (lhs, Ast.CondOr, rhs) ->
                let result = alloc_temp()
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
            | Ast.LtEq  -> emit(Clt(result, a, b))
            | Ast.Gt    -> emit(Clt(result, a, b))
            | Ast.GtEq  -> emit(Clt(result, a, b))
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

        and compile_expression e =
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
                let offset = alloc_temp()
                emit(Add(offset, a, i))
                emit(Copy(offset, b))
                offset
            | ProcedureCallExpression(id_ref, args) ->
                let mutable argc = 0

                args |> List.iteri (fun i x ->
                    argc <- i + 1
                    emit(PushParam(compile_expression x)))

                let a = alloc_temp()
                emit(Call(a, id_ref.Identifier))
                emit(FreeParams(argc))
                a
            | LiteralExpression(l) ->
                match l with
                | IntLiteral(i) ->
                    Const i
                | StringLiteral(s) ->
                    alloc_global None MachineType.WORD (s.Length.ToString()) // String capacity
                    alloc_global None MachineType.WORD (s.Length.ToString()) // String length
                    alloc_global_ptr MachineType.WORD (sprintf "\"%s\"" s)
                | _ -> Nop // Todo other types
            | IdentifierExpression(id_ref) ->
                alloc_var id_ref
            | ArrayIdentifierExpression(id_ref, e) ->
                let a = alloc_var id_ref
                let i = compile_expression e
                let offset = alloc_temp()
                emit(Add(offset, a, i))
                let value = alloc_temp()
                emit(LoadRef(value, offset))
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
                    | ScalarDecl(_) ->
                        // Local without assignmet
                        alloc_local()
                        Nop
                    | ArrayDecl(_, id_ref, Some e, _, None) ->
                        let a = alloc_var id_ref
                        let size = compile_expression e
                        emit(Malloc(a, size))
                        alloc_local()
                        Nop
                    | ArrayDecl(_, id_ref, None, _, None) ->
                        let a = alloc_var id_ref
                        let size = Const(4) // @Todo: cell size
                        emit(Malloc(a, size))
                        alloc_local()
                        Nop
                    | ArrayDecl(_, id_ref, _, _, Some tup) ->
                        let a = alloc_var id_ref
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
                                    let o = alloc_temp()
                                    emit(Add(o, a, Const(i)))
                                    o
                                else a // a[0] == a
                            emit(Copy(offset, r)))
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

        compile_block block |> ignore

        {
            Name       = name
            ReturnType = return_type
            Parameters = parameters
            Body       = Seq.toList body
            Locals     = locals
        }

    let procs = new List<Proc>()

    let compile_decl decl =
        match decl with
        | VariableDecl(d) ->
            match d with
            | ProcedureDecl(_, id, p, t, stmt) ->
                procs.Add(compile_proc(id.Identifier, p, t, stmt, semantics))
            | _ -> () // @Todo handle global variables
        | DeclNop -> ()

    program |> List.iter compile_decl

    {
        Globals = Seq.toList globals
        Procedures = Seq.toList procs
    }

let instruction_tostr =
    let s =
        function
        | Const(i) -> i.ToString()
        | Var(sym)
        | Ref(sym) -> sym.Name
        | Temp(i) -> sprintf "(R%i)" i
        | Nop -> "nop"

    function
    | Add(dst, op1, op2) -> sprintf "%s := %s + %s" (s dst) (s op1) (s op2)
    | Sub(dst, op1, op2) -> sprintf "%s := %s - %s" (s dst) (s op1) (s op2)
    | Mul(dst, op1, op2) -> sprintf "%s := %s * %s" (s dst) (s op1) (s op2)
    | Div(dst, op1, op2) -> sprintf "%s := %s / %s" (s dst) (s op1) (s op2)
    | Mod(dst, op1, op2) -> sprintf "%s := %s %% %s" (s dst) (s op1) (s op2)
    | Ceq(dst, op1, op2) -> sprintf "%s := %s = %s" (s dst) (s op1) (s op2)
    | Cne(dst, op1, op2) -> sprintf "%s := %s <> %s" (s dst) (s op1) (s op2)
    | Clt(dst, op1, op2) -> sprintf "%s := %s < %s" (s dst) (s op1) (s op2)
    | Cgt(dst, op1, op2) -> sprintf "%s := %s > %s" (s dst) (s op1) (s op2)
    | Cle(dst, op1, op2) -> sprintf "%s := %s <= %s" (s dst) (s op1) (s op2)
    | Cge(dst, op1, op2) -> sprintf "%s := %s >= %s" (s dst) (s op1) (s op2)
    | Neg(dst, op1) -> sprintf "%s := -%s" (s dst) (s op1)
    | Copy(dst, op) -> sprintf "%s := %s" (s dst) (s op)
    | Call(dst, op) -> sprintf "%s := call %s" (s dst) op
    | Malloc(dst, op) -> sprintf "%s := malloc %s" (s dst) (s op)
    | LoadRef(dst, op) -> sprintf "%s := %s^" (s dst) (s op)
    | PushParam(op) -> sprintf "PushParam %s" (s op)
    | FreeParams(op) -> sprintf "FreeParams %i" op
    | IfFalse(op, label) -> sprintf "IfFalse %s %s" (s op) label
    | IfTrue(op, label) -> sprintf "IfTrue %s %s" (s op) label
    | Lab(label) -> sprintf "%s:" label
    | Goto(label) -> sprintf "goto %s" label
    | Ret(op) -> sprintf "ret %s" (s op)
    | _ -> "Not supported"

let tac_to_text (ir: IR) =
    let mutable result = ""
    let append i = result <- sprintf "%s\t%s\n" result (instruction_tostr i)

    ir.Globals |> List.iter (fun g ->
        let name =
            function
            | Some n -> n + ":"
            | None -> ""
        result <- sprintf "%s%s%s%s\n" result (name g.Name) (g.Type.ToString()) g.Value)

    ir.Procedures |> List.iter (fun proc ->
        result <- sprintf "%s%s:\n" result proc.Name
        proc.Body |> List.iter append)

    result
