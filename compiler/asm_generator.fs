module AsmGenerator

open Asm
open IRCompiler
open SemanticAnalysis
open System.Collections.Generic
open Ast


type AsmObject =
    {
        Type  : VarType
        mutable Address : Address option
    }

and Address =
    | RelativeAddress of Asm.Register * int
    | AbsoluteAddress of Label

and VarType =
    | Int
    | Float
    | Byte
    | Void

let compile (ir : IR) =
    let symbol_table = new Dictionary<IdentifierRef, AsmObject>(HashIdentity.Reference)
    let register_map = [| None; None |]
    let mutable text = List.empty<Asm.OpCode>
    let mutable data = List.empty<Asm.Data>
    let mutable imports = List.empty<Asm.ProcTableEntry>
    let mutable exports = List.empty<Asm.ProcTableEntry>
    let mutable text_pos = 0

    let emit opcode =
        text <- text @ [opcode]
        text_pos <- text_pos + 1

    let rec insert_opcode opcode i l =
        match i, l with
        | 0, xs -> opcode :: xs
        | i, x :: xs -> x :: insert_opcode opcode (i - 1) xs
        | i, [] -> failwith "FATAL: Index out of range."

    let emit_dat dat =
        data <- data @ [dat]

    let is_live variable r =
        match variable with
        | Var(sym) ->
           symbol_table.ContainsKey sym.Name && register_map.[int r] = Some symbol_table.[sym.Name]
        | _ -> false

    let alloc_object sym obj =
        if symbol_table.ContainsKey sym then
            symbol_table.[sym] = obj |> ignore
        else
            symbol_table.Add(sym, obj)

    let asm_type =
        function
        | { Type = Ast.Int; IsArray = false } -> Int
        | { Type = Ast.Int; IsArray = true } -> Int
        | { Type = Ast.Float; IsArray = true } -> Int
        | { Type = Ast.Float; IsArray = false } -> Float
        | { Type = Ast.String; IsArray = false } -> Int
        | _ -> Int // @Todo: other types

    let find_type (sym: Operand) (fallback_type: VarType option) =
        match sym with
        | Const(_) -> Int // @Todo: float
        | Ptr(_) -> Int
        | Var(sym) -> asm_type sym.Type
        | IRCompiler.Nop -> Void

    let compile_proc (proc: IRCompiler.Proc) =
        // Setup stack
        emit(Push(Register.EBP))
        emit(Mov(Register.EBP, Register.ESP))

        let stack_resize_pos = text_pos

        proc.Parameters |> List.iteri (fun i p ->

            // Parameter addresses at EBP+parameter_index+2
            //
            // Stack:
            // ...
            // esp-5    ebp+3    param 1
            // esp-4    ebp+2    param 0
            // esp-3    ebp+1    return address
            // esp-2    ebp+0    caller's frame pointer
            // esp-1    ebp-1    local 0
            // esp-0    ebp-2    local 1
            // ...

            let a = Some(RelativeAddress(Register.EBP, i + 2))

            match p with
            | ScalarDecl(_, id_ref, Some(t), _) ->
                let o = { Type = asm_type { Type = t; IsArray = false }; Address = a }
                symbol_table.Add(id_ref, o)
            | ArrayDecl(_, id_ref, _, Some(t), _) ->
                let o = { Type = asm_type { Type = t; IsArray = true }; Address = a }
                symbol_table.Add(id_ref, o)
            | _ -> printfn "FATAL: Unsupported parameter declaration %A" p)

        let mutable stack_offset = 0

        let alloc_stack() =
            stack_offset <- stack_offset - 1
            stack_offset

        let address_error obj =
            printfn "FATAL: Expected memory addressable variable, found %A" obj

        /// Determines if an object will be used in the future
        let lookahead pos obj =
            let mutable result = false

            let obj_equal =
                function
                | Var(sym) ->
                    symbol_table.ContainsKey sym.Name && symbol_table.[sym.Name] = obj
                | _ -> false

            proc.Body |> List.iteri (fun i line ->
                if i > pos + 0 then
                    match line with
                    | IRCompiler.Add(_, lhs, rhs)
                    | IRCompiler.Sub(_, lhs, rhs)
                    | IRCompiler.Mul(_, lhs, rhs)
                    | IRCompiler.Div(_, lhs, rhs) ->
                        if obj_equal lhs || obj_equal rhs then
                            result <- true
                    | IRCompiler.Copy(_, src)
                    | IRCompiler.LoadRef(_, src)
                    | IRCompiler.StoreRef(_, src) ->
                        if obj_equal src then
                            result <- true
                    | PushParam(op) ->
                        if obj_equal op then
                            result <- true
                    | _ -> ())

            result

        let rec free_register pos (r : Asm.Register) (force: bool) =
            if r = Register.EAX then
                // Only EAX might be spilled
                match register_map.[int r] with
                | Some(obj) ->
                    if force || lookahead pos obj then
                        // This object will be used later, it must be spilled into
                        // memory.
                        match obj.Address with
                        | Some(address) ->
                            match address with
                            | RelativeAddress(reg, o) ->
                                emit(Asm.Stw(reg, Asm.Value o))
                            | AbsoluteAddress(l) ->
                                free_register pos Register.EDX false
                                emit(Lea(Register.EDX, Asm.Label l))
                                emit(Asm.Stw(Register.EDX, Asm.Value 0))
                        | None ->
                            // This object has no address, make space for it
                            // in the stack.
                            let offset = alloc_stack()
                            let a = RelativeAddress(Register.EBP, offset)
                            obj.Address <- Some a
                            emit(Asm.Stw(Register.EBP, Asm.Value offset))

                | None -> ()

            register_map.[int r] <- None

        let alloc_register (r : Asm.Register) (v: AsmObject) =
            register_map.[int r] <- Some(v)

        let load_edx pos rhs =
            // rhs gets loaded into edx
            match rhs with
            | Const(c) ->
                // Reserve edx register
                free_register pos Register.EDX false
                alloc_register Register.EDX { Type = Int; Address = None }
                // Load constant
                emit(Lea(Register.EDX, Value(c)))
            | Ptr(sym) ->
                if is_live rhs Register.EAX then
                    // Variable available in EAX, no need to load it from memory
                    let obj = symbol_table.[sym.Name]
                    alloc_register Register.EDX obj
                    emit(Mov(Register.EDX, Register.EAX))
                else if not(is_live rhs Register.EDX) then
                    // Variable not in a register, load from memory
                    let obj = symbol_table.[sym.Name]

                    match obj.Address with
                    | Some(a) ->
                        // EAX register must be freed to load from memory
                        free_register pos Register.EAX false
                        free_register pos Register.EDX false
                        alloc_register Register.EDX obj

                        match a with
                        | RelativeAddress(r, o) ->
                            emit(Ldw(r, Asm.Value(o)))
                        | AbsoluteAddress(l) ->
                            // This value is in the data section
                            emit(Lea(Register.EAX, Asm.Label l))

                        emit(Mov(Register.EDX, Register.EAX))
                    | None -> address_error sym
            | Var(sym) ->
                if is_live rhs Register.EAX then
                    // Variable available in EAX, no need to load it from memory
                    let obj = symbol_table.[sym.Name]
                    alloc_register Register.EDX obj
                    emit(Mov(Register.EDX, Register.EAX))
                else if not(is_live rhs Register.EDX) then
                    // Variable not in a register, load from memory
                    let obj = symbol_table.[sym.Name]

                    match obj.Address with
                    | Some(a) ->
                        // EAX register must be freed to load from memory
                        free_register pos Register.EAX false
                        free_register pos Register.EDX false
                        alloc_register Register.EDX obj

                        match a with
                        | RelativeAddress(r, o) -> emit(Ldw(r, Asm.Value(o)))
                        | AbsoluteAddress(l) ->
                            // This value is in the data section
                            emit(Lea(Register.EAX, Asm.Label l))
                            emit(Ldw(Register.EAX, Asm.Value 0))

                        emit(Mov(Register.EDX, Register.EAX))
                    | None -> address_error sym
            | IRCompiler.Nop -> ()

        let load_eax pos lhs =
            match lhs with
            | Const(c) ->
                // Reserve eax register
                free_register pos Register.EAX false
                alloc_register Register.EAX { Type = Int; Address = None }
                // Load constant
                emit(Lea(Register.EAX, Value(c)))
            | Ptr(sym) ->
                if not(is_live lhs Register.EAX) then
                    let obj = symbol_table.[sym.Name]

                    match obj.Address with
                    | Some(a) ->
                        free_register pos Register.EAX false
                        alloc_register Register.EAX obj

                        match a with
                        | RelativeAddress(r, o) -> emit(Ldw(r, Asm.Value o))
                        | AbsoluteAddress(l) ->
                            emit(Lea(Register.EAX, Asm.Label l))
                    | None -> address_error sym
            | Var(sym) ->
                // First of all check if variable already in eax
                if not(is_live lhs Register.EAX) then
                    let obj = symbol_table.[sym.Name]

                    match obj.Address with
                    | Some(a) ->
                        free_register pos Register.EAX false
                        alloc_register Register.EAX obj

                        match a with
                        | RelativeAddress(r, o) -> emit(Ldw(r, Asm.Value(o)))
                        | AbsoluteAddress(l) ->
                            // This value is in the data section
                            emit(Lea(Register.EAX, Asm.Label l))
                            emit(Ldw(Register.EAX, Asm.Value 0))
                    | None -> address_error sym
            | IRCompiler.Nop -> ()

        let store_eax dst =
            // At this point we have lhs in EAX and rhs in EDX
            match dst with
            | Ptr(sym)
            | Var(sym) ->
                if not(symbol_table.ContainsKey sym.Name) then
                    alloc_object sym.Name { Type = asm_type sym.Type; Address = None }

                alloc_register Register.EAX symbol_table.[sym.Name]
            | IRCompiler.Nop
            | Const(_) -> address_error dst

        let rec compile_ir pos x =
            let binary_op dst lhs rhs int_opcode float_opcode =
                load_edx pos rhs
                load_eax pos lhs
                store_eax dst

                // @Todo: float + int cast
                match dst with
                | Ptr(op)
                | Var(op) ->
                    let r = (Register.EAX, Register.EDX)

                    match op.Type.Type with
                    | Ast.Int ->
                        match int_opcode with
                        | Some(opcode) -> emit(opcode r)
                        | None -> printfn "FATAL: %A undefined for 'int'" int_opcode
                    | Ast.Float ->
                        match float_opcode with
                        | Some(opcode) -> emit(opcode r)
                        | None -> printfn "FATAL: %A undefined for 'float'" int_opcode
                    | _ -> printfn "FATAL: Invalid type for instruction."
                | _ -> ()

            match x with
            | IRCompiler.Lab(l) -> emit (Asm.Lab(l))
            | LoadRef(dst, src) ->
                load_eax pos src
                emit(Ldw(Register.EAX, Asm.Value 0))
                store_eax dst
                free_register pos Register.EAX true
            | StoreRef(dst, src) ->
                load_edx pos dst
                load_eax pos src
                emit(Stw(Register.EDX, Asm.Value 0))
            | ArrayLoad(dst, a, i) ->
                load_edx pos i
                load_eax pos a
                store_eax dst
                emit(Asm.Add(Register.EDX, Register.EAX))
                emit(Ldw(Register.EDX, Asm.Value 0))
            | ArrayStore(a, i, src) ->
                load_edx pos i
                load_eax pos a
                emit(Asm.Add(Register.EDX, Register.EAX))
                load_eax pos src
                emit(Asm.Stw(Register.EDX, Asm.Value 0))
            | Goto(l) -> emit (Asm.Jmp(Asm.Label(l)))
            | IRCompiler.Copy(dst, src) ->
                load_eax pos src
                store_eax dst
                free_register pos Register.EAX true
            | IRCompiler.Add(dst, lhs, rhs) -> binary_op dst lhs rhs (Some Asm.Add) (Some Asm.Addf)
            | IRCompiler.Sub(dst, lhs, rhs) -> binary_op dst lhs rhs (Some Asm.Sub) (Some Asm.Subf)
            | IRCompiler.Mul(dst, lhs, rhs) -> binary_op dst lhs rhs (Some Asm.Mul) (Some Asm.Mulf)
            | IRCompiler.Div(dst, lhs, rhs) -> binary_op dst lhs rhs (Some Asm.Div) (Some Asm.Divf)
            | IRCompiler.Mod(dst, lhs, rhs) -> binary_op dst lhs rhs (Some Asm.Mod) None
            | IRCompiler.BitOr(dst, lhs, rhs) -> binary_op dst lhs rhs (Some Asm.Or) None
            | IRCompiler.BitXor(dst, lhs, rhs) -> binary_op dst lhs rhs (Some Asm.Xor) None
            | IRCompiler.BitAnd(dst, lhs, rhs) -> binary_op dst lhs rhs (Some Asm.And) None
            | IRCompiler.Ceq(dst, lhs, rhs) -> binary_op dst lhs rhs (Some Asm.Ceq) (Some Asm.Ceq)
            | IRCompiler.Cne(dst, lhs, rhs) -> binary_op dst lhs rhs (Some Asm.Cne) (Some Asm.Cne)
            | IRCompiler.Clt(dst, lhs, rhs) -> binary_op dst lhs rhs (Some Asm.Clt) (Some Asm.Cltf)
            | IRCompiler.Cle(dst, lhs, rhs) -> binary_op dst lhs rhs (Some Asm.Cle) (Some Asm.Clef)
            | IRCompiler.Cgt(dst, lhs, rhs) -> binary_op dst lhs rhs (Some Asm.Cgt) (Some Asm.Cgtf)
            | IRCompiler.Cge(dst, lhs, rhs) -> binary_op dst lhs rhs (Some Asm.Cge) (Some Asm.Cgef)
            // @Temporary: Until neg instruction is added
            | IRCompiler.Neg(dst, lhs) -> binary_op dst lhs (Const -1) (Some Asm.Mul) (Some Asm.Mulf)
            // @Todo: Not
            | IfFalse(op, l) ->
                load_eax pos op
                emit(Jne(Asm.Label l))
            | IfTrue(op, l) ->
                load_eax pos op
                emit(Je(Asm.Label l))
            | PushParam(p) ->
                match p with
                | Const(c) -> emit(ImmPush(Asm.Value c))
                | _ ->
                    load_eax pos p
                    emit(Asm.Push(Register.EAX))
            | FreeParams(count) -> emit(ImmAdd(Register.ESP, Asm.Value count))
            | Call(dst, proc) ->
                emit(Asm.Call(Asm.Label proc))
                store_eax dst
                free_register pos Register.EAX false
            | CallExtern(dst, proc) ->
                emit(Asm.Calx(Asm.Label proc))
                store_eax dst
                free_register pos Register.EAX false
            // @Todo: Call extern
            | Ret(res) ->
                match proc.EndLabel with
                | Some(l) ->
                    load_eax pos res
                    emit(Jmp(Asm.Label l))
                | None -> printfn "FATAL: Missing end label for %A" proc.Name
            | Malloc(dst, src) ->
                load_eax pos src
                store_eax dst
                emit(Asm.Alloc(Register.EAX))
                free_register pos Register.EAX true
            | Free(op) ->
                // @Performance: Use edx?
                load_eax pos op
                emit(Asm.Free(Register.EAX))
            // @Todo: Endproc
            | _ -> ()

        proc.Body |> List.iteri (fun i x -> compile_ir i x)

        // Emit instruction to allocate local variables on the stack at
        // the begining of the procedure.
        let resize_instruction = ImmAdd(Register.ESP, Asm.Value stack_offset)
        text <- insert_opcode resize_instruction stack_resize_pos text
        text_pos <- text_pos + 1

        // Cleanup stack, return to caller
        emit(Mov(Register.ESP, Register.EBP))
        emit(Pop(Register.EBP))
        emit(Pop(Register.EDX))
        emit(Asm.Ret(Register.EDX))

    ir.Globals |> List.iter (fun x ->
        match x.Symbol with
        | Some(s) ->
            match x.Data.Label with
            | Some(l) ->
                let obj = { Type = asm_type s.Type; Address = Some(AbsoluteAddress l) }
                symbol_table.Add(s.Name, obj)
            | None -> printfn "FATAL: Symbol reference requires label."
        | None -> ()

        emit_dat x.Data)

    ir.Procedures |> List.iter (fun p ->
        if p.IsExtern then
            imports <- { Name = p.Name; Label = p.Name } :: imports
        else
            emit(Asm.Lab(p.Name))
            exports <- { Name = p.Name; Label = p.Name } :: exports

            if p.Body.Length <= 1 then
                // This is an empty procedure,
                // We can just return to caller without
                // setting up the stack.
                emit(Pop(Register.EDX))
                emit(Asm.Ret(Register.EDX))
            else compile_proc p)

    {
        ImportTable = imports
        ExportTable = exports
        Text = text
        Data = data
    }
