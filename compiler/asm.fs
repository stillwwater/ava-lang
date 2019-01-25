
module Asm

open System.Text
open System.Collections.Generic

let str_to_utf32 (str: string) =
    Array.toList (Encoding.UTF32.GetBytes(str))

let str_to_utf8 (str: string) =
    Array.toList (Encoding.UTF8.GetBytes(str))

let i32_to_bytes i =
    [ byte(i &&& 0xff);
       byte((i &&& 0xff00) >>> 8);
       byte((i &&& 0xff0000) >>> 16);
       byte((i &&& 0xff000000) >>> 24); ]

type Bytecode =
    {
        ImportTable : ImportTableEntry list
        ExportTable : ExportTableEntry list
        Text        : OpCode list
        Const       : Data list
        Data        : Data list
    }

and Label = string

and Immediate =
    | Value of int
    | Label of Label

and Data =
    {
        Label : Label option
        Value : byte list
    }

and ImportTableEntry =
    {
        Name  : string
        Label : Label
    }

and ExportTableEntry =
    {
        Pointer : int
        Name    : string
    }

and Header =
    {
        Signature : int16
        Version   : int16
        StackSize : int32
        HeapSize  : int32
        TextPtr   : int32
        ConstPtr  : int32
        DatPtr    : int32
        ImportTablePtr : int32
        ExportTablePtr : int32
    }

    static member Size = 32 >>> 2

    member x.ToBytes() =
        List.concat
            [ i32_to_bytes (int((x.Version <<< 8) ||| x.Signature));
               i32_to_bytes x.StackSize;
               i32_to_bytes x.HeapSize;
               i32_to_bytes x.TextPtr;
               i32_to_bytes x.ConstPtr;
               i32_to_bytes x.DatPtr;
               i32_to_bytes x.ImportTablePtr;
               i32_to_bytes x.ExportTablePtr ]

and Register =
    | EAX = 0x00
    | EDX = 0x01
    | ESP = 0x02
    | EBP = 0x04

and OpCode =
    | Sll      of Register * Register
    | Srl      of Register * Register
    | Srlu     of Register * Register
    | Mul      of Register * Register
    | Div      of Register * Register
    | Divu     of Register * Register
    | Mod      of Register * Register
    | Modu     of Register * Register
    | Add      of Register * Register
    | Sub      of Register * Register
    | Mov      of Register * Register
    | And      of Register * Register
    | Or       of Register * Register
    | Xor      of Register * Register
    | Ceq      of Register * Register
    | Cne      of Register * Register
    | Clt      of Register * Register
    | Cle      of Register * Register
    | Cgt      of Register * Register
    | Cge      of Register * Register
    | Cltu     of Register * Register
    | Cleu     of Register * Register
    | Cgtu     of Register * Register
    | Cgeu     of Register * Register
    | Cltf     of Register * Register
    | Clef     of Register * Register
    | Cgtf     of Register * Register
    | Cgef     of Register * Register
    | Mulf     of Register * Register
    | Divf     of Register * Register
    | Addf     of Register * Register
    | Subf     of Register * Register
    | Cvtfw    of Register
    | Cvtwf    of Register
    | Alloc    of Register
    | Free     of Register
    | Push     of Register
    | Pop      of Register
    | Not      of Register
    | Ret      of Register
    | Halt     of Register
    | ImmAdd   of Register * Immediate
    | Ldw      of Register * Immediate
    | Ldb      of Register * Immediate
    | Stw      of Register * Immediate
    | Stb      of Register * Immediate
    | Lea      of Register * Immediate
    | ImmSll   of Register * Immediate
    | Jmp      of Immediate
    | Je       of Immediate
    | Jne      of Immediate
    | ImmPush  of Immediate
    | Call     of Immediate
    | Calx     of Immediate
    | Lab      of Label

    member x.ToBytes(context: Dictionary<Label, int>) =
        let eval =
            function
            | Label(l) -> context.[l] // @Todo error
            | Value(v) -> v

        match x with
        | Sll(r0, r1)    -> [ 0x00uy; 0x00uy; byte r0; byte r1; ]
        | Srl(r0, r1)    -> [ 0x01uy; 0x00uy; byte r0; byte r1; ]
        | Srlu(r0, r1)   -> [ 0x02uy; 0x00uy; byte r0; byte r1; ]
        | Mul(r0, r1)    -> [ 0x03uy; 0x00uy; byte r0; byte r1; ]
        | Div(r0, r1)    -> [ 0x04uy; 0x00uy; byte r0; byte r1; ]
        | Divu(r0, r1)   -> [ 0x05uy; 0x00uy; byte r0; byte r1; ]
        | Mod(r0, r1)    -> [ 0x06uy; 0x00uy; byte r0; byte r1; ]
        | Modu(r0, r1)   -> [ 0x07uy; 0x00uy; byte r0; byte r1; ]
        | Add(r0, r1)    -> [ 0x08uy; 0x00uy; byte r0; byte r1; ]
        | Sub(r0, r1)    -> [ 0x09uy; 0x00uy; byte r0; byte r1; ]
        | Mov(r0, r1)    -> [ 0x0Auy; 0x00uy; byte r0; byte r1; ]
        | And(r0, r1)    -> [ 0x0Buy; 0x00uy; byte r0; byte r1; ]
        | Or(r0, r1)     -> [ 0x0Cuy; 0x00uy; byte r0; byte r1; ]
        | Xor(r0, r1)    -> [ 0x0Duy; 0x00uy; byte r0; byte r1; ]
        | Ceq(r0, r1)    -> [ 0x0Euy; 0x00uy; byte r0; byte r1; ]
        | Cne(r0, r1)    -> [ 0x0Fuy; 0x00uy; byte r0; byte r1; ]
        | Clt(r0, r1)    -> [ 0x10uy; 0x00uy; byte r0; byte r1; ]
        | Cle(r0, r1)    -> [ 0x11uy; 0x00uy; byte r0; byte r1; ]
        | Cgt(r0, r1)    -> [ 0x12uy; 0x00uy; byte r0; byte r1; ]
        | Cge(r0, r1)    -> [ 0x13uy; 0x00uy; byte r0; byte r1; ]
        | Cltu(r0, r1)   -> [ 0x14uy; 0x00uy; byte r0; byte r1; ]
        | Cleu(r0, r1)   -> [ 0x15uy; 0x00uy; byte r0; byte r1; ]
        | Cgtu(r0, r1)   -> [ 0x16uy; 0x00uy; byte r0; byte r1; ]
        | Cgeu(r0, r1)   -> [ 0x17uy; 0x00uy; byte r0; byte r1; ]
        | Cltf(r0, r1)   -> [ 0x18uy; 0x00uy; byte r0; byte r1; ]
        | Clef(r0, r1)   -> [ 0x19uy; 0x00uy; byte r0; byte r1; ]
        | Cgtf(r0, r1)   -> [ 0x1Auy; 0x00uy; byte r0; byte r1; ]
        | Cgef(r0, r1)   -> [ 0x1Buy; 0x00uy; byte r0; byte r1; ]
        | Mulf(r0, r1)   -> [ 0x1Cuy; 0x00uy; byte r0; byte r1; ]
        | Divf(r0, r1)   -> [ 0x1Duy; 0x00uy; byte r0; byte r1; ]
        | Addf(r0, r1)   -> [ 0x1Euy; 0x00uy; byte r0; byte r1; ]
        | Subf(r0, r1)   -> [ 0x1Fuy; 0x00uy; byte r0; byte r1; ]
        | Cvtfw(r0)      -> [ 0x20uy; 0x00uy; byte r0; 0x00uy;  ]
        | Cvtwf(r0)      -> [ 0x21uy; 0x00uy; byte r0; 0x00uy;  ]
        | Alloc(r0)      -> [ 0x22uy; 0x00uy; byte r0; 0x00uy;  ]
        | Free(r0)       -> [ 0x23uy; 0x00uy; byte r0; 0x00uy;  ]
        | Push(r0)       -> [ 0x24uy; 0x00uy; byte r0; 0x00uy;  ]
        | Pop(r0)        -> [ 0x25uy; 0x00uy; byte r0; 0x00uy;  ]
        | Not(r0)        -> [ 0x26uy; 0x00uy; byte r0; 0x00uy;  ]
        | Ret(r0)        -> [ 0x27uy; 0x00uy; byte r0; 0x00uy;  ]
        | Halt(r0)       -> [ 0x28uy; 0x00uy; byte r0; 0x00uy;  ]
        | ImmAdd(r0, i)  -> List.concat [ [ 0x29uy; 0x00uy; byte r0; 0x00uy ]; i32_to_bytes (eval i) ]
        | Ldw(r0, i)     -> List.concat [ [ 0x2Auy; 0x00uy; byte r0; 0x00uy ]; i32_to_bytes (eval i) ]
        | Ldb(r0, i)     -> List.concat [ [ 0x2Buy; 0x00uy; byte r0; 0x00uy ]; i32_to_bytes (eval i) ]
        | Stw(r0, i)     -> List.concat [ [ 0x2Cuy; 0x00uy; byte r0; 0x00uy ]; i32_to_bytes (eval i) ]
        | Stb(r0, i)     -> List.concat [ [ 0x2Duy; 0x00uy; byte r0; 0x00uy ]; i32_to_bytes (eval i) ]
        | Lea(r0, i)     -> List.concat [ [ 0x2Euy; 0x00uy; byte r0; 0x00uy ]; i32_to_bytes (eval i) ]
        | ImmSll(r0, i)  -> List.concat [ [ 0x2Fuy; 0x00uy; byte r0; 0x00uy ]; i32_to_bytes (eval i) ]
        | Jmp(i)         -> List.concat [ [ 0x3Auy; 0x00uy; 0x00uy; 0x00uy ]; i32_to_bytes (eval i) ]
        | Je(i)          -> List.concat [ [ 0x3Buy; 0x00uy; 0x00uy; 0x00uy ]; i32_to_bytes (eval i) ]
        | Jne(i)         -> List.concat [ [ 0x3Cuy; 0x00uy; 0x00uy; 0x00uy ]; i32_to_bytes (eval i) ]
        | ImmPush(i)     -> List.concat [ [ 0x3Duy; 0x00uy; 0x00uy; 0x00uy ]; i32_to_bytes (eval i) ]
        | Call(i)        -> List.concat [ [ 0x3Euy; 0x00uy; 0x00uy; 0x00uy ]; i32_to_bytes (eval i) ]
        | Calx(i)        -> List.concat [ [ 0x3Fuy; 0x00uy; 0x00uy; 0x00uy ]; i32_to_bytes (eval i) ]
        | Lab(_)         -> [ ]

    member x.GetSize() =
        match x with
        | Lab(_) -> 0
        | ImmAdd(_)
        | Ldw(_)
        | Ldb(_)
        | Stw(_)
        | Stb(_)
        | Lea(_)
        | ImmSll(_)
        | Jmp(_)
        | Je(_)
        | Jne(_)
        | ImmPush(_)
        | Call(_)
        | Calx(_) -> 2
        | _ -> 1


type Assembly =
    {
        mutable Header : byte list
        mutable Text   : byte list
        mutable Data   : byte list
    }

let emit (bytes, section: byte list byref, pos: int byref) =
    section <- section @ bytes
    pos <- pos + (bytes.Length >>> 2)

let assemble (bytecode: Bytecode) =
    let mutable context = new Dictionary<Label, int>()
    let mutable pos = 0

    let assembly = { Header = []; Text = []; Data = [] }

    let add_label l =
        context.Add(l, pos)

    // Assembly header data
    // For now just offset the ip with its size
    pos <- Header.Size

    let import_ptr = pos

    // The import table constains procedures defined in the
    // abstract machine host for extern calls.
    bytecode.ImportTable |> List.iter
        (fun proc ->
            add_label proc.Label

            let name = str_to_utf32 proc.Name
            emit(i32_to_bytes(name.Length >>> 2), &assembly.Header, &pos)
            emit(name, &assembly.Header, &pos))

    let export_ptr = pos

    // The export table constains procedures that can be
    // called from the host application.
    // Format:
    //      str_length (word)
    //      proc_name
    //      ptr to text section
    //      ...
    bytecode.ExportTable |> List.iter
        (fun proc ->
            let name = str_to_utf32 proc.Name
            emit(i32_to_bytes(name.Length >>> 2), &assembly.Header, &pos)
            emit(name, &assembly.Header, &pos)
            emit(i32_to_bytes proc.Pointer, &assembly.Header, &pos))

    // pos currently points to the start of the text section
    let text_ptr = pos

    // The text section constains all instructions
    // For now just use calculate its size
    bytecode.Text |> List.iter (fun opcode -> pos <- pos + opcode.GetSize())

    let const_ptr = pos

    let emit_data (data: Data list) =
        data |> List.iter
            (fun d ->
                match d.Label with
                | Some(l) -> add_label l
                | None -> ()

                emit(d.Value, &assembly.Data, &pos))

    emit_data bytecode.Const

    let dat_ptr = pos
    emit_data bytecode.Data

    // Rewind to emit text section,
    // now that all labels have been resolved
    pos <- text_ptr

    bytecode.Text |> List.iter
        (fun opcode ->
            match opcode with
            | Lab(l) -> add_label l
            | _ -> emit(opcode.ToBytes(context), &assembly.Text, &pos))

    let header =
        {
            Signature = ((int16 ':' <<< 8) ||| int16 ')')
            Version   = (0s <<< 8) ||| 2s
            StackSize = 32
            HeapSize = 32
            TextPtr = text_ptr
            ConstPtr = const_ptr
            DatPtr = dat_ptr
            ImportTablePtr = import_ptr
            ExportTablePtr = export_ptr
        }

    printfn "%A" header

    pos <- 0
    emit(header.ToBytes(), &assembly.Header, &pos)

    Seq.toArray (List.concat [ assembly.Header; assembly.Text; assembly.Data ] )

let dump (bytecode: OpCode list) =
    let buf = new StringBuilder()

    let reg =
        function
        | Register.EAX -> "eax"
        | Register.EDX -> "edx"
        | Register.EBP -> "ebp"
        | Register.ESP -> "esp"
        | _ -> "Unknown Register"

    let dump_opcode =
        let s_imm =
            function
            | Value(v) -> sprintf "0x%X" v
            | Label(l) -> l

        function
        | Sll(r0, r1)   -> sprintf "\tsll\t%s, %s" (reg r0) (reg r1)
        | Srl(r0, r1)   -> sprintf "\tsrl\t%s, %s" (reg r0) (reg r1)
        | Srlu(r0, r1)  -> sprintf "\tsrlu\t%s, %s" (reg r0) (reg r1)
        | Mul(r0, r1)   -> sprintf "\tmul\t%s, %s" (reg r0) (reg r1)
        | Div(r0, r1)   -> sprintf "\tdiv\t%s, %s" (reg r0) (reg r1)
        | Divu(r0, r1)  -> sprintf "\tdivu\t%s, %s" (reg r0) (reg r1)
        | Mod(r0, r1)   -> sprintf "\tmod\t%s, %s" (reg r0) (reg r1)
        | Modu(r0, r1)  -> sprintf "\tmodu\t%s, %s" (reg r0) (reg r1)
        | Add(r0, r1)   -> sprintf "\tadd\t%s, %s" (reg r0) (reg r1)
        | Sub(r0, r1)   -> sprintf "\tsub\t%s, %s" (reg r0) (reg r1)
        | Mov(r0, r1)   -> sprintf "\tmov\t%s, %s" (reg r0) (reg r1)
        | And(r0, r1)   -> sprintf "\tand\t%s, %s" (reg r0) (reg r1)
        | Or(r0, r1)    -> sprintf "\tor\t%s, %s" (reg r0) (reg r1)
        | Xor(r0, r1)   -> sprintf "\txor\t%s, %s" (reg r0) (reg r1)
        | Ceq(r0, r1)   -> sprintf "\tceq\t%s, %s" (reg r0) (reg r1)
        | Cne(r0, r1)   -> sprintf "\tcne\t%s, %s" (reg r0) (reg r1)
        | Clt(r0, r1)   -> sprintf "\tclt\t%s, %s" (reg r0) (reg r1)
        | Cle(r0, r1)   -> sprintf "\tcle\t%s, %s" (reg r0) (reg r1)
        | Cgt(r0, r1)   -> sprintf "\tcgt\t%s, %s" (reg r0) (reg r1)
        | Cge(r0, r1)   -> sprintf "\tcge\t%s, %s" (reg r0) (reg r1)
        | Cltu(r0, r1)  -> sprintf "\tclu\t%s, %s" (reg r0) (reg r1)
        | Cleu(r0, r1)  -> sprintf "\tcleu\t%s, %s" (reg r0) (reg r1)
        | Cgtu(r0, r1)  -> sprintf "\tcgtu\t%s, %s" (reg r0) (reg r1)
        | Cgeu(r0, r1)  -> sprintf "\tcgeu\t%s, %s" (reg r0) (reg r1)
        | Cltf(r0, r1)  -> sprintf "\tcltf\t%s, %s" (reg r0) (reg r1)
        | Clef(r0, r1)  -> sprintf "\tclef\t%s, %s" (reg r0) (reg r1)
        | Cgtf(r0, r1)  -> sprintf "\tcgtf\t%s, %s" (reg r0) (reg r1)
        | Cgef(r0, r1)  -> sprintf "\tcgef\t%s, %s" (reg r0) (reg r1)
        | Mulf(r0, r1)  -> sprintf "\tmulf\t%s, %s" (reg r0) (reg r1)
        | Divf(r0, r1)  -> sprintf "\tdivf\t%s, %s" (reg r0) (reg r1)
        | Addf(r0, r1)  -> sprintf "\taddf\t%s, %s" (reg r0) (reg r1)
        | Subf(r0, r1)  -> sprintf "\tsubf\t%s, %s" (reg r0) (reg r1)
        | Cvtfw(r0)     -> sprintf "\tcvtfw\t%s" (reg r0)
        | Cvtwf(r0)     -> sprintf "\tcvtwf\t%s" (reg r0)
        | Alloc(r0)     -> sprintf "\talloc\t%s" (reg r0)
        | Free(r0)      -> sprintf "\tfree\t%s" (reg r0)
        | Push(r0)      -> sprintf "\tpush\t%s" (reg r0)
        | Pop(r0)       -> sprintf "\tpop\t%s" (reg r0)
        | Not(r0)       -> sprintf "\tnot\t%s" (reg r0)
        | Ret(r0)       -> sprintf "\tret\t%s" (reg r0)
        | Halt(r0)      -> sprintf "\thalt\t%s" (reg r0)
        | ImmAdd(r0, i) -> sprintf "\tadd\t%s, %s" (reg r0) (s_imm i)
        | Ldw(r0, i)    -> sprintf "\tldw\t%s, %s" (reg r0) (s_imm i)
        | Ldb(r0, i)    -> sprintf "\tldb\t%s, %s" (reg r0) (s_imm i)
        | Stw(r0, i)    -> sprintf "\tstw\t%s, %s" (reg r0) (s_imm i)
        | Stb(r0, i)    -> sprintf "\tstb\t%s, %s" (reg r0) (s_imm i)
        | Lea(r0, i)    -> sprintf "\tlea\t%s, %s" (reg r0) (s_imm i)
        | ImmSll(r0, i) -> sprintf "\tsll\t%s, %s" (reg r0) (s_imm i)
        | Jmp(i)        -> sprintf "\tjmp\t%s" (s_imm i)
        | Je(i)         -> sprintf "\tje\t%s" (s_imm i)
        | Jne(i)        -> sprintf "\tjne\t%s" (s_imm i)
        | ImmPush(i)    -> sprintf "\tpush\t%s" (s_imm i)
        | Call(i)       -> sprintf "\tcall\t%s" (s_imm i)
        | Calx(i)       -> sprintf "\tcalx\t%s" (s_imm i)
        | Lab(l)        -> sprintf "%s:" l

    buf.AppendLine(".text") |> ignore

    bytecode |> List.iter
        (fun x ->
            buf.AppendLine(dump_opcode x) |> ignore
            ())


    buf.ToString()
