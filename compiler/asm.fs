///
/// Ava Assembler
///

module Asm

open System.Text
open System.Collections.Generic

let assembler_signature = ((int16 ':' <<< 8) ||| int16 '(')

let assembly_version = (2s <<< 8) ||| 0s


module Encode =
    let str_to_utf32 (str: string) =
        Array.toList (Encoding.UTF32.GetBytes(str))

    let str_to_utf8 (str: string) =
        Array.toList (Encoding.UTF8.GetBytes(str))

    let i32_to_bytes i =
        [ byte(i &&& 0xff);
           byte((i &&& 0xff00) >>> 8);
           byte((i &&& 0xff0000) >>> 16);
           byte((i &&& 0xff000000) >>> 24); ]

    let i16_to_bytes (i: int16) =
        [ byte(i &&& 0xffs);
          byte((i &&& 0xff00s) >>> 8);]

    let bytes_to_int (bytes: byte list) =
        let mutable res = int bytes.Head
        bytes.Tail |> List.iteri (fun i x -> res <- res ||| (int x <<< 8 * (i + 1)))
        res

type Bytecode =
    {
        ImportTable : ProcTableEntry list
        ExportTable : ProcTableEntry list
        Text        : OpCode list
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

and ProcTableEntry =
    {
        Name  : string
        Label : Label
    }

and Header =
    {
        Signature : int16
        Version   : int16
        StackSize : int32
        HeapSize  : int32
        TextPtr   : int32
        DatPtr    : int32
        ImportTablePtr : int32
        ExportTablePtr : int32
    }

    static member Size = 32 >>> 2

    member x.ToBytes() =
        List.concat
            [ Encode.i16_to_bytes x.Signature;
              Encode.i16_to_bytes x.Version;
              Encode.i32_to_bytes x.StackSize;
              Encode.i32_to_bytes x.HeapSize;
              Encode.i32_to_bytes x.ImportTablePtr;
              Encode.i32_to_bytes x.ExportTablePtr;
              Encode.i32_to_bytes x.TextPtr;
              Encode.i32_to_bytes x.DatPtr;
              Encode.i32_to_bytes 0x03030303 ] // Reserved

and Register =
    | EAX = 0x00
    | EDX = 0x01
    | ESP = 0x02
    | EBP = 0x03

and Section =
    | TEXT   = 0x01
    | IMPORT = 0x02
    | EXPORT = 0x03
    | DATA   = 0x04

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
    | Neg      of Register
    | Negf     of Register
    | Ret      of Register
    | Halt     of Register
    | ImmAdd   of Register * Immediate
    | Ldw      of Register * Immediate
    | Stw      of Register * Immediate
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

        let i32 = Encode.i32_to_bytes

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
        | Neg(r0)        -> [ 0x27uy; 0x00uy; byte r0; 0x00uy;  ]
        | Negf(r0)       -> [ 0x28uy; 0x00uy; byte r0; 0x00uy;  ]
        | Ret(r0)        -> [ 0x29uy; 0x00uy; byte r0; 0x00uy;  ]
        | Halt(r0)       -> [ 0x2Auy; 0x00uy; byte r0; 0x00uy;  ]
        | Ldw(r0, i)     -> List.concat [ [ 0x2Duy; 0x00uy; byte r0; 0x00uy ]; i32 (eval i) ]
        | Lea(r0, i)     -> List.concat [ [ 0x2Euy; 0x00uy; byte r0; 0x00uy ]; i32 (eval i) ]
        | Stw(r0, i)     -> List.concat [ [ 0x2Fuy; 0x00uy; byte r0; 0x00uy ]; i32 (eval i) ]
        | ImmAdd(r0, i)  -> List.concat [ [ 0x30uy; 0x00uy; byte r0; 0x00uy ]; i32 (eval i) ]
        | ImmSll(r0, i)  -> List.concat [ [ 0x31uy; 0x00uy; byte r0; 0x00uy ]; i32 (eval i) ]
        | Jmp(i)         -> List.concat [ [ 0x3Auy; 0x00uy; 0x00uy; 0x00uy ]; i32 (eval i) ]
        | Je(i)          -> List.concat [ [ 0x3Buy; 0x00uy; 0x00uy; 0x00uy ]; i32 (eval i) ]
        | Jne(i)         -> List.concat [ [ 0x3Cuy; 0x00uy; 0x00uy; 0x00uy ]; i32 (eval i) ]
        | ImmPush(i)     -> List.concat [ [ 0x3Duy; 0x00uy; 0x00uy; 0x00uy ]; i32 (eval i) ]
        | Call(i)        -> List.concat [ [ 0x3Euy; 0x00uy; 0x00uy; 0x00uy ]; i32 (eval i) ]
        | Calx(i)        -> List.concat [ [ 0x3Fuy; 0x00uy; 0x00uy; 0x00uy ]; i32 (eval i) ]
        | Lab(_)         -> [ ]

    member x.GetSize() =
        match x with
        | Lab(_) -> 0
        | ImmAdd(_)
        | Ldw(_)
        | Stw(_)
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
            let name = Encode.str_to_utf32 proc.Name
            emit(Encode.i32_to_bytes(name.Length >>> 2), &assembly.Header, &pos)
            add_label proc.Label
            emit(name, &assembly.Header, &pos))

    let export_ptr = pos

    // The export table constains procedures that can be
    // called from the host application.
    // Format:
    //      str_length (word)
    //      proc_name (utf32)
    //      ptr to text section (word)
    //      ...
    // We cannot resolve the labels yet, so we just calculate
    // the size of the header.
    bytecode.ExportTable |> List.iter (fun proc ->
        pos <- pos + proc.Name.Length + 2)

    // pos currently points to the start of the text section
    let text_ptr = pos
    pos <- 0

    // The text section constains all instructions
    // For now just use calculate its size
    bytecode.Text |> List.iter
        (fun opcode ->
            match opcode with
            | Lab(l) -> add_label l
            | _ -> pos <- pos + opcode.GetSize())

    let dat_ptr = text_ptr + pos

    bytecode.Data |> List.iter
        (fun d ->
            match d.Label with
            | Some(l) -> add_label l
            | None -> ()
            pos <- pos + (d.Value.Length >>> 2))

    pos <- export_ptr // Rewind

    bytecode.ExportTable |> List.iter
        (fun proc ->
            let name = Encode.str_to_utf32 proc.Name
            emit(Encode.i32_to_bytes(name.Length >>> 2), &assembly.Header, &pos)
            emit(name, &assembly.Header, &pos)
            emit(Encode.i32_to_bytes context.[proc.Label], &assembly.Header, &pos))

    pos <- dat_ptr

    bytecode.Data |> List.iter (fun d -> emit(d.Value, &assembly.Data, &pos))

    // Rewind to emit text section,
    // now that all labels have been resolved
    pos <- text_ptr

    bytecode.Text |> List.iter (fun op -> emit(op.ToBytes(context), &assembly.Text, &pos))

    let header =
        {
            Signature = assembler_signature
            Version   = assembly_version
            StackSize = 4096
            HeapSize = 4096
            TextPtr = (text_ptr <<< 2)
            DatPtr = (dat_ptr <<< 2)
            ImportTablePtr = (import_ptr <<< 2)
            ExportTablePtr = (export_ptr <<< 2)
        }

    assembly.Header <- header.ToBytes() @ assembly.Header
    assembly

let concat(assembly) =
    Seq.toArray (List.concat [ assembly.Header; assembly.Text; assembly.Data ] )

let dump bytecode (assembly: Assembly option) =
    let buf = StringBuilder()

    let append (s: string) =
        buf.Append(s) |> ignore

    let reg =
        function
        | Register.EAX -> "eax"
        | Register.EDX -> "edx"
        | Register.EBP -> "ebp"
        | Register.ESP -> "esp"
        | _ -> "Unknown Register"

    let hexi n =
        if n = 0 then "0h"
        else sprintf "0%Xh" n

    let hexb n =
        if n = 0uy then "0h"
        else sprintf "0%Xh" n

    let dump_bytes (bytes: byte list) =
        append(hexb bytes.Head)
        bytes.Tail |> List.iter (fun x -> append(", "); append(hexb x))
        append "\n"

    let dump_opcode =
        let s_imm =
            function
            | Value(v) -> hexi v
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
        | Neg(r0)       -> sprintf "\tneg\t%s" (reg r0)
        | Negf(r0)      -> sprintf "\tnegf\t%s" (reg r0)
        | Ret(r0)       -> sprintf "\tret\t%s" (reg r0)
        | Halt(r0)      -> sprintf "\thalt\t%s" (reg r0)
        | ImmAdd(r0, i) -> sprintf "\tadd\t%s, %s" (reg r0) (s_imm i)
        | Ldw(r0, i)    -> sprintf "\tldw\t%s, %s" (reg r0) (s_imm i)
        | Stw(r0, i)    -> sprintf "\tstw\t%s, %s" (reg r0) (s_imm i)
        | Lea(r0, i)    -> sprintf "\tlea\t%s, %s" (reg r0) (s_imm i)
        | ImmSll(r0, i) -> sprintf "\tsll\t%s, %s" (reg r0) (s_imm i)
        | Jmp(i)        -> sprintf "\tjmp\t%s" (s_imm i)
        | Je(i)         -> sprintf "\tje\t%s" (s_imm i)
        | Jne(i)        -> sprintf "\tjne\t%s" (s_imm i)
        | ImmPush(i)    -> sprintf "\tpush\t%s" (s_imm i)
        | Call(i)       -> sprintf "\tcall\t%s" (s_imm i)
        | Calx(i)       -> sprintf "\tcalx\t%s" (s_imm i)
        | Lab(l)        -> sprintf "%s:" l

    match assembly with
    | Some(a) ->
        let e = Encode.bytes_to_int
        let signature = sprintf "%c%c" (char (e a.Header.[0..0])) (char (e a.Header.[1..1]))

        append(";;\t\t\t\t;;\n")
        append(sprintf ";; Ava Assembly (%i bytes)\t;;\n;;\t\t\t\t;;\n" (concat a).Length)
        append(sprintf ";;\tsignature\t%s\t;;\n" signature)
        append(sprintf ";;\tversion\t\t%i.%i\t;;\n" (e a.Header.[2..2]) (e a.Header.[3..3]))
        append(sprintf ";;\tstack_size\t%X\t;;\n" (e a.Header.[4..7]))
        append(sprintf ";;\theap_size\t%X\t;;\n" (e a.Header.[8..11]))
        append(sprintf ";;\timport_table\t%X\t;;\n" (e a.Header.[12..15]))
        append(sprintf ";;\texport_table\t%X\t;;\n" (e a.Header.[16..19]))
        append(sprintf ";;\ttext_ptr\t%X\t;;\n" (e a.Header.[20..23]))
        append(sprintf ";;\tdata_ptr\t%X\t;;\n" (e a.Header.[24..27]))
        append(";;\t\t\t\t;;\n")
    | None -> ()

    append("\n.import\n")
    bytecode.ImportTable |> List.iter
        (fun p ->
            let name = Encode.str_to_utf32 p.Name
            append("\t.word\t")
            append(hexi p.Name.Length)
            append("\n")
            append(p.Label)
            append(":\t.byte\t")
            dump_bytes name)

    append("\n.export\n")
    bytecode.ExportTable |> List.iter
        (fun p ->
            let name = Encode.str_to_utf32 p.Name
            append("\t.word\t")
            append(hexi p.Name.Length)
            append("\n")
            append("\t.byte\t")
            dump_bytes name
            append("\t.word\t")
            append(p.Label)
            append("\n"))

    append("\n.text\n")
    bytecode.Text |> List.iter (fun x -> append(dump_opcode x); append("\n"))

    append("\n.data\n")
    bytecode.Data |> List.iter
        (fun d ->
            match d.Label with
            | Some(l) -> append(l + ":\t.byte\t")
            | None -> append("\t.byte\t")
            dump_bytes d.Value)

    buf.ToString()
