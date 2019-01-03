open SemanticAnalysis
open System.IO
open System.Diagnostics
open Microsoft.FSharp.Text.Lexing

let parse (text: string) =
    let lexbuf = LexBuffer<char>.FromString text
    Parser.start Lexer.tokenstream lexbuf

let parsefile (filename: string) =
    use reader = new StreamReader(filename)
    let lexbuf = LexBuffer<char>.FromTextReader reader

    let stream = Lexer.tokenstream
    Parser.start stream lexbuf

let print_debug_info program sem_analysis =
    printfn "%A" program

    let print_dict dict =
        for kv in dict do
            printfn "%A" kv

    print_dict sem_analysis.ExpressionTable
    print_dict sem_analysis.ProcedureTable
    print_dict sem_analysis.SymbolTable

[<EntryPoint>]
let main(args) =
    if args.Length = 0 then
        printfn "missing input!"

    let sw = Stopwatch.StartNew()

    let program = parsefile args.[0]
    let sem_analysis = SemanticAnalysis.analyse program

    sw.Stop()

    if args.Length > 1 && args.[1] = "-d" then
        print_debug_info program sem_analysis

    printfn "compiled %s in %.2f seconds" args.[0] (sw.Elapsed.TotalSeconds)
    0
