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
    let tac = IRCompiler.compile(program, sem_analysis)

    sw.Stop()
    printfn "compiled %s in %.2f seconds" args.[0] (sw.Elapsed.TotalSeconds)

    // @Temporary: handling command line options to print info
    let handle_option opt =
        match opt with
        | "-d" -> print_debug_info program sem_analysis
        | "-sl" ->
            printfn "Symbol lookup. ^C to exit."
            while true do
                printf "symbols> "
                let search = System.Console.ReadLine()

                if search <> "" then
                    printfn "%A" (debug_symbol_search sem_analysis search)
        | "-ast" -> printfn "%A" program
        | "-irl" -> printfn "%A" tac
        | "-ir"  -> tac |> IRCompiler.tac_to_text |> printfn "%s"
        | _ -> printfn "Unknown command line option %s." opt

    Seq.skip 1 args |> Seq.toList |> List.iter handle_option
    0
