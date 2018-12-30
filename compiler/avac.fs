// Learn more about F# at http://fsharp.org
open Ast
open System.IO
open Microsoft.FSharp.Text.Lexing

let parse (text: string) =
    let lexbuf = LexBuffer<char>.FromString text
    Parser.start Lexer.tokenstream lexbuf

let parsefile (filename: string) =
    use reader = new StreamReader(filename)
    let lexbuf = LexBuffer<char>.FromTextReader reader

    let stream = Lexer.tokenstream
    Parser.start stream lexbuf

let test = Path.Combine(__SOURCE_DIRECTORY__, "test.ava")
let program = parsefile test

printfn "%A" program
printfn "The end"
