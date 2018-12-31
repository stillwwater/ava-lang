{
module Lexer

open Microsoft.FSharp.Text.Lexing
open Ast
open Parser
open System
open System.Text

let str_buffer = new StringBuilder()

let newline (lexbuf: LexBuffer<_>) =
    // Update newline position informations
    lexbuf.StartPos <- lexbuf.StartPos.NextLine

let lexeme lexbuf =
    LexBuffer<char>.LexemeString lexbuf
}

let digit = ['0'-'9']

let whitespace = [' ' '\t']
let newline = ('\n' | '\r' '\n')

rule tokenstream =
    parse
    | [' ''\t']+  { tokenstream lexbuf } // Skip whitespace @Todo: mark pos
    | "--"        { comment lexbuf }
    | newline+    { EOL }           // Register new line
    // Literals
    | ['-']?digit+('.'digit+)   { FLOAT_LITERAL(float32(lexeme lexbuf)) }
    | ['-']?digit+              { INT_LITERAL(int(lexeme lexbuf)) }
    //| digit+['b']               { BYTE_LITERAL(byte(lexeme lexbuf)) }
    // @Todo hex
    | '"' {
        str_buffer.Clear() |> ignore
        read_str false lexbuf
    }
    // Todo single quotes
    // Keywords
    | "void"     { KW_VOID }
    | "int"      { KW_INT }
    | "float"    { KW_FLOAT }
    | "byte"     { KW_BYTE }
    | "char"     { KW_CHAR }
    | "str"      { KW_STRING }
    | "text"     { KW_TEXT}
    | "or"       { KW_OR }
    | "and"      { KW_AND }
    | "not"      { KW_NOT }
    | "if"       { KW_IF }
    | "else"     { KW_ELSE }
    | "while"    { KW_WHILE }
    | "then"     { KW_THEN }
    | "do"       { KW_DO }
    | "end"      { KW_END }
    | "of"       { KW_OF }
    | "is"       { KW_IS }
    | "return"   { KW_RETURN }
    | "break"    { KW_BREAK }
    | "continue" { KW_CONTINUE }
    | "export"   { KW_EXPORT }
    // Native procedures
    | "count"    { KW_COUNT }
    | ['a'-'z' '_']+ { IDENT(lexeme lexbuf) }
    // Symbols
    | ","        { COMMA }
    | ";"        { EOL }
    | ":"        { COLON }
    | "."        { DOT }
    | "="        { SINGLE_EQUALS }
    | "::"       { DOUBLE_COLON }
    | ":="       { COLON_EQUALS }
    | "->"       { RARROW }
    | "+"        { PLUS }
    | "-"        { MINUS }
    | "*"        { ASTERISK }
    | "/"        { FSLASH }
    | "%"        { PERCENT }
    | "=="       { DOUBLE_EQUALS }
    | "<>"       { LESS_GREATER }
    | "<="       { LESS_EQUALS }
    | "<"        { LESS }
    | ">="       { GREATER_EQUALS }
    | ">"        { GREATER }
    | "&"        { AMP }
    | "("        { LPAREN }
    | ")"        { RPAREN }
    | "["        { LBRACKET }
    | "]"        { RBRACKET }
    | "#"        { HASH }
    | eof        { EOF }

and comment =
    parse
    | newline         { tokenstream lexbuf |> ignore; EOL } // Keep EOL as it may be required by the parser
    | _               { comment lexbuf }                // Skip comment chars

and read_str ignorequote =
    parse
    | '"' {
        if ignorequote then
            str_buffer.Append("\"") |> ignore
            read_str false lexbuf
        else
            // End of string
            STRING_LITERAL(str_buffer.ToString())
    }
    | '\\'    { read_str true lexbuf }
    | [^ '"' '\\']+ {
        str_buffer.Append(lexeme lexbuf) |> ignore
        read_str false lexbuf
    }
    // @Todo: raise error at EOF
