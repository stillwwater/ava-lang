module Ast

open System

[<Flags>]
type ProcFlags =
    // @Todo: These flags should apply to any declaration
    // DeclFlgs * VariableDecl
    | INTERNAL = 0b0001
    | PUBLIC   = 0b0010

[<Flags>]
type ScalarFlags =
    | NONE     = 0b0001
    | STATIC   = 0b0010
    | CONSTANT = 0b0100
    | SERIAL   = 0b1000

[<Flags>]
type ArrayFlags =
    | NONE     = 0b0001
    | STATIC   = 0b0010
    | FIXED    = 0b0100
    | SERIAL   = 0b1000

type Program = Declaration list

and Declaration =
    | VariableDecl of VariableDecl
    | DeclNop

and VariableDecl =
    | ScalarDecl of ScalarFlags * IdentifierRef * TypeSpec option * Expression option
    | ArrayDecl of ArrayFlags * IdentifierRef * Expression option * TypeSpec option * Aggregate option
    | ProcedureDecl of ProcFlags * IdentifierRef * Parameters * TypeSpec * CompoundStatement

and TypeSpec =
    | Void   // 1 cell
    | Int    // 1 cell
    | Float  // 1 cell
    | Byte   // 1/sizeof(cell) cells
    | Char   // Same as int
    | Text   // Same as [] of byte
    | String // Same as [] of char

and Identifier = string

and Parameters = VariableDecl list

and Aggregate = Expression list

and IdentifierRef = { Identifier : string; }

and Statement =
    | Declaration of Declaration
    | SExpression of SExpression
    | CompoundStatement of CompoundStatement
    | IfStatement of IfStatement
    | WhileStatement of WhileStatement
    | ReturnStatement of Expression option
    | BreakStatement
    | ContinueStatement

// An Expression evaluates something while a statement does something
// An Expression statement is a statement composed of an Expression
and SExpression =
    | Expression of Expression
    | Nop

and CompoundStatement = Statement list

and LocalDeclarations = VariableDecl list

and Arguments = Expression list

// @Todo: Elsif
and IfStatement = ConditionalClause list * Statement list option

and ConditionalClause = Expression * Statement list

and WhileStatement = Expression * Statement

and Expression =
    | ScalarAssignExpression of IdentifierRef * Expression
    | ArrayAssignExpression of IdentifierRef * Expression * Expression
    | BinaryExpression of Expression * BinaryOperaror * Expression
    | UnaryExpression of UnaryOperator * Expression
    | IdentifierExpression of IdentifierRef
    | ArrayIdentifierExpression of IdentifierRef * Expression
    | ProcedureCallExpression of IdentifierRef * Arguments
    | ArrayCountExpression of IdentifierRef
    // @Todo: Array size vs list count
    // @Todo: Sizeof
    | LiteralExpression of Literal

and BinaryOperaror =
    | Eq
    | NotEq
    | LtEq
    | Lt
    | GtEq
    | Gt
    | CondAnd
    | CondOr
    | Xor
    | Add
    | Sub
    | Mul
    | Div
    | Mod

and UnaryOperator =
    | LogicalNot
    | BitwiseNot
    | Negate
    | Identity

and Literal =
    | IntLiteral of int
    | CharLiteral of char
    | FloatLiteral of float32
    | ByteLiteral of byte
    | StringLiteral of string
    | TextLiteral of string
