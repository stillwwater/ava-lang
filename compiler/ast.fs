module Ast
open System.Linq.Expressions

type Program = Declaration list

and Declaration =
    | VariableDecl of VariableDecl
    | ConstantDecl of ConstantDecl
    | ProcedureDecl of ProcedureDecl
    | DeclNop

and VariableDecl =
    | ScalarVariableDecl of Identifier * TypeSpec * Expression option
    | ArrayVariableDecl of Identifier * TypeSpec
    | FixedArrayVariableDecl of Identifier * int * TypeSpec

and ConstantDecl =
    | ScalarConstantDecl of Identifier * Literal

and ProcedureDecl =
    | InternalProcedureDecl of Identifier * Parameters * TypeSpec * CompoundStatement
    | PublicProcedureDecl of Identifier * Parameters * TypeSpec * CompoundStatement

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
and IfStatement = Expression * Statement list * Statement list option

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
    //| ArrayDeclExpression of TypeSpec * Expression

and BinaryOperaror =
    | Eq
    | NotEq
    | LtEq
    | Lt
    | GtEq
    | Gt
    | CondAnd
    | CondOr
    | Add
    | Sub
    | Mul
    | Div
    | Mod

and UnaryOperator =
    | LogicalNot
    | Negate
    | Identity

and Literal =
    | IntLiteral of int
    | CharLiteral of char
    | FloatLiteral of float32
    | ByteLiteral of byte
    | StringLiteral of string
    | TextLiteral of string
