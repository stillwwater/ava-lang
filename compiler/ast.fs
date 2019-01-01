module Ast

type Program = Declaration list

and Declaration =
    | VariableDecl of VariableDecl
    | DeclNop

and VariableDecl =
    | ScalarVariableDecl of Identifier * TypeSpec option * Expression option
    | ArrayVariableDecl of Identifier * Expression option * TypeSpec option * Aggregate option
    // A fixed scalar is constant in value and is allocated in the .data section
    | FixedScalarDecl of Identifier * Literal
    // A fixed array is fixed in size and is allocated in the .data section
    | FixedArrayVariableDecl of Identifier * int * TypeSpec
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
    | Negate
    | Identity

and Literal =
    | IntLiteral of int
    | CharLiteral of char
    | FloatLiteral of float32
    | ByteLiteral of byte
    | StringLiteral of string
    | TextLiteral of string
