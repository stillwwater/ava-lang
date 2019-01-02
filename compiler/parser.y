%{
open Ast

let mutable syntax_error = false

let yyerror (msg: string) = 
    syntax_error <- true
    printfn "syntax error: %s" msg
%}

// Start function
%start start

// Terminal Tokens

%token <int> INT_LITERAL
%token <float32> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <byte> BYTE_LITERAL
%token <string> IDENT

%token KW_FLOAT
%token KW_VOID
%token KW_INT
%token KW_BYTE
%token KW_CHAR
%token KW_STRING
%token KW_TEXT

%token COMMA
%token SEMI_COLON
%token COLON
%token DOT
%token HASH

%token SINGLE_EQUALS
%token DOUBLE_COLON
%token COLON_EQUALS

%token PLUS MINUS
%token ASTERISK FSLASH
%token PERCENT

%token DOUBLE_EQUALS
%token LESS_GREATER
%token KW_IS
%token LESS_EQUALS
%token LESS
%token GREATER_EQUALS
%token GREATER
%token KW_OR
%token KW_AND
%token KW_NOT
%token AMP

%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token RARROW

%token KW_IF
%token KW_THEN
%token KW_WHILE
%token KW_DO
%token KW_END
%token KW_OF
%token KW_ELSE
%token KW_ELSIF
%token KW_RETURN
%token KW_BREAK
%token KW_CONTINUE
%token KW_EXPORT

%token KW_COUNT
%token EOL
%token EOF
%token Empty // @Temp

// Non-terminal Types

%type <Program> program
%type <Program> start
%type <Declaration list> decl_list
%type <Declaration> decl
%type <VariableDecl> variable_decl
%type <TypeSpec> type_spec
%type <Parameters> parameters
%type <Parameters> parameter_list
%type <VariableDecl> parameter
%type <Statement list> stmt_list
%type <Statement> stmt
%type <SExpression> sexpr
%type <IfStatement> if_stmt
%type <WhileStatement> while_stmt
%type <CompoundStatement> compound_stmt
%type <Expression option> return_stmt
%type <unit> break_stmt
%type <unit> continue_stmt
%type <Expression> expr
%type <Arguments> arguments
%type <Literal> literal

// Association

%nonassoc SINGLE_EQUALS DOUBLE_COLON COLON_EQUALS
%left KW_OR KW_AND
%left DOUBLE_EQUALS LESS_GREATER KW_IS LESS_EQUALS LESS GREATER_EQUALS GREATER
%left PLUS MINUS
%left ASTERISK FSLASH PERCENT

%%

// Productions

start: program { $1 }

program: decl_list EOF           { $1 }

decl_list: decl_list decl     { $1 @ [$2] }
    | decl                    { [$1] }

decl: variable_decl           { Ast.VariableDecl $1 }
    | EOL                     { Ast.DeclNop} // @Temporary: Causes reduce/reduce conflicts

type_spec: KW_VOID   { Ast.Void }
    | KW_INT    { Ast.Int }
    | KW_FLOAT  { Ast.Float }
    | KW_CHAR   { Ast.Char }
    | KW_BYTE   { Ast.Byte }
    | KW_STRING { Ast.String }
    | KW_TEXT   { Ast.Text }

// const :: 5
fixed_decl: fixed_scalar_decl { $1 }
    | fixed_array_decl { $1 }

variable_decl: scalar_decl { $1 }
    | array_decl { $1 } 
    | fixed_decl { $1 }
    | procedure_decl { $1 }   

fixed_scalar_decl: IDENT DOUBLE_COLON literal    { Ast.FixedScalarDecl($1, $3) }

fixed_array_decl: IDENT DOUBLE_COLON LBRACKET INT_LITERAL RBRACKET KW_OF type_spec EOL
    { Ast.FixedArrayVariableDecl($1, $4, $7) }

scalar_decl: IDENT COLON type_spec EOL
    { Ast.ScalarVariableDecl($1, Some($3), None) }
    
    // var :int = expr
    | IDENT COLON type_spec SINGLE_EQUALS expr EOL
    { Ast.ScalarVariableDecl($1, Some($3), Some($5)) }
    
    // var := expr -- Implicit type
    | IDENT COLON_EQUALS expr EOL
    { Ast.ScalarVariableDecl($1, None, Some($3))}


array_decl: IDENT COLON LBRACKET RBRACKET KW_OF type_spec EOL
    { Ast.ArrayVariableDecl($1, None, Some($6), None) }

    | IDENT COLON LBRACKET expr RBRACKET KW_OF type_spec EOL
    { Ast.ArrayVariableDecl($1, Some($4), Some($7), None) }
    
    // array := (1, 2, 3, 4)
    | IDENT COLON_EQUALS aggregate EOL
    { Ast.ArrayVariableDecl($1, None, None, Some($3))}

// main :: (arg : type) -> type
procedure_decl: IDENT DOUBLE_COLON LPAREN parameters RPAREN RARROW type_spec compound_stmt
    { Ast.InternalProcedureDecl($1, $4, $7, $8)}

    | IDENT DOUBLE_COLON LPAREN RPAREN RARROW type_spec compound_stmt
    { Ast.InternalProcedureDecl($1, [], $6, $7)}

    | IDENT DOUBLE_COLON LPAREN parameters RPAREN compound_stmt
    { Ast.InternalProcedureDecl($1, $4, Ast.Void, $6)}

    | IDENT DOUBLE_COLON LPAREN RPAREN compound_stmt
    { Ast.InternalProcedureDecl($1, [], Ast.Void, $5)}

    // export main :: (arg : type) -> type
    | KW_EXPORT IDENT DOUBLE_COLON LPAREN parameters RPAREN RARROW type_spec compound_stmt
    { Ast.PublicProcedureDecl($2, $5, $8, $9)}

parameters: parameter_list  { $1 }
    // @Todo: ommit void
    | KW_VOID { [] }

parameter_list: parameter_list COMMA parameter { $1 @ [$3] }
    | parameter { [$1] }

parameter: IDENT COLON type_spec            
    { Ast.ScalarVariableDecl($1, Some $3, None) }
    // array : [] of int
    | IDENT COLON LBRACKET RBRACKET KW_OF type_spec
    { Ast.ArrayVariableDecl($1, None, Some $6, None) }
    // array : []int
    | IDENT COLON LBRACKET RBRACKET type_spec
    { Ast.ArrayVariableDecl($1, None, Some $5, None) }

stmt_list: stmt_list stmt  { $1 @ [$2] }
    | stmt                 { [$1] }

stmt: decl               { Ast.Declaration $1 }
    | sexpr              { Ast.SExpression $1 }
    | compound_stmt      { Ast.CompoundStatement $1 }
    | if_stmt            { Ast.IfStatement $1 }
    | while_stmt         { Ast.WhileStatement $1 }
    | return_stmt        { Ast.ReturnStatement $1 }
    | break_stmt         { Ast.BreakStatement }
    | continue_stmt      { Ast.ContinueStatement }

sexpr: expr EOL { Ast.Expression $1 }

while_stmt: KW_WHILE expr stmt { ($2, $3) }

if_stmt: KW_IF cond_clause_s KW_END EOL       { ($2, None) }
    | KW_IF cond_clause_s else_opt KW_END EOL { ($2, Some($3)) }

cond_clause_s: cond_clause { [$1] }
	| cond_clause_s KW_ELSIF cond_clause { $1 @ [$3] }

cond_clause: cond_part stmt_list { Ast.ConditionalClause($1, $2) }

cond_part: condition KW_THEN { $1 }

condition: expr { $1 }

else_opt: KW_ELSE stmt_list { $2 }

compound_stmt: KW_DO EOL stmt_list KW_END EOL    { $3 }
    | KW_DO EOL KW_END EOL    { [] }

break_stmt: KW_BREAK EOL { } // Todo check AST

continue_stmt: KW_CONTINUE EOL { }

// @Todo: Return expression
return_stmt: KW_RETURN expr EOL       { Some($2) }
    | KW_RETURN EOL       { None }

expr_list: expr_list COMMA expr { $1 @ [$3] }
    | expr { [$1] }

aggregate: LPAREN expr_list RPAREN { $2 }

expr: relation { $1 }
    | expr logical relation { Ast.BinaryExpression($1, $2, $3) }
    | IDENT SINGLE_EQUALS expr
    { Ast.ScalarAssignExpression({ Identifier = $1 }, $3) }
    // a[expr] = expr
    | IDENT LBRACKET expr RBRACKET SINGLE_EQUALS expr
    { Ast.ArrayAssignExpression({ Identifier = $1 }, $3, $6) }


logical: KW_AND { Ast.CondAnd }
    | KW_OR     { Ast.CondOr }
    // @Todo: XOR

relation: equation { $1 }
    // Disallow cases like (x < 2 < 3)
    // Technically it's ok to allow multiple relational comparisons
    // but it can cause confusing cases like (x == x == x) which is a Type Error
    // because the left hand side is evaluated to a bool first. The correct
    // expression would be (x == x == True)
    | equation relational equation { Ast.BinaryExpression($1, $2, $3) }
    | error relation {
        yyerror "Multiple relation expressions"
        $2
    }

relational: DOUBLE_EQUALS { Ast.Eq }
    | LESS_EQUALS         { Ast.LtEq }
    | LESS                { Ast.Lt }
    | GREATER_EQUALS      { Ast.GtEq }
    | GREATER             { Ast.Gt }

equation: term { $1 }
    | equation combinatory term { Ast.BinaryExpression($1, $2, $3) }

unary: PLUS { Ast.Identity }
    | MINUS { Ast.Negate }

combinatory: PLUS { Ast.Add }
    | MINUS       { Ast.Sub }

term: factor { $1 }
    | term sequential factor { Ast.BinaryExpression($1, $2, $3) }

sequential: ASTERISK { Ast.Mul }
    | FSLASH         { Ast.Div }
    | PERCENT        { Ast.Mod }

factor: primary  { $1 }
    | KW_NOT primary  { Ast.UnaryExpression(Ast.LogicalNot, $2)}
    | unary primary { Ast.UnaryExpression($1, $2) }

primary: literal { Ast.LiteralExpression $1 }
    | IDENT      { Ast.IdentifierExpression({ Identifier = $1 }) }
    | qualified  { $1 }
    | paren_primary { $1 }

qualified: IDENT LBRACKET expr RBRACKET
    { Ast.ArrayIdentifierExpression({ Identifier = $1 }, $3) }
    | IDENT LPAREN arguments RPAREN
    { Ast.ProcedureCallExpression({ Identifier = $1 }, $3)}
    // proc()
    | IDENT LPAREN RPAREN
    { Ast.ProcedureCallExpression({ Identifier = $1 }, [])}
    | IDENT DOT KW_COUNT
    { Ast.ArrayCountExpression({ Identifier = $1} )}

    // @Todo struct IDEN DOT

paren_primary: LPAREN expr RPAREN { $2 }

arguments: arguments COMMA expr { $1 @ [$3] }
    | expr                      { [$1] }

literal: INT_LITERAL   { Ast.IntLiteral(int $1) }
    | FLOAT_LITERAL    { Ast.FloatLiteral(float32 $1) }
    | BYTE_LITERAL     { Ast.ByteLiteral(byte $1) }
    | STRING_LITERAL   { Ast.StringLiteral $1 }
    | HASH KW_CHAR STRING_LITERAL { Ast.CharLiteral(char $3) } // @Todo error check string length
    | HASH KW_TEXT STRING_LITERAL { Ast.TextLiteral $3 }

%%

// Trailer
