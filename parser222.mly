%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE
%token ADDONE MINUSONE
%token ELETIMES ELEDIVIDE
%token LPARE RPARE LBRACE RBRACE LBRACK RBRACK
%token AND OR NOT
%token COMMA SEMICOL COL DOT
%token ASSIGN EQUAL NEQUAL
%token GT NLT LT NGT
%token IF ELSE ELIF
%token FOR WHILE BREAK RETURN
%token FUNCTION STRUCT MATRIX
%token INT FLOAT BOOLEAN
%token TRUE FALSE

%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <string> ID
%token GLOBAL EOF

/* Precedence */
%left SEMICOL
%nonassoc RETURN
%right ASSIGN
%nonassoc NOELSE
%nonassoc ELSE
%nonassoc ELIF
%left COMMA
%nonassoc COL
%nonassoc DOT
%left OR
%left AND
%left EQUAL NEQUAL
%left GT NLT LT NGT
%left PLUS MINUS
%left ADDONE MINUSONE
%left ELETIMES ELEDIVIDE
%left TIMES DIVIDE
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
    decls EOF { $1 }

decls:
    /* nothing */      { [], [] }
  | decls vdecl        { ($2 :: fst $1), snd $1 }
  | decls fdecl        { fst $1, ($2 :: snd $1) }

fdecl:
  typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
    { { typ = $1; 
        fname = $2; 
        formals = $4;
        locals = List.rev $7; 
        body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3, $4) :: $1 }

typ:
    INT       { Int }
  | FLOAT     { Float }
  | BOOLEAN      { Boolean }
  | STRING    { String }
  | MATRIX {Matrix}

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
    typ ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                                                   { Expr $1 }
  | RETURN SEMI                                                 { Return Noexpr }
  | RETURN expr SEMI                                            { Return $2 }
  | LBRACE stmt_list RBRACE                                     { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE                     { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt                        { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt                               { While($3, $5) }

expr_opt:
    /* nothing */                   { Noexpr }
  | expr                            { $1 }

expr:
    NUM_LIT                                                     { NumLit($1) }
  | STRING_LIT                                                  { StringLit($1) }
  | TRUE                                                        { BoolLit(true) }
  | FALSE                                                       { BoolLit(false) }
  | ID                                                          { Id($1) }
  | expr PLUS expr                                              { Binop($1, Add, $3) }
  | expr MINUS expr                                             { Binop($1, Sub, $3) }
  | expr TIMES expr                                             { Binop($1, Mult, $3) }
  | expr DIVIDE expr                                            { Binop($1, Div, $3) }
  | expr EQ expr                                                { Binop($1, Equal, $3) }
  | expr NEQ expr                                               { Binop($1, Neq, $3) }
  | expr LT expr                                                { Binop($1, Less, $3) }
  | expr LEQ expr                                               { Binop($1, Leq, $3) }
  | expr GT expr                                                { Binop($1, Greater, $3) }
  | expr GEQ expr                                               { Binop($1, Geq, $3) }
  | expr AND expr                                               { Binop($1, And, $3) }
  | expr OR expr                                                { Binop($1, Or, $3) }
  | MINUS expr %prec NEG                                        { Unop(Neg, $2) }
  | NOT expr                                                    { Unop(Not, $2) }
  | INC expr                                                    { Unop(Inc, $2) }
  | DEC expr                                                    { Unop(Dec, $2) }
  | expr ASSIGN expr                                            { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN                                { Call($1, $3) }
  | LPAREN expr RPAREN                                          { $2 }
  | LBRACKET mat_lit RBRACKET                                   { MatrixLit($2) }
  | ID LBRACKET expr RBRACKET LBRACKET expr RBRACKET            { MatrixAccess($1, $3, $6) }
  | ID COLON ROWS                                               { Rows($1) }
  | ID COLON COLS                                               { Cols($1) }
  | ID COLON TRANSPOSE                                          { Transpose($1) }
  | ID COLON TRACE                                              { Trace($1) }
  | ID COLON SUBMATRIX LBRACKET expr COMMA expr COMMA expr COMMA expr RBRACKET { SubMatrix($1, $5, $7, $9, $11) }

actuals_opt:
    /* nothing */                   { [] }
  | actuals_list                    { List.rev $1 }

actuals_list:
    expr                            { [$1] }
  | actuals_list COMMA expr         { $3 :: $1 }

mat_lit:
    LBRACKET lit_list RBRACKET                       { [$2] }
    | mat_lit SEMI LBRACKET lit_list RBRACKET        { $4 :: $1 }

lit_list:
    lit                             { [$1] }
    | lit_list COMMA lit            { $3 :: $1 }

lit:
    NUM_LIT                         { $1 }
