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
%token INT FLOAT BOOLEAN STRING
%token TRUE FALSE

%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <string> ID
%token EOF

/* Precedence and associativity of each operator */

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
  decls EOF { List.rev $1 }

decls:
    /* nothing */ { [] }
  | decls fdecl { $2 :: $1 }

fdecl:
  FUNCTION ID LPARE formals_opt RPARE LBRACE stmt_list RBRACE
    { { ftyp = Void;
        fname = $2;
        formals = $4;
        body = List.rev $7 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT { Int }
  | FLOAT { Float }
  | BOOLEAN { Boolean }
  | MATRIX { Matrix }
  | STRING { Strin g}

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMICOL { Expr($1) }
  | RETURN expr SEMICOL { Return($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPARE expr RPARE stmt %prec NOELSE { If($3, $5, Block([])) } 
    /* elseif */
  | IF LPARE expr RPARE stmt ELSE stmt { If($3, $5, $7) }
  | FOR LPARE expr SEMICOL expr SEMICOL expr RPARE stmt  { For($3, $5, $7, $9) }
  | WHILE LPARE expr RPARE stmt { While($3, $5) }
  | typ ID SEMICOL { Initial($1, $2, Empty) }
  | typ ID ASSIGN expr SEMICOL { Initial($1, $2, $4) }
  | typ ID LBRACK INT COMMA INT RBRACK { Defaultmat($2, $4, $6) } 

expr_opt:
   /* nothing */ { Empty }
  | expr { $1 }

expr:
    INT_LITERAL { Intlit($1) }
  | STRING_LITERAL { Stringlit($1) }
  | FLOAT_LITERAL { Floatlit($1) }
  | mat_literal { Matrixlit(fst $1, snd $1) }
  | TRUE { Boollit(true) }
  | FALSE { Boollit(false) }
  | ID { Var($1) }
  | expr PLUS expr { Binop($1, Add, $3) }
  | expr MINUS expr { Binop($1, Sub, $3) }
  | expr TIMES expr { Binop($1, Mult, $3) }
  | expr DIVIDE expr { Binop($1, Div, $3) }
  | expr ADDONE   { Binop($1, Add, Intlit(1)) }
  | expr MINUSONE { Binop($1, Sub, Intlit(1)) }
  | expr ELETIMES expr { Binop($1, Elemult, $3) }
  | expr ELEDIVIDE expr { Binop($1, Elediv, $3) }
  | expr EQUAL expr { Binop($1, Eq, $3) }
  | expr NEQUAL expr { Binop($1, Neq, $3) }
  | expr LT expr { Binop($1, Less, $3) }
  | expr NGT expr { Binop($1, Leq, $3) }
  | expr GT expr { Binop($1, Greater, $3) }
  | expr NLT expr { Binop($1, Geq, $3) }
  | expr AND expr { Binop($1, And, $3) }
  | expr OR expr { Binop($1, Or, $3) }
  | expr COMMA expr { match $1, $3 with
                        Comma(e1), Comma(e2) -> Comma(e1@e2)
                      | Comma(e1), e2 -> Comma(e1@[e2])
                      | e1, Comma(e2) -> Comma(e1::e2)
                      | e1, e2 -> Comma([e1;e2])} 
  | MINUS expr %prec NEG { Uop(Nega, $2) }
  | NOT expr { Uop(Not, $2) }
  | expr ASSIGN expr { Assign($1, $3) }
  | ID LBRACK expr RBRACK LBRACK expr RBRACK { match $3, $6 with
                                                   Range(_,_), Range(_,_) -> Matslicing($1, $3, $6)
                                                 | Range(_,_), Intlit(_) -> Matslicing($1, $3, Range(Ind($6),Ind($6)))
                                                 | Intlit(_), Range(_,_) -> Matslicing($1, Range(Ind($3),Ind($3), $6))
                                                 | Intlit(_), Intlit(_) -> Mataccess($1, $3, $6)
                                                 | _ -> failwith("wrong indexing expression")}
  | ID LPARE expr_opt RPARE { let inputs = match $3 with
                                             Comma(e1) -> e1
                                           | Empty -> []
                                           | _ -> [$3] in Call($1, inputs)}
  | LPARE expr RPARE { $2 }
  | expr COL { Range(Ind($1), End) }
  | expr COL expr { Range(Ind($1), Ind($3)) }
  | COL expr { Range(Beg, Ind($2)) }
  | COL { Range(Beg, End) }

mat_literal: 
    LBRACK RBRACK { [| |], (0, 0) }     /* empty matrix */
  | LBRACK mat_assembly RBRACK { $2 } 

mat_assembly:
    ele { [| $1 |], (1, 1) }
  | mat_assembly COMMA ele { Array.append (fst $1) [| $3 |], (fst (snd $1) , snd (snd $1) +1) }
  | mat_assembly SEMICOL ele { Array.append (fst $1) [| $3 |], (fst (snd $1) +1 , 1) }

ele:
    FLOAT_LITERAL { $1 }
  | MINUS FLOAT_LITERAL %prec NEG { -. $2 }
