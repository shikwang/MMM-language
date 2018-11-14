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
%token INT FLOAT BOOLEAN STRING VOID
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
  decls EOF { List.rev (fst $1), List.rev (snd $1) }

decls:
    /* nothing */ { [], [] }
  | decls fdecl { ($2 :: fst $1), snd $1 }
  | decls stdecl { fst $1, ($2 :: snd $1) }

fdecl:
  FUNCTION typ ID LPARE formals_opt RPARE LBRACE stmt_list RBRACE
    { { ftyp = $2;
        fname = $3;
        formals = $5;
        body = List.rev $8 } }

stdecl:
  STRUCT ID LBRACE struct_list RBRACE { {stname = $2; stvar = List.rev $4} }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                        { [Primdecl($1,$2)] }
  | LT ID GT ID                   { [Strudecl($2,$4)] }
  | formal_list COMMA typ ID      { Primdecl($3,$4) :: $1 }
  | formal_list COMMA LT ID GT ID { Strudecl($4,$6) :: $1 }

struct_list:
    typ ID                          { [Primdecl($1,$2)] }
  | struct_list SEMICOL typ ID      { Primdecl($3,$4) :: $1 }

typ:
    INT { Int }
  | FLOAT { Float }
  | BOOLEAN { Boolean }
  | MATRIX { Matrix }
  | STRING { String }
  | VOID { Void }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr_opt SEMICOL { Expr($1) }
  | RETURN expr SEMICOL { Return($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPARE expr RPARE stmt %prec NOELSE { If($3, $5, Block([])) } 
    /* elseif */
  | IF LPARE expr RPARE stmt ELSE stmt { If($3, $5, $7) }
  | FOR LPARE expr SEMICOL expr SEMICOL expr RPARE stmt  { For($3, $5, $7, $9) }
  | WHILE LPARE expr RPARE stmt { While($3, $5) }
  | typ ID SEMICOL { Initial($1, $2, Empty) }
  | typ ID ASSIGN expr SEMICOL { Initial($1, $2, $4) }
  | typ ID LBRACK INT_LITERAL COMMA INT_LITERAL RBRACK SEMICOL{ Defaultmat($2, $4, $6) } 
  | STRUCT ID ASSIGN ID LPARE expr RPARE SEMICOL{ let inputs = match $6 with
                                                              Comma(e1) -> e1
                                                            | Empty -> []
                                                            | _ -> [$6] in IniStrucct($2, $4, inputs)}

expr_opt:
   /* nothing */ { Empty }
  | expr { $1 }

expr:
    INT_LITERAL { Intlit($1) }
  | STRING_LITERAL { Stringlit($1) }
  | FLOAT_LITERAL { Floatlit($1) }
  | mat_literal { Matrixlit(fst $1, snd $1) }
  | TRUE { Boolit(true) }
  | FALSE { Boolit(false) }
  | ID { Var($1) }
  | ID DOT ID  { Struaccess ($1, $3)}
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
                                                   Range(_,_), c -> (match c with Range(_,_) -> Matslicing($1, $3, c)
                                                                       | _ -> Matslicing($1, $3, Range(Ind(c),Ind(c))))
                                                 | r, Range(_,_) -> Matslicing($1, Range(Ind(r),Ind(r)), $6)
                                                 | a, b -> Mataccess($1, a, b)
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
