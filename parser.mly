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
  decls EOF { $1 }

decls:
    /* nothing */ { [], [] }
  | decls fdecl {  ($2 :: fst $1), snd $1 }
  | decls stmt  {  fst $1, ($2 :: snd $1) }

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
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT { Int }
  | FLOAT { Float }
  | BOOLEAN { Boolean }
  | MATRIX {Matrix}
  | STRING {String}























