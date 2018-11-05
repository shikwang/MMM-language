type token =
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ADDONE
  | MINUSONE
  | ELETIMES
  | ELEDIVIDE
  | LPARE
  | RPARE
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | AND
  | OR
  | NOT
  | COMMA
  | SEMICOL
  | COL
  | DOT
  | ASSIGN
  | EQUAL
  | NEQUAL
  | GT
  | NLT
  | LT
  | NGT
  | IF
  | ELSE
  | ELIF
  | FOR
  | WHILE
  | BREAK
  | RETURN
  | FUNCTION
  | STRUCT
  | MATRIX
  | INT
  | FLOAT
  | BOOLEAN
  | STRING
  | TRUE
  | FALSE
  | INT_LITERAL of (int)
  | FLOAT_LITERAL of (float)
  | STRING_LITERAL of (string)
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
