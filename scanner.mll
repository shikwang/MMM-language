{ open Parser }

rule token =
  parse [' ' '\t' '\r' '\n']  { token lexbuf }
      | "/*"                  { comment lexbuf}
      | "//"                  { inlinecom lexbuf}
      | '+'                   { PLUS }
      | '-'                   { MINUS }
      | '*'                   { TIMES }
      | '/'                   { DIVIDE }
      | "++"                  { ADDONE }
      | "--"                  { MINUSONE }
      | ".*"                  { ELETIMES }
      | "./"                  { ELEDIVIDE }
      | '('                   { LPARE }
      | ')'                   { RPARE }
      | '{'                   { LBRACE }
      | '}'                   { RBRACE }
      | '['                   { LBRACK }
      | ']'                   { RBRACK }
      | "&&"                  { AND }
      | "||"                  { OR }
      | '!'                   { NOT }
      | ','                   { COMMA }
      | ';'                   { SEMICOL }
      | ':'                   { COL }
      | '.'                   { DOT }
      | '='                   { ASSIGN }
      | "=="                  { EQUAL }
      | "!="                  { NEQUAL }
      | '>'                   { GT }
      | ">="                  { NLT }
      | '<'                   { LT }
      | "<="                  { NGT }
      | "if"                  { IF }
      | "else"                { ELSE }
      | "elif"                { ELIF }
      | "for"                 { FOR }
      | "while"               { WHILE }
      | "break"               { BREAK }
      | "return"              { RETURN }
      | "func"                { FUNCTION }
      | "struct"              { STRUCT }
      | "matrix"              { MATRIX }
      | "int"                 { INT }
      | "float"               { FLOAT }
      | "string"              { STRING }
      | "void"                { VOID }
      | "bool"                { BOOLEAN }
      | "true"                { TRUE }
      | "false"               { FALSE }
      | ['0'-'9']+ as lit     { INT_LITERAL(int_of_string lit) }
      | ['0'-'9']+'.'['0'-'9']+ as flt    { FLOAT_LITERAL(float_of_string flt) }
      | '"' ([^ '"']* as str) '"'         { STRING_LITERAL(str) }
      | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
      | eof                   { EOF }
      | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
        "*/"                  { token lexbuf }
      | _                     { comment lexbuf }
and inlinecom = parse
        ['\r' '\n']           { token lexbuf }
      | _                     { inlinecom lexbuf}
