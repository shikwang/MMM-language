open Ast

(* Pretty-printing functions *)

let string_of_biop = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Eq -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Elemult -> ".*"
  | Elediv -> "./"

let string_of_uniop = function
    Nega -> "-"
  | Not -> "!"

let rec string_of_expr = function
    SIntlit(l) -> string_of_int l
  | SFloatlit(l) -> string_of_float l
  | SMatrixlit(_,(a,b)) -> "(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ")"
  | SStringlit(l) -> l
  | SBoolit(true) -> "true"
  | SBoolit(false) -> "false"
  | SVar(s) -> s
  | SStruaccess(s, p) -> s^ "." ^p
  | SBinop(e1, o, e2) -> string_of_expr e1 ^ " " ^ string_of_biop o ^ " " ^ string_of_expr e2
  | SUop(o, e) -> string_of_uniop o ^ string_of_expr e
  | SAssign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
  | SCall(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | SMataccess(m, b, c) -> m ^ "[" ^string_of_expr b^ "][" ^ string_of_expr c ^ "]"
  | SMatslicing(m, b, c) -> m ^ "[" ^string_of_expr b^ "][" ^ string_of_expr c ^ "]"
  | SRange(s, e) -> let a = (match s with Beg -> "Beg" | End -> "End" | Ind(e) -> string_of_expr e) and b = 
                           (match e with Beg -> "Beg" | End -> "End" | Ind(e) -> string_of_expr e) in
                           a ^ " : " ^ b
  | SEmpty -> ""
  | _ -> ""

let string_of_datatyp = function
    Int -> "int"
  | Boolean -> "bool"
  | Void -> "void"
  | Float -> "float"
  | Matrix -> "matrix"
  | String -> "string"
  | _ -> ""

let rec string_of_stmt = function
    SBlock(stmts) -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^"}\n"
  | SExpr(expr) -> string_of_expr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | SIf(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^string_of_stmt s
  | SIf(e, s1, s2) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | SFor(e1, e2, e3, s) -> "for (" ^ string_of_expr e1 ^ " ; " ^ string_of_expr e2 ^ "; " ^ string_of_expr e3 ^ ") " ^ string_of_stmt s
  | SWhile(e, s) -> "while (" ^ string_of_expr e ^ ") " ^string_of_stmt s
  | SInitial(t, v, n) -> (match n with 
                            Empty -> string_of_datatyp t ^ " " ^ v ^ ";\n"
                          | _ -> string_of_datatyp t^ " " ^ v ^ " = " ^ string_of_expr n ^ ";\n")
  | SDefaultmat(m, a, b) -> "matrix " ^ m ^ "[" ^ string_of_int a ^ ", " ^ string_of_int b ^ "];\n"
  | SIniStrucct(o, s, el) -> "struct " ^ o ^ " = " ^ s ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ");\n"
  | _ -> ""

let string_of_bind = function
    Primdecl(t, v) -> string_of_datatyp t ^ " " ^ v
  | Strudecl(t, v) -> "<" ^ t ^ "> " ^ v

let string_of_stdecl stdecl = "struct " ^ stdecl.sstname ^ " {\n" ^ String.concat ";\n" (List.map string_of_bind stdecl.sstvar) ^ "\n}\n"

let string_of_fdecl fdecl = "func " ^ fdecl.sfname ^ "(" ^ String.concat ", " (List.map string_of_bind fdecl.sformals) ^
   ")\n{\n" ^ String.concat "" (List.map string_of_stmt fdecl.sbody) ^ "}\n"

let string_of_program (funcs, structs) = String.concat "\n" (List.map string_of_stdecl structs) ^ "\n" ^String.concat "\n" (List.map string_of_fdecl funcs)

let _ = let lexbuf = Lexing.from_channel stdin in
let expr = Parser.program Scanner.token lexbuf in let result = string_of_program expr in print_endline result