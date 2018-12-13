type biop = Add | Sub | Mult | Div | Eq | Neq | Less | Leq | Greater | Geq | And | Or | Elemult | Elediv

(*elemult is matrix between int*)
type uniop = Not | Nega

type datatyp = Int | Float | Boolean | Matrix | String | Void | Struct | SMatrix of int * int

type bind = Primdecl of datatyp * string
  | Strudecl of string * string

type expr = 
    Intlit of int
  | Stringlit of string
  | Floatlit of float
  | Boolit of bool
  | Matrixlit of float array * (int * int)
  | Var of string
  | Struaccess of string * string
  | Binop of expr * biop * expr
  | Comma of expr list
  | Assign of expr * expr
  | Uop of uniop * expr
  | Call of string * expr list
  | Mataccess of string * expr * expr
  | Matslicing of string * expr * expr
  | Empty (*declare variable without assigning value*)
  | Range of index * index
and index = Beg | End | Ind of int

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Initial of datatyp * string * expr
  | Defaultmat of string * int * int
  | IniStrucct of string * string * expr list

type func_decl = {
  mutable ftyp : datatyp;
  fname : string;
  formals : bind list;
  body : stmt list;
}

type struc_decl = {
  stname : string;
  stvar : bind list;
}

type program =  func_decl list * struc_decl list

(* pretty print *)
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
    Intlit(l) -> string_of_int l
  | Floatlit(l) -> string_of_float l
  | Matrixlit(_,(a,b)) -> "(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ")"
  | Stringlit(l) -> l
  | Boolit(true) -> "true"
  | Boolit(false) -> "false"
  | Var(s) -> s
  | Struaccess(s, p) -> s^ "." ^p
  | Binop(e1, o, e2) -> string_of_expr e1 ^ " " ^ string_of_biop o ^ " " ^ string_of_expr e2
  | Uop(o, e) -> string_of_uniop o ^ string_of_expr e
  | Assign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
  | Call(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Mataccess(m, b, c) -> m ^ "[" ^string_of_expr b^ "][" ^ string_of_expr c ^ "]"
  | Matslicing(m, b, c) -> m ^ "[" ^string_of_expr b^ "][" ^ string_of_expr c ^ "]"
  | Range(s, e) -> let a = (match s with Beg -> "Beg" | End -> "End" | Ind(e) -> string_of_int e) and b = 
                           (match e with Beg -> "Beg" | End -> "End" | Ind(e) -> string_of_int e) in
                           a ^ " : " ^ b
  | Empty -> ""
  | _ -> ""

let string_of_datatyp = function
    Int -> "int"
  | Boolean -> "bool"
  | Void -> "void"
  | Float -> "float"
  | Matrix -> "matrix"
  | String -> "string"
  | Struct -> "struct"
  | SMatrix(a,b) -> "SMatrix(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ")"
  | _ -> ""

let rec string_of_stmt = function
    Block(stmts) -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^"}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^string_of_stmt s
  | If(e, s1, s2) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) -> "for (" ^ string_of_expr e1 ^ " ; " ^ string_of_expr e2 ^ "; " ^ string_of_expr e3 ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^string_of_stmt s
  | Initial(t, v, n) -> (match n with 
                            Empty -> string_of_datatyp t ^ " " ^ v ^ ";\n"
                          | _ -> string_of_datatyp t^ " " ^ v ^ " = " ^ string_of_expr n ^ ";\n")
  | Defaultmat(m, a, b) -> "matrix " ^ m ^ "[" ^ string_of_int a ^ ", " ^ string_of_int b ^ "];\n"
  | IniStrucct(o, s, el) -> "struct " ^ o ^ " = " ^ s ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ");\n"
  | _ -> ""

let string_of_bind = function
    Primdecl(t, v) -> string_of_datatyp t ^ " " ^ v
  | Strudecl(t, v) -> "<" ^ t ^ "> " ^ v

let string_of_stdecl stdecl = "struct " ^ stdecl.stname ^ " {\n" ^ String.concat ";\n" (List.map string_of_bind stdecl.stvar) ^ "\n}\n"

let string_of_fdecl fdecl = "func " ^ fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_bind fdecl.formals) ^
   ")\n{\n" ^ String.concat "" (List.map string_of_stmt fdecl.body) ^ "}\n"

let string_of_program (funcs, structs) = String.concat "\n" (List.map string_of_stdecl structs) ^ "\n" ^String.concat "\n" (List.map string_of_fdecl funcs)