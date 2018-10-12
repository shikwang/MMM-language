type biop = Add | Sub | Mult | Div | Eq | Neq | Less | Leq | Greater | Geq | And | Or | Elemult | Elediv

type uniop = Not | Nega

type datatyp = Int | Float | Boolean | Matrix | Void

type bind = datatyp * string

type expr = 
    Intlit of int
  | Stringlit of string
  | Floatlit of float
  | Boolit of bool
  | Matrixlit of float array * (int * int)
  | Var of string
  | Binop of expr * biop * expr
  | Comma of expr list
  | Assign of expr * expr
  | Uop of uniop * expr
  | Call of string * expr list
  | Mataccess of string * expr * expr
  | Bug (* debug entity, not for other use *)
  | Range of index * index
and index = Beg | End | Ind of expr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Initial of datatyp * string * expr

type func_decl = {
  mutable ftyp : datatyp;
  fname : string;
  formals : bind list;
  body : stmt list;
}

type program = func_decl list
