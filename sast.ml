(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

module StringMap = Map.Make(String)

type sexpr = datatyp * sx
and sx =
    SIntlit of int
  | SStringlit of string
  | SFloatlit of float
  | SBoolit of bool
  | SMatrixlit of float array * (int * int)
  | SVar of string
  | SStruaccess of string * string
  | SBinop of sexpr * biop * sexpr
  | SComma of sexpr list
  | SAssign of sexpr * sexpr
  | SUop of uniop * sexpr
  | SCall of string * sexpr list
  | SMataccess of string * sexpr * sexpr
  | SMatslicing of string * sexpr * sexpr
  | SEmpty (*declare variable without assigning value*)
  | SRange of sindex * sindex
and sindex = SBeg | SEnd | SInd of sexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SInitial of datatyp * string * sexpr
  | SDefaultmat of string * int * int
  | SIniStrucct of string * string * sexpr list

type sfunc_decl = {
    mutable sftyp : datatyp;
    sfname : string;
    sformals : bind list;
    slocals : bind list;
    smatsiz: (int * int) StringMap.t;
    sbody : sstmt list;
  }

type sstruc_decl = {
    sstname : string;
    sstvar : bind list;
  }

type sprogram =  sfunc_decl list * sstruc_decl list

(* Pretty-printing functions *)
let rec string_of_sexpr = function
    (_, SIntlit(l)) -> string_of_int l
  | (_, SFloatlit(l)) -> string_of_float l
  | (_, SMatrixlit(_,(a,b))) -> "(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ")"
  | (_, SStringlit(l)) -> l
  | (_, SBoolit(true)) -> "true"
  | (_, SBoolit(false)) -> "false"
  | (_, SVar(s)) -> s
  | (_, SStruaccess(s, p)) -> s^ "." ^p
  | (_, SBinop(e1, o, e2)) -> string_of_sexpr e1 ^ " " ^ string_of_biop o ^ " " ^ string_of_sexpr e2
  | (_, SUop(o, e)) -> string_of_uniop o ^ string_of_sexpr e
  | (_, SAssign(v, e)) -> string_of_sexpr v ^ " = " ^ string_of_sexpr e
  | (_, SCall(f, el)) -> f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | (_, SMataccess(m, b, c)) -> m ^ "[" ^string_of_sexpr b^ "][" ^ string_of_sexpr c ^ "]"
  | (_, SMatslicing(m, b, c)) -> m ^ "[" ^string_of_sexpr b^ "][" ^ string_of_sexpr c ^ "]"
  | (_, SRange(s, e)) -> let a = (match s with SBeg -> "SBeg" | SEnd -> "SEnd" | SInd(e) -> string_of_sexpr e) and b = 
                         (match e with SBeg -> "SBeg" | SEnd -> "SEnd" | SInd(e) -> string_of_sexpr e) in
                         a ^ " : " ^ b
  | (_, SEmpty) -> ""
  | _ -> ""

let rec string_of_sstmt = function
    SBlock(stmts) -> "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^"}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
  | SIf(e, s, SBlock([])) -> "if (" ^ string_of_sexpr e ^ ")\n" ^string_of_sstmt s
  | SIf(e, s1, s2) -> "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) -> "for (" ^ string_of_sexpr e1 ^ " ; " ^ string_of_sexpr e2 ^ "; " ^ string_of_sexpr e3 ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^string_of_sstmt s
  | SInitial(t, v, n) -> (match n with 
                            (Void, SEmpty) -> string_of_datatyp t ^ " " ^ v ^ ";\n"
                          | _ -> string_of_datatyp t^ " " ^ v ^ " = " ^ string_of_sexpr n ^ ";\n")
  | SDefaultmat(m, a, b) -> "matrix " ^ m ^ "[" ^ string_of_int a ^ ", " ^ string_of_int b ^ "];\n"
  | SIniStrucct(o, s, el) -> "struct " ^ o ^ " = " ^ s ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ");\n"
  | _ -> ""

let string_of_sstdecl stdecl = "struct " ^ stdecl.sstname ^ " {\n" ^ String.concat ";\n" (List.map string_of_bind stdecl.sstvar) ^ "\n}\n"

let string_of_sfdecl fdecl = "func " ^ string_of_datatyp fdecl.sftyp ^ " " ^fdecl.sfname ^ "(" ^ 
    String.concat ", " (List.map string_of_bind fdecl.sformals) ^ ")\n{\n" ^ String.concat "" (List.map string_of_sstmt fdecl.sbody) ^ "}\n"

let string_of_sprogram (funcs, structs) = String.concat "\n" (List.map string_of_sstdecl structs) ^ "\n" ^String.concat "\n" (List.map string_of_sfdecl funcs)