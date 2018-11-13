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
    slocals : datatyp StringMap.t;
    sbody : sstmt list;
  }

type sstruc_decl = {
    sstname : string;
    sstvar : bind list;
  }

type sprogram =  sfunc_decl list * sstruc_decl list

(* Pretty-printing functions *)

(*let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SLiteral(l) -> string_of_int l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SFliteral(l) -> l
  | SId(s) -> s
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SNoexpr -> ""
				  ) ^ ")"				     

let rec string_of_sstmt = function
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
  | SIf(e, s, SBlock([])) ->
      "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s

let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
*)