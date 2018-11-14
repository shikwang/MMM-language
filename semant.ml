open Ast
open Sast
(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.

   Check each struct, then check each function *)

module StringMap = Map.Make(String)

let check(functions, structures)=
  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate except list =
    let rec helper = function
    n1 :: n2 :: _ when n1 = n2 -> raise(Failure(except n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
      Primdecl(Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in

  (* Raise an exception of the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
    if lvaluet = rvaluet then lvaluet else raise (Failure err)
  in

  (**** Checking struct ****)
  report_duplicate (fun n -> "duplicate struct " ^ n) (List.map (fun st -> st.stname) structures);

  (**** Checking Functions ****)
  let report_built_in_duplicate list =
    let rec helper = function
    | [] -> ()
    | "print" :: _ -> raise (Failure ("function print may not be defined"))
    | "height" :: _ -> raise (Failure ("function height may not be defined"))
    | "width" :: _ -> raise (Failure ("function width may not be defined"))
    | "sum" :: _ -> raise (Failure ("function sum may not be defined"))
    | "mean" :: _ -> raise (Failure ("function mean may not be defined"))
    | "trans" :: _ -> raise (Failure ("function trans may not be defined"))
    | "eig" :: _ -> raise (Failure ("function eig may not be defined"))
    | "inv" :: _ -> raise (Failure ("function inv may not be defined"))
    | "det" :: _ -> raise (Failure ("function det may not be defined"))
    | "cov" :: _ -> raise (Failure ("function cov may not be defined"))
    | "imread" :: _ -> raise (Failure ("function imread may not be defined"))
    | "save" :: _ -> raise (Failure ("function save may not be defined"))
    | _ :: t -> helper t
    in helper list
  in report_built_in_duplicate (List.map (fun fd -> fd.fname) functions);

  (* Add function declaration for a named function *)
  let built_in_decls =  
    let add_bind map (name,ty) = StringMap.add name {
      ftyp = Void;
      fname = name;
      formals = [Primdecl(ty,"x")];
      body = []
    } map
    in List.fold_left add_bind StringMap.empty [
      ("print",String);("height",Matrix);("width",Matrix);
      ("sum",Matrix);("mean",Matrix);("trans",Matrix);
      ("eig",Matrix);("inv",Matrix);("det",Matrix);
      ("imread",String)
      ]
  in

  let function_decls = List.fold_left (
    fun m fd -> StringMap.add fd.fname fd m)
  built_in_decls functions
  in

  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let struct_decls = List.fold_left (
    fun m st -> StringMap.add st.stname st m)
    StringMap.empty structures
  in

  let find_str s = 
    try StringMap.find s struct_decls
    with Not_found -> raise (Failure ("unrecognized struct " ^ s))
  in

  let check_str str = 
    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^ " in " ^ str.stname)) str.stvar;
    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ str.stname)
    (List.map (fun b -> match b with 
        Primdecl(_,n) -> n
      | Strudecl(_,n) -> n) str.stvar);
    { sstname = str.stname;
      sstvar = str.stvar;}
  in

  let check_function func = 
    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map (fun b -> match b with 
      Primdecl(_,n) -> n
    | Strudecl(_,n) -> n) func.formals);

    (*
    List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
      " in " ^ func.fname)) func.locals;

    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
      (List.map snd func.locals);
    *)

    (* Build local symbol table of variables for this function *)
    let (symbols,matrixsize,strmap) = 
      let rec build_map sym siz smp = function
        Expr e ->(sym, siz, smp)
      | If(p, b1, b2) -> let (sym1,siz1,smp1) = build_map sym siz smp b1 in build_map sym1 siz1 smp1 b2
      | For(e1, e2, e3, st) -> build_map sym siz smp st 
      | While(p, s) -> build_map sym siz smp s
      | Return e -> (sym, siz, smp)  
      | Initial(t, v, e) -> if StringMap.mem v sym then raise (Failure "variable has been defined")
        else (StringMap.add v t sym, siz, smp)
      | Defaultmat(m, r, c) -> if StringMap.mem m sym then raise (Failure "matrix has been defined")
        else (StringMap.add m Matrix sym, StringMap.add m (r,c) siz, smp)
      | IniStrucct(v, stname, elist) -> if StringMap.mem v sym then raise (Failure "struct has been defined")
        else (StringMap.add v Struct sym, siz, StringMap.add v stname smp)
      (* A block is correct if each statement is correct and nothing
      follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> 
          let rec build_map_list a b c = function
            [Return _ as s] -> build_map a b c s
          | Return _ :: _   -> raise (Failure "nothing may follow a return")
          | Block sl :: ss  -> build_map_list a b c (sl @ ss) (* Flatten blocks *)
          | s :: ss         -> let (a1,b1,c1) = build_map a b c s in build_map_list a1 b1 c1 ss
          | []              -> (a, b, c)
          in build_map_list sym siz smp sl
      in 
      let symb = 
        let transfer m f = match f with 
        Primdecl(ty,na) -> StringMap.add na ty m
      | Strudecl(str,na) -> StringMap.add na Struct m in
        List.fold_left transfer StringMap.empty (func.formals) 
      in
      build_map symb StringMap.empty StringMap.empty (Block func.body)
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    let type_of_struct vn = 
      try StringMap.find vn strmap
      with Not_found -> raise (Failure ("unrecognized struct variable " ^ vn))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
        Intlit  l   -> (Int, SIntlit l)
      | Floatlit l  -> (Float, SFloatlit l)
      | Stringlit l -> (String, SStringlit l)
      | Boolit l    -> (Boolean, SBoolit l)
      | Empty       -> (Void, SEmpty)
      | Matrixlit(l,(r,c)) -> if Array.length l = r * c then (Matrix, SMatrixlit(l, (r,c)))
                              else raise ( Failure ("illegal Matrix Dimension"))
      | Var s       -> (type_of_identifier s, SVar s)
      | Struaccess (vname, member) -> 
          let stname = type_of_struct vname in 
          let st = find_str stname in
          let mt = 
            let rec find x lst = match lst with
                                    [] -> raise (Failure ("unrecognized struct member " ^ x))
                                  | hd :: tl -> (match hd with 
                                                    Primdecl(ty, x) -> ty
                                                  | _ -> find x tl)
            in find member st.stvar         
          in (mt, SStruaccess(vname, member))
      | Assign(var, e) as ex -> 
          let (lt, s) = expr var
          and (rt, e') = expr e in
          let err = "illegal assignment " ^ string_of_datatyp lt ^ " = " ^ 
            string_of_datatyp rt ^ " in " ^ string_of_expr e
          in (match lt,rt,var,e with
             Struct,Struct,Var(a),Var(b) -> let lstn = type_of_struct a in 
                let rstn = type_of_struct b in 
                if lstn = rstn then (Struct, SAssign((lt, s), (rt, e')))
                else raise (Failure ("illegal assignment " ^ lstn ^ " = " ^ rstn ^ " in " ^ string_of_expr e))
           | _ ->(check_assign lt rt err, SAssign((lt, s), (rt, e'))))
      | Uop(op, e) as ex -> 
          let (t, e') = expr e in
          let ty = match op with
            Nega when t = Int || t = Float || t = Matrix -> t
          | Not  when t = Boolean -> Boolean
          | _ -> raise (Failure ("illegal unary operator " ^ 
                            string_of_uniop op ^ string_of_datatyp t ^
                            " in " ^ string_of_expr ex))
          in (ty, SUop(op, (t, e')))
      | Binop(e1, op, e2) as e -> 
          let (t1, e1') = expr e1 
          and (t2, e2') = expr e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div when same && t1 = Int    -> Int
          | Add | Sub | Mult | Div when same && t1 = Float  -> Float
          | Add | Sub | Mult | Div when t1 = Float && t2 = Int  -> Float
          | Add | Sub | Mult | Div when t1 = Int && t2 = Float  -> Float
          | Mult when (t1 = Int || t1 = Float) && t2 = Matrix -> Matrix
          | Mult | Div  when t1 = Matrix && (t2 = Int || t2 = Float) -> Matrix
          | Add | Sub | Mult |Elemult | Elediv when same && t1 = Matrix -> Matrix
          | Eq | Neq               when same                -> Boolean
          | Less | Leq | Greater | Geq when same && (t1 = Int || t1 = Float)  -> Boolean
          | Less | Leq | Greater | Geq when t1 = Int && t2 = Float -> Boolean
          | Less | Leq | Greater | Geq when t1 = Float && t2 = Int -> Boolean
          | And | Or when same && t1 = Boolean -> Boolean
          | _ -> raise ( Failure ("illegal binary operator " ^ string_of_datatyp t1 ^ " " ^ string_of_biop op ^ " " ^
                  string_of_datatyp t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Comma(el) -> (Void, SComma(List.map expr el))
      | Mataccess(m, e1, e2) -> let mty = type_of_identifier m in 
                                if mty = Matrix then
                                  let (t1, se1) = expr e1 in let (t2, se2) = expr e2 in 
                                  (match t1,t2 with
                                    Int,Int -> (Float, SMataccess(m, (t1, se1), (t2, se2)))
                                  | _ -> raise ( Failure ("Index of "^ m ^ " is not a integer!")))
                                else raise ( Failure (m ^ "is not a matrix!"))
      | Matslicing(m, e1, e2) -> let mty = type_of_identifier m in 
                                 if mty = Matrix then (Matrix, SMatslicing(m, expr e1, expr e2))
                                 else raise ( Failure (m ^ "is not a matrix!"))
      | Range(a,b) -> (match a,b with
                        Beg, End -> (Int, SRange(SBeg, SEnd))
                      | Beg, Ind(s) -> let (t1, s1) = expr s in if t1 = Int then (Int, SRange(SBeg, SInd(t1, s1)))
                                       else raise ( Failure ("Illegeal Range"))
                      | Ind(s), End -> let (t1, s1) = expr s in if t1 = Int then (Int, SRange(SBeg, SInd(t1, s1)))
                                       else raise ( Failure ("Illegeal Range"))
                      | Ind(sl), Ind(ss) -> let (t1, s1) = expr sl in let (t2, s2) = expr ss in
                                            if t1 = Int && t2 = Int then (Int, SRange(SInd(t1, s1),SInd(t2, s2)))
                                            else raise ( Failure ("Illegeal Range"))
                      | _ -> raise ( Failure ("Illegeal Index")))
      | Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else 
          let check_call ff e = let (et, e') = expr e in 
            let err = "illegal argument found " ^ string_of_datatyp et ^ " in " ^ string_of_expr e
            in match ff with 
                Primdecl(styp,_) -> (check_assign styp et err, e')
              | Strudecl(strty,_) -> (match e with 
                                        Var(ve) -> let stn = type_of_struct ve in 
                                        if stn = strty then (Struct, e')
                                        else raise (Failure ("inconsistent type of struct " ^ strty ^ " and " ^ stn ^ " in " ^ string_of_expr e))
                                      | _ -> raise (Failure ("illegeal type of struct ")))
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.ftyp, SCall(fname, args'))
    in

    let check_bool_expr e = 
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Boolean then raise (Failure err) else (t', e') 
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
      Expr e -> SExpr (expr e)
    | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
    | For(e1, e2, e3, st) -> SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
    | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
    | Return e -> let (t, e') = expr e in
      if t = func.ftyp then SReturn (t, e') 
      else raise (
      Failure ("return gives " ^ string_of_datatyp t ^ " expected " ^
      string_of_datatyp func.ftyp ^ " in " ^ string_of_expr e))

    | Initial(t, v, e) -> let (t', e') = expr e in
      if t' = t || e = Empty then match t with 
        Struct | Void -> raise ( Failure (string_of_datatyp t ^ " cannot be initialed this way!"))
      | _ -> SInitial(t, v, (t', e'))
      else raise ( Failure ("Initial gives " ^ string_of_datatyp t' ^ " expected " ^
      string_of_datatyp t ^ " in " ^ string_of_expr e))

    | Defaultmat(m, r, c) -> SDefaultmat(m, r, c)

    | IniStrucct(v, stname, elist) -> 
      let st = find_str stname in
      let mem_length = List.length st.stvar in
      if List.length elist != mem_length then
      raise (Failure ("expecting " ^ string_of_int mem_length ^ 
                      " members in " ^ stname ))
      else let check_mem sv e = match sv with Primdecl(styp,_) ->
        let (et, e') = expr e in 
        let err = "illegal argument found " ^ string_of_datatyp et ^ 
        " expected " ^ string_of_datatyp styp ^ " in " ^ string_of_expr e
        in (check_assign styp et err, e')
      in 
      let elist' = List.map2 check_mem st.stvar elist in SIniStrucct(v, stname, elist')
    (* A block is correct if each statement is correct and nothing
       follows any Return statement.  Nested blocks are flattened. *)
    | Block sl -> 
        let rec check_stmt_list = function
            [Return _ as s] -> [check_stmt s]
          | Return _ :: _   -> raise (Failure "nothing may follow a return")
          | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
          | s :: ss         -> check_stmt s :: check_stmt_list ss
          | []              -> []
        in SBlock(check_stmt_list sl)
    in (* body of check_function *)
      { sftyp = func.ftyp;
        sfname = func.fname;
        sformals = func.formals;
        slocals = List.map (fun (v,ty) -> Primdecl(ty,v)) (StringMap.bindings symbols);
        sbody = match check_stmt (Block func.body) with
        SBlock(sl) -> sl
        | _ -> raise (Failure ("internal error: block didn't become a block?"))
      }
  in (List.map check_function functions, List.map check_str structures)