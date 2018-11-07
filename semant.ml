open Ast

module StringMap = Map.Make(String)
(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.

   Check each struct, then check each function *)

let check(struct, functions)=
  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate except list =
    let rec helper function
    n1 :: n2 :: _ when n1 = n2 -> raise(Failure(exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
    (Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in

  (* Raise an exception of the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
    if lvaluet == rvaluet then lvaluet else raise (Failure err)
  in

  (**** Checking struct ****)
  List.iter (check_not_void (fun n -> "illegal void struct " ^ n)) struct;
  report_duplicate (fun n -> "duplicate struct " ^ n) (List.map snd struct);

  (**** Checking Functions ****)
  let report_built_in_duplicate list =
    let rec helper = function
    | [] -> ()
    | _ :: t -> helper t
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
    in helper list
  in report_built_in_duplicate (List.map (fun fd -> fd.name) functions)

  (* Add function declaration for a named function *)
  let built_in_decls =  
    let add_bind map (name,ty) = StringMap.add name {
      ftyp = Void;
      fname = name;
      formals = [(ty,"x")];
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

  let _ = function_decl "main" in (* Ensure "main" is defined *)

  let check_function func = 
    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);

    (*
    List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
      " in " ^ func.fname)) func.locals;

    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
      (List.map snd func.locals);
    *)

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
      StringMap.empty (func.formals @ func.locals )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
        Intlit  l   -> (Int, SIntlit l)
      | Floatlit l  -> (Float, SFloatlit l)
      | Stringlit l -> (String, SStringlit l)
      | Boolit l    -> (Boolean, SBoolit l)
      | Empty       -> (Void, SEmpty)
      | Matrixlit(l,s) -> (Matrix, SMatrixlit(l, s))
      | Var s       -> (type_of_identifier s, SVar s)
      | Assign(var, e) as ex -> 
          let (lt, s) = expr var
          and (rt, e') = expr e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign((lt, s), (rt, e')))
      | Uop(op, e) as ex -> 
          let (t, e') = expr e in
          let ty = match op with
            Nega when t = Int || t = Float || t = Matrix -> t
          | Not  when t = Boolean -> Boolean
          | _ -> raise (Failure ("illegal unary operator " ^ 
                            string_of_uop op ^ string_of_typ t ^
                            " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
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
          | Mult when t1 = Matrix && (t2 = Int || t2 = Float) -> Matrix
          | Div  when t1 = Matrix && (t2 = Int || t2 = Float) -> Matrix
          | Add | Sub | Mult |Elemult | Elediv when same && t1 = Matrix -> Matrix
          | Eq | Neq               when same                -> Boolean
          | Less | Leq | Greater | Geq when same && (t1 = Int || t1 = Float)  -> Boolean
          | Less | Leq | Greater | Geq when t1 = Int && t2 = Float -> Boolean
          | Less | Leq | Greater | Geq when t1 = Float && t2 = Int -> Boolean
          | And | Or when same && t1 = Boolean -> Boolean
          | _ -> raise ( Failure ("illegal binary operator " ^ string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e = 
            let (et, e') = expr e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.ftyp, SCall(fname, args'))
    in

    let check_bool_expr e = 
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e') 
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, st) ->
	      SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Return e -> let (t, e') = expr e in
        if t = func.ftyp then SReturn (t, e') 
        else raise (
	      Failure ("return gives " ^ string_of_typ t ^ " expected " ^
		    string_of_typ func.ftyp ^ " in " ^ string_of_expr e))

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
      { styp = func.ftyp;
        sfname = func.fname;
        sformals = func.formals;
        sbody = match check_stmt (Block func.body) with
        SBlock(sl) -> sl
        | _ -> raise (Failure ("internal error: block didn't become a block?"))
      }
  in (struct, List.map check_function functions)