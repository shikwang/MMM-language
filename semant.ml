open Ast

module StringMap = Map.Make(String)
(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check(globals, functions)=
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
    if lvaluet == rvaluet then lvaluet else raise err
  in

  (**** Checking Global Variables ****)
  List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals;
  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

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

  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

  (* Function declaration for a named function *)
  let built_in_decls =  
    let add_bind map (name,ty) = StringMap.add name {
      ftyp = Void;
      fname = name;
      formals = [(ty,"x")];
      locals = [];
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

  let function_decl s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = function_decl "main" in (* Ensure "main" is defined *)

  let check_function func = 
    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);

    List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
      " in " ^ func.fname)) func.locals;

    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
      (List.map snd func.locals);



    

      (* following are edited by Shenghao *)
      (* Return a semantically-checked statement i.e. containing sexprs *)
    in
    
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, st) -> SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Return e -> let (t, e') = expr e in
        if t = func.typ then SReturn (t, e') 
        else raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                    string_of_typ func.typ ^ " in " ^ string_of_expr e))
	    
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
    { styp = func.typ;
      sfname = func.fname;
      sformals = func.formals;
      slocals  = func.locals;
      sbody = match check_stmt (Block func.body) with SBlock(sl) -> sl
      | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
  in (globals, List.map check_function functions)