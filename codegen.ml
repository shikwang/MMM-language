module L = Llvm
module A = Ast
open Sast 
open Ast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (functions, structs) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which we will generate code *)
  let the_module = L.create_module context "MMM" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and void_t     = L.void_type   context 
  and array_t    = L.array_type
  and pointer_t  = L.pointer_type 
  in

  let matrix_t = L.named_struct_type context "matrix_t" in 
    L.struct_set_body matrix_t [|L.pointer_type float_t; i32_t; i32_t|] false;

  (*let matrixptr_t = L.pointer_type matrix_t in*)

  (* Return the LLVM type for a MicroC type *)
  (* To do : matrix and struct *)
  let ltype_of_typ = function
        A.Int   -> i32_t
      | A.Boolean  -> i1_t
      | A.Float -> float_t
      | A.Void  -> void_t
      | A.String  -> pointer_t i8_t
      (*
      | A.Matrix -> array_t float_t 4 (*the int must be equal to total size of the matrix*)
      *)
      | A.Matrix -> L.pointer_type matrix_t
  in

  (* function types *)
  let printf_t : L.lltype = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = L.declare_function "printf" printf_t the_module in

  let open_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t;i32_t |] in
  let open_func = L.declare_function "open" open_t the_module in


  (* let read_t = L.var_arg_function_type i32_t [| i32_t; L.pointer_type i32_t; i32_t |] in
  let read_func = L.declare_function "read" read_t the_module in
  let readbyte_t = L.var_arg_function_type i32_t [| i32_t; L.pointer_type i8_t; i32_t |] in
  let readbyte_func = L.declare_function "read" readbyte_t the_module in *)
  let readfl_t = L.var_arg_function_type i32_t [| i32_t; L.pointer_type float_t; i32_t |] in
  let readfl_func = L.declare_function "read" readfl_t the_module in
  let creat_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t;i32_t |] in
  let creat_func = L.declare_function "creat" creat_t the_module in
  let write_t = L.var_arg_function_type i32_t [| i32_t; L.pointer_type i8_t; i32_t |] in
  let write_func = L.declare_function "write" write_t the_module in
  let close_t = L.var_arg_function_type i32_t [| i32_t |] in
  let close_func = L.declare_function "close" close_t the_module in

  (*
  let getHeight_t : L.function_type i32_t [| matrix_t |] in
  let getHeight_func = L.declare_function "height" getHeight_t the_module in 
  *)

  (* Define each function (arguments and return type) so we can 
    call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
          and formal_types = Array.of_list(List.map (fun (t,_) -> ltype_of_typ t) 
          (List.map (fun c -> match c with 
            Primdecl(a,b) -> (a,b)
          | Strudecl(a,b) -> (Struct,b)) fdecl.sformals) 
          )
      in let ftype = L.function_type (ltype_of_typ fdecl.sftyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let string_format_str = L.build_global_stringptr "%s\n" "fmt" builder
    and int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = 
        L.set_value_name n p;
	    let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
	    StringMap.add n local m 

      (* Allocate space for any locally declared variables and add the
      * resulting registers to our map *)
      and add_local m (t, n) =
      let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m 
      in
      let formal_list = List.map (fun c -> match c with 
                                              Primdecl(a,b) -> (a,b)
                                            | Strudecl(a,b) -> (Struct,b)) fdecl.sformals
      in
      let formals = List.fold_left2 add_formal StringMap.empty formal_list
                    (Array.to_list (L.params the_function)) 
      in List.fold_left add_local formals (List.filter (fun s -> not (List.mem s formal_list))
                    (List.map (fun c -> match c with 
                      Primdecl(a,b) -> (a,b)
                      | Strudecl(a,b) -> (Struct,b))  fdecl.slocals))
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = StringMap.find n local_vars 
    in

    (*the function builds the matrixlit*)
    let build_matrix_lit(f_array,(r,c)) builder =
      let mat = L.build_array_malloc float_t (L.const_int i32_t (r*c)) "system_mat" builder in
      (*try cast pointer*)
      (for x = 0 to (r*c-1) do
        let element_ptr = L.build_gep mat [|(L.const_int i32_t x)|] "element_ptr" builder in 
        ignore(L.build_store (L.const_float float_t f_array.(x)) element_ptr builder)
      done);
      let m = L.build_malloc matrix_t "m" builder in
      let m_mat = L.build_struct_gep m 0 "m_mat" builder in ignore(L.build_store mat m_mat builder);
      let m_r = L.build_struct_gep m 1 "m_r" builder in ignore(L.build_store (L.const_int i32_t r) m_r builder);
      let m_c = L.build_struct_gep m 2 "m_c" builder in ignore(L.build_store (L.const_int i32_t c) m_c builder);
      m (*L.build_pointercast m matrixptr_t "m" builder*)
    in

    let is_matrix ptr = 
      let ltype_string = L.string_of_lltype (L.type_of ptr) in
      match ltype_string with
        "%matrix_t*" -> true
      | _ -> false
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder ((_, e) : sexpr) = match e with
	      SIntlit i  -> L.const_int i32_t i
      | SBoolit b  -> L.const_int i1_t (if b then 1 else 0)
      | SFloatlit l -> L.const_float float_t l
      | SStringlit s -> L.build_global_stringptr s "tmp" builder

      (*turn float array into list, then change type to float_t, then back to array
      | SMatrixlit (f_array,(r,c)) -> 
        let l = r * c in 
        let f_array_list = Array.to_list f_array in
        let f_array_list_ll = List.map(L.const_float float_t) f_array_list in
        let f_array_list_ll_array = Array.of_list f_array_list_ll in
        L.const_array (array_t float_t l) f_array_list_ll_array
      *)
      | SMatrixlit (f_array,(r,c)) -> (build_matrix_lit (f_array,(r,c)) builder)
      | SMataccess (s,e1,e2) ->
        let idx = 
          let e1' = expr builder e1 and e2' = expr builder e2 in
          let ptr = L.build_load (lookup s) s builder in
          let mat = L.build_load (L.build_struct_gep ptr 0 "m_mat" builder) "mat" builder in
          let r = L.build_load (L.build_struct_gep ptr 1 "m_r" builder) "r_mat" builder in
          let c = L.build_load (L.build_struct_gep ptr 2 "m_c" builder) "c_mat" builder in

          (*L.build_load (build_matrix_access (mat r c e1' e2' builder) "element_ptr" builder)*)
          let index = L.build_add e2' (L.build_mul e1' c "tmp" builder) "index" builder in
          L.build_gep mat [|index|] "element_ptr_ptr" builder
        in
        L.build_load idx "element_ptr" builder

      | SEmpty     -> L.const_int i32_t 0
      | SVar s     -> L.build_load (lookup s) s builder
       (* let ptr = lookup s in
        (match (is_matrix ptr) with
          | true -> ptr
          | false -> (L.build_load (ptr) s builder))*)

      | SAssign (s, e) -> 
        let e' = expr builder e in 
          (match s with 
            | (_,SVar(s1)) -> ignore(L.build_store e' (lookup s1) builder); e'
            | _ -> raise (Failure "Assign Failiure!"))              
      | SBinop ((A.Float,_ ) as e1, op, e2) ->
        let e1' = expr builder e1
        and e2' = expr builder e2 in
        (match op with 
          A.Add     -> L.build_fadd
        | A.Sub     -> L.build_fsub
        | A.Mult    -> L.build_fmul
        | A.Div     -> L.build_fdiv 
        | A.Eq      -> L.build_fcmp L.Fcmp.Oeq
        | A.Neq     -> L.build_fcmp L.Fcmp.One
        | A.Less    -> L.build_fcmp L.Fcmp.Olt
        | A.Leq     -> L.build_fcmp L.Fcmp.Ole
        | A.Greater -> L.build_fcmp L.Fcmp.Ogt
        | A.Geq     -> L.build_fcmp L.Fcmp.Oge
        | A.And | A.Or ->
            raise (Failure "internal error: semant should have rejected and/or on float")
        ) e1' e2' "tmp" builder
        
      | SBinop (e1, op, e2) ->
        let e1' = expr builder e1
        and e2' = expr builder e2 in
        (match op with
          A.Add     -> L.build_add
        | A.Sub     -> L.build_sub
        | A.Mult    -> L.build_mul
        | A.Div     -> L.build_sdiv
        | A.And     -> L.build_and
        | A.Or      -> L.build_or
        | A.Eq      -> L.build_icmp L.Icmp.Eq
        | A.Neq     -> L.build_icmp L.Icmp.Ne
        | A.Less    -> L.build_icmp L.Icmp.Slt
        | A.Leq     -> L.build_icmp L.Icmp.Sle
        | A.Greater -> L.build_icmp L.Icmp.Sgt
        | A.Geq     -> L.build_icmp L.Icmp.Sge
        ) e1' e2' "tmp" builder

      (* operator for matrix*)
      (*
      | SBinop ((A.Matrix,_ ) as e1, op, e2) ->
        let e1' = expr builder e1 
        and e2' = expr builder e2 in
        (match op with
          A.Add -> 
          A.Sub ->
          A.Mult ->
          A.Elemult ->
          A.Elediv ->
        )
        *)

      | SUop(op, ((t, _) as e)) ->
        let e' = expr builder e in
        (match op with
          A.Nega when t = A.Float -> L.build_fneg 
        | A.Nega                  -> L.build_neg
        | A.Not                   -> L.build_not) e' "tmp" builder

      | SCall ("print", [e]) ->
	      L.build_call printf_func [| int_format_str ; (expr builder e) |]
        "printf" builder      

      (*Add print functions, other built-in functions*)
      | SCall ("printStr", [e]) ->
        L.build_call printf_func [| string_format_str ; (expr builder e) |]
        "printf" builder

      | SCall ("printFloat",[e]) ->
        L.build_call printf_func [| float_format_str ; (expr builder e) |]
        "printf" builder

      | SCall ("open", ([ e ; e2 ])) ->
              (L.build_call open_func [| expr builder e;expr builder e2|] "open" builder)
      
      (* | SCall ("imread", ([ e ])) ->
        let ev = A.string_of_expr e in
        (* let arrptr = (lookup ev) in  *)
        (* need array size  *)
        let fd = (L.build_call open_func [| ev ; L.const_int i32_t 0|] "open" builder) in
        let ret = L.build_call readfl_func 
            [| fd ;
              (L.build_gep arrptr [|L.const_int i32_t 0;L.const_int i32_t 0|] "tmp" builder);
                L.const_int i32_t (arrsize*8)|] "read" builder in
        (ignore (L.build_call close_func [| fd |] "close" builder));ret 

 *)


      (* print matrix function *)


      | SCall ("height",[e]) ->
        let r = L.build_load (L.build_struct_gep (expr builder e) 1 "m_r" builder) "r_mat" builder in
        r
      
      | SCall ("width", [e]) ->
        let c = L.build_load (L.build_struct_gep (expr builder e) 2 "m_c" builder) "c_mat" builder in
        c

      | SCall ("sum", [e]) ->
        let r = L.build_load (L.build_struct_gep (expr builder e) 1 "m_r" builder) "r_mat" builder in
        let c = L.build_load (L.build_struct_gep (expr builder e) 2 "m_c" builder) "c_mat" builder in
        let mat = L.build_load (L.build_struct_gep (expr builder e) 0 "m_mat" builder) "mat_mat" builder in

        let sum = L.build_alloca float_t "sumOfEle" builder in
        let total = L.build_sub (L.build_mul r c "tmp" builder) (L.const_int i32_t 1) "index" builder in
        ignore(L.build_store (L.const_float float_t 0.0) sum builder);
        (*let total_int = L.const_ptrtoint total i32_t in
        let total_const_int = L.const_int i32_t total_int in*)

        (*
        let total_int64opt = L.int64_of_const total in
        
        let total_val = match total_int64opt with
          | Some t -> t
          | None -> raise(Failure "Fail here")
        in
        
        let t_v = Int64.to_int r_val in *)
        (for x=0 to 3 do
          let ele_ptr_ptr = (L.build_gep mat [|L.const_int i32_t x|] "element_ptr_ptr" builder) in
          let ele = L.build_load ele_ptr_ptr "element_ptr" builder in
          let tmp_sum = L.build_fadd (L.build_load sum "addsum" builder) ele "tmp_sum" builder in 
            ignore(L.build_store tmp_sum sum builder);
        done);
        L.build_load sum "addsum" builder
        
      | SCall ("mean", [e]) ->
        let r = L.build_load (L.build_struct_gep (expr builder e) 1 "m_r" builder) "r_mat" builder in
        let c = L.build_load (L.build_struct_gep (expr builder e) 2 "m_c" builder) "c_mat" builder in
        let mat = L.build_load (L.build_struct_gep (expr builder e) 0 "m_mat" builder) "mat_mat" builder in

        let sum = L.build_alloca float_t "sumOfEle" builder in
        let total = L.build_sub (L.build_mul r c "tmp" builder) (L.const_int i32_t 1) "index" builder in
        ignore(L.build_store (L.const_float float_t 0.0) sum builder);

        (for x=0 to 3 do
          let ele_ptr_ptr = (L.build_gep mat [|L.const_int i32_t x|] "element_ptr_ptr" builder) in
          let ele = L.build_load ele_ptr_ptr "element_ptr" builder in
          let tmp_sum = L.build_fadd (L.build_load sum "addsum" builder) ele "tmp_sum" builder in 
            ignore(L.build_store tmp_sum sum builder);
        done);
        L.build_fdiv (L.build_load sum "addsum" builder) (L.const_float float_t 4.0) "mean_sum" builder

      | SCall (f, args) -> 
         let (fdef, fdecl) = StringMap.find f function_decls in
	       let llargs = List.rev (List.map (expr builder) (List.rev args)) in
	       let result = (match fdecl.sftyp with 
                        A.Void -> ""
                      | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list llargs) result builder
    in
    
    (* LLVM insists each basic block end with exactly one "terminator" 
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
	      Some _ -> ()
      | None -> ignore (instr builder) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)

    let rec stmt builder = function
	      SBlock sl -> List.fold_left stmt builder sl
      | SExpr e -> ignore(expr builder e); builder 
      | SReturn e -> ignore(match fdecl.sftyp with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder 
                              (* Build return statement *)
                            | _ -> L.build_ret (expr builder e) builder );
                     builder
      | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = expr builder predicate in
        let merge_bb = L.append_block context "merge" the_function in
        let build_br_merge = L.build_br merge_bb in (* partial function *)

        let then_bb = L.append_block context "then" the_function in
        add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
          build_br_merge;

        let else_bb = L.append_block context "else" the_function in
        add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
          build_br_merge;

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context merge_bb

      | SWhile (predicate, body) ->
        let pred_bb = L.append_block context "while" the_function in
        ignore(L.build_br pred_bb builder);

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (stmt (L.builder_at_end context body_bb) body)
          (L.build_br pred_bb);

        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val = expr pred_builder predicate in

        let merge_bb = L.append_block context "merge" the_function in
        ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
        L.builder_at_end context merge_bb

      (* Implement for loops as while loops *)
      | SFor (e1, e2, e3, body) -> stmt builder
        ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
      (* Implement initial here *)
      | SInitial (typ, name, e) -> (match e with 
                                        (_, SEmpty) -> builder 
                                      | _ -> let e' = expr builder e in
                                             (ignore(L.build_store e' (lookup name) builder); builder))
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.sftyp with
        A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module