module L = Llvm
module A = Ast
open Sast 
open Ast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (functions, structs) =
  let idx_check = {
          sftyp = Void;
          sfname = "index_check";
          sformals = [Primdecl(Int,"i");Primdecl(Int,"r")];
          slocals = [];
          smatsiz = [];
          strlist =[];
          sbody = [SIf((Boolean, SBinop ((Int, SVar "i"), Less, (Int, SIntlit 0))), SBlock([SExpr (Void, SCall("abort",[]))]), SBlock([]));
          SIf((Boolean, SBinop ((Int, SVar "i"), Geq, (Int, SVar "r"))), SBlock([SExpr (Void, SCall("abort",[]))]), SBlock([]))]
  } 
  in
  let functions = idx_check::functions in

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


  (* use to interrupt the function flow and throw run-time exception *)
  let abort_func = L.declare_function "abort" (L.function_type void_t [||]) the_module in


  let load_cpp_t : L.lltype = L.function_type void_t [| L.pointer_type i8_t;L.pointer_type float_t;L.pointer_type float_t;L.pointer_type float_t |] in
  let load_cpp_func : L.llvalue = L.declare_function "load_cpp" load_cpp_t the_module in

  let save_cpp_t : L.lltype = L.function_type void_t [| L.pointer_type i8_t; L.pointer_type float_t; 
                              L.pointer_type float_t; L.pointer_type float_t; i32_t; i32_t; |] in
  let save_cpp_func : L.llvalue = L.declare_function "save_cpp" save_cpp_t the_module in

  let filter_cpp_t : L.lltype = L.function_type void_t [| L.pointer_type float_t; L.pointer_type i8_t; L.pointer_type i8_t; i32_t|] in
  let filter_cpp_func : L.llvalue = L.declare_function "filter_cpp" filter_cpp_t the_module in

  let iter1mat_cpp_t : L.lltype = L.function_type void_t [| L.pointer_type float_t; L.pointer_type float_t; float_t; i32_t|] in
  let iter1mat_cpp_func : L.llvalue = L.declare_function "iter1mat_cpp" iter1mat_cpp_t the_module in

  let trans_cpp_t : L.lltype = L.function_type void_t [| L.pointer_type float_t; L.pointer_type float_t; i32_t; i32_t|] in
  let trans_cpp_func : L.llvalue = L.declare_function "trans_cpp" trans_cpp_t the_module in

  let iter2mat_cpp_t : L.lltype = L.function_type void_t [| L.pointer_type float_t; L.pointer_type float_t; L.pointer_type float_t; i32_t; i32_t|] in
  let iter2mat_cpp_func : L.llvalue = L.declare_function "iter2mat_cpp" iter2mat_cpp_t the_module in

  let matmul_cpp_t : L.lltype = L.function_type void_t [| L.pointer_type float_t; L.pointer_type float_t; L.pointer_type float_t; i32_t; i32_t; i32_t; i32_t|] in
  let matmul_cpp_func : L.llvalue = L.declare_function "matmul_cpp" matmul_cpp_t the_module in


  let struct_decls : (L.lltype * sstruc_decl) StringMap.t = 
    let struct_decl m sdecl = 
      let struc_name = sdecl.sstname 
          and mem_types = Array.of_list(List.map (fun (t,_) -> ltype_of_typ t) 
          (List.map (fun c -> match c with 
            Primdecl(a,b) -> (a,b)
          | _ -> raise (Failure "Struct cannot have struct member!")) sdecl.sstvar))
      in let stype = L.named_struct_type context struc_name in 
      ignore(L.struct_set_body stype mem_types false);
      StringMap.add struc_name (stype, sdecl) m in
    List.fold_left struct_decl StringMap.empty structs in

  (* Define each function (arguments and return type) so we can 
    call it even before we've created its body *)
    let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
      let function_decl m fdecl =
        let name = fdecl.sfname
        and formal_types = 
            let formal_list = List.map (fun c -> match c with 
            Primdecl(a,b) -> (a,b)
          | Strudecl(a,b) -> (SStruct(a),b)) fdecl.sformals 
            in let typ_trans (t,v) = match t with
                SStruct(stn) -> let (sdef,_) = StringMap.find stn struct_decls in
                                pointer_t sdef
              | _ -> ltype_of_typ t
            in
            Array.of_list (List.map typ_trans formal_list)
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
	      let local = match t with 
                    SStruct(stn) -> let (sdef,_) = StringMap.find stn struct_decls in
                         L.build_alloca (pointer_t sdef) (n^"_ptr") builder
                  | _ -> L.build_alloca (ltype_of_typ t) n builder  
        in
      ignore (L.build_store p local builder);
	    StringMap.add n local m 

      (* Allocate space for any locally declared variables and add the
      * resulting registers to our map *)
      and add_local m (t, n) = 
          let local_var = match t with 
              SStruct(stn) -> let (sdef,_) = StringMap.find stn struct_decls in
                   L.build_alloca (pointer_t sdef) (n^"_ptr") builder
            | _ -> L.build_alloca (ltype_of_typ t) n builder 
          in StringMap.add n local_var m 
      in
      let formal_list = List.map (fun c -> match c with 
                                              Primdecl(a,b) -> (a,b)
                                            | Strudecl(a,b) -> (SStruct(a),b)) fdecl.sformals
      in
      let formals = List.fold_left2 add_formal StringMap.empty formal_list
                    (Array.to_list (L.params the_function)) 
      in List.fold_left add_local formals (List.filter (fun s -> not (List.mem s formal_list))
                    (List.map (fun c -> match c with 
                      Primdecl(a,b) -> (a,b)
                    | Strudecl(a,b) -> (SStruct(a),b))  fdecl.slocals))
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = StringMap.find n local_vars 
    in

    let stru_name =
      List.fold_left (fun tmp c -> match c with
      | (v,stn) -> StringMap.add v stn tmp) StringMap.empty fdecl.strlist
    in

    let mat_sizes =
      List.fold_left (fun tmp c -> match c with
      | (s,(r,c)) -> StringMap.add s (r,c) tmp) StringMap.empty fdecl.smatsiz 
    in

    let lookup_size n = match n with
      | (A.SMatrix (r,c),_) -> (r,c)
      | _ -> (0,0)
    in

    let find_size_inmap n = 
      try StringMap.find n mat_sizes with 
      Not_found -> raise(Failure("Not found the matrix size"))
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

    let build_default_mat (r,c) builder = 
      let mat = L.build_array_malloc float_t (L.const_int i32_t (r*c)) "system_mat" builder in
      (*try cast pointer*)
      (*for x = 0 to (r*c-1) do
        let element_ptr = L.build_gep mat [|(L.const_int i32_t x)|] "element_ptr" builder in 
        ignore(L.build_store (L.const_float float_t 0.0) element_ptr builder)
      done*)
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

    let mat_float_operation m1 r1 c1 num builder = 
      let mat1 = L.build_load (L.build_struct_gep m1 0 "m_mat" builder) "mat1" builder in
      let res_mat = build_default_mat (r1,c1) builder in (* Change the r and c size here *)
      let res = L.build_load (L.build_struct_gep res_mat 0 "m_mat" builder) "mat" builder in
      ignore(L.build_call iter1mat_cpp_func [| mat1;res;num;(L.const_int i32_t (r1*c1)) |] "" builder);
      (*for i = 0 to r1*c1-1 do
        let m1_ele_ptr_ptr = L.build_gep mat1 [|L.const_int i32_t i|] "element_ptr_ptr" builder in
        let m1_ele_ptr = L.build_load m1_ele_ptr_ptr "element_ptr" builder in
        let res_ptr_ptr = L.build_gep res [|L.const_int i32_t i|] "res_ptr_ptr" builder in
        let tmp_ele = L.build_fmul m1_ele_ptr num "tmp_ele" builder in ignore(L.build_store tmp_ele res_ptr_ptr builder)
      done*); res_mat
    in
        
    let mat_mat_operation m1 m2 r1 c1 r2 c2 op if_elewise builder =
      match if_elewise with 
      | "yes" ->        
        let mat1 = L.build_load (L.build_struct_gep m1 0 "m_mat" builder) "mat1" builder in
        let mat2 = L.build_load (L.build_struct_gep m2 0 "m_mat" builder) "mat2" builder in
        let res_mat = build_default_mat (r1,c1) builder in (* Change the r and c size here *)
        let res = L.build_load (L.build_struct_gep res_mat 0 "m_mat" builder) "mat" builder in
        ignore(L.build_call iter2mat_cpp_func [| mat1;mat2;res;(L.const_int i32_t op);(L.const_int i32_t (r1*c1)) |] "" builder);
        res_mat

      | "not" -> 
        let mat1 = L.build_load (L.build_struct_gep m1 0 "m_mat" builder) "mat1" builder in
        let mat2 = L.build_load (L.build_struct_gep m2 0 "m_mat" builder) "mat2" builder in
        let res_mat = build_default_mat (r1,c2) builder in
        let res = L.build_load (L.build_struct_gep res_mat 0 "m_mat" builder) "mat" builder in
        ignore(L.build_call matmul_cpp_func [| mat1;mat2;res;(L.const_int i32_t r1);(L.const_int i32_t c1);(L.const_int i32_t r2);(L.const_int i32_t c2) |] "" builder);
        res_mat
 
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder ((typ, e) : sexpr) = match e with
	      SIntlit i  -> L.const_int i32_t i
      | SBoolit b  -> L.const_int i1_t (if b then 1 else 0)
      | SFloatlit l -> L.const_float float_t l
      | SStringlit s -> L.build_global_stringptr s "tmp" builder

      | SMatrixlit (f_array,(r,c)) -> (build_matrix_lit (f_array,(r,c)) builder)
      | SMataccess (s,e1,e2) ->
        let idx = 
          let e1' = expr builder e1 and e2' = expr builder e2 in
          let ptr = L.build_load (lookup s) s builder in
          let mat = L.build_load (L.build_struct_gep ptr 0 "m_mat" builder) "mat" builder in
          let r = L.build_load (L.build_struct_gep ptr 1 "m_r" builder) "r_mat" builder in
          let c = L.build_load (L.build_struct_gep ptr 2 "m_c" builder) "c_mat" builder in
          let index = L.build_add e2' (L.build_mul e1' c "tmp" builder) "index" builder in
          let (fdef, _) = StringMap.find "index_check" function_decls in
          ignore(L.build_call fdef [| e1'; r |] "" builder);
          ignore(L.build_call fdef [| e2'; c |] "" builder);
          L.build_gep mat [|index|] "element_ptr_ptr" builder
        in
        L.build_load idx "element_ptr" builder

      (* add matrix slicing here*)
      | SMatslicing(s,e1,e2) ->
        (match (e1,e2) with 
          | ((_,SRange(rs1,rt1)),(_,SRange(rs2,rt2))) ->
            (let (r,c) = find_size_inmap s in
            
            let (rs1,rt1) = (match e1 with (_,SRange(rs1,rt1)) -> (rs1,rt1)) in
            let s1 = match rs1 with SBeg -> 0 | SEnd -> r-1 | SInd(s1) -> s1 in
            let t1 = match rt1 with SBeg -> 0 | SEnd -> r-1 | SInd(t1) -> t1 in

            let (rs2,rt2) = (match e2 with (_,SRange(rs2,rt2)) -> (rs2,rt2)) in
            let s2 = match rs2 with SBeg -> 0 | SEnd -> c-1 | SInd(s2) -> s2 in
            let t2 = match rt2 with SBeg -> 0 | SEnd -> c-1 | SInd(t2) -> t2 in

            let ptr = L.build_load (lookup s) s builder in
            let mat = L.build_load (L.build_struct_gep ptr 0 "m_mat" builder) "mat" builder in

            let res_mat = build_default_mat ((t1-s1+1),(t2-s2+1)) builder in
            let res = L.build_load (L.build_struct_gep res_mat 0 "m_mat" builder) "mat" builder in
            
            let pointer = ref 0 in

            (for i = 0 to r-1 do
              (for j = 0 to c-1 do
                let ele_ptr_ptr = (L.build_gep mat [|L.const_int i32_t (i*c+j)|] "element_ptr_ptr" builder) in
                let ele = L.build_load ele_ptr_ptr "element_ptr" builder in

                (if ((s1<=i) && (i<=t1) && (s2<=j) && (j<=t2)) then (
                  let res_ptr_ptr = L.build_gep res [|L.const_int i32_t (!pointer)|] "res_ptr_ptr" builder in
                  ignore(L.build_store ele res_ptr_ptr builder);
                  pointer := !pointer+1;
                ))
              done);
            done);res_mat)

          | (e1,(_,SRange(rs2,rt2))) ->
            (let (r,c) = find_size_inmap s in
            let (rs2,rt2) = (match e2 with (_,SRange(rs2,rt2)) -> (rs2,rt2)) in
            let s2 = match rs2 with SBeg -> 0 | SEnd -> c-1 | SInd(s2) -> s2 in
            let t2 = match rt2 with SBeg -> 0 | SEnd -> c-1 | SInd(t2) -> t2 in

            let ptr = L.build_load (lookup s) s builder in
            let mat = L.build_load (L.build_struct_gep ptr 0 "m_mat" builder) "mat" builder in

            let res_mat = build_default_mat (1,(t2-s2+1)) builder in
            let res = L.build_load (L.build_struct_gep res_mat 0 "m_mat" builder) "mat" builder in
            
            let pointer = ref 0 in
            
            let i = expr builder e1 in

            (for j = 0 to c-1 do
              let index = L.build_add (L.build_mul i (L.const_int i32_t c) "mul_tmp" builder) (L.const_int i32_t j) "add_tmp" builder in
              let ele_ptr_ptr = (L.build_gep mat [|index|] "element_ptr_ptr" builder) in
              let ele = L.build_load ele_ptr_ptr "element_ptr" builder in

              (if ((s2<=j) && (j<=t2)) then (
                let res_ptr_ptr = L.build_gep res [|L.const_int i32_t (!pointer)|] "res_ptr_ptr" builder in
                ignore(L.build_store ele res_ptr_ptr builder);
                pointer := !pointer+1;
              ))
            done);res_mat)
            
          | ((_,SRange(rs1,rt1)),e2) ->
            (let (r,c) = find_size_inmap s in
            let (rs1,rt1) = (match e1 with (_,SRange(rs1,rt1)) -> (rs1,rt1)) in
            let s1 = match rs1 with SBeg -> 0 | SEnd -> r-1 | SInd(s1) -> s1 in
            let t1 = match rt1 with SBeg -> 0 | SEnd -> r-1 | SInd(t1) -> t1 in

            let ptr = L.build_load (lookup s) s builder in
            let mat = L.build_load (L.build_struct_gep ptr 0 "m_mat" builder) "mat" builder in

            let res_mat = build_default_mat ((t1-s1+1),1) builder in
            let res = L.build_load (L.build_struct_gep res_mat 0 "m_mat" builder) "mat" builder in
            
            let pointer = ref 0 in
            
            let j = expr builder e2 in

            (for i = 0 to r-1 do
              let index = L.build_add (L.build_mul (L.const_int i32_t i) (L.const_int i32_t c) "mul_tmp" builder) j "add_tmp" builder in
              let ele_ptr_ptr = (L.build_gep mat [|index|] "element_ptr_ptr" builder) in
              let ele = L.build_load ele_ptr_ptr "element_ptr" builder in

              (if ((s1<=i) && (i<=t1)) then (
                let res_ptr_ptr = L.build_gep res [|L.const_int i32_t (!pointer)|] "res_ptr_ptr" builder in
                ignore(L.build_store ele res_ptr_ptr builder);
                pointer := !pointer+1;
              ))
            done);res_mat)
        )

      | SEmpty     -> L.const_int i32_t 0
      | SVar s     -> L.build_load (lookup s) s builder

      | SStruaccess(vname, member) -> let stn = StringMap.find vname stru_name in
        let (sdef,sdecl) = StringMap.find stn struct_decls in
        let mem_idx = 
          let rec find_idx m mlist = match mlist with
              [] -> raise (Failure ("unrecognized struct member " ^ m))
            | Primdecl(_, nm) :: tl -> if m = nm then 0 else 1 + find_idx m tl
          in find_idx member sdecl.sstvar
        in let str_ptr = L.build_load (lookup vname) vname builder
        in L.build_load (L.build_struct_gep str_ptr mem_idx (stn ^ member) builder) (vname ^ member) builder

      | SAssign (s, e) -> 
        let e' = expr builder e in 
          (match s with 
            | (_,SVar(s1)) -> ignore(L.build_store e' (lookup s1) builder); e'
            | (_,SStruaccess(vname, member)) -> 
                let mem_ptr = 
                let stn = StringMap.find vname stru_name in
                let (sdef,sdecl) = StringMap.find stn struct_decls in
                let mem_idx = 
                  let rec find_idx m mlist = match mlist with
                      [] -> raise (Failure ("unrecognized struct member " ^ m))
                    | Primdecl(_, nm) :: tl -> if m = nm then 0 else 1 + find_idx m tl
                  in find_idx member sdecl.sstvar
                in let str_ptr = L.build_load (lookup vname) vname builder
                in L.build_struct_gep str_ptr mem_idx (stn ^ member) builder
                in ignore(L.build_store e' mem_ptr builder); e'
            | (_,SMataccess (s,e1,e2)) ->
                 let idx = 
                   let e1' = expr builder e1 and e2' = expr builder e2 in
                   let ptr = L.build_load (lookup s) s builder in
                   let mat = L.build_load (L.build_struct_gep ptr 0 "m_mat" builder) "mat" builder in
                   let r = L.build_load (L.build_struct_gep ptr 1 "m_r" builder) "r_mat" builder in
                   let c = L.build_load (L.build_struct_gep ptr 2 "m_c" builder) "c_mat" builder in
                   let index = L.build_add e2' (L.build_mul e1' c "tmp" builder) "index" builder in
                   let (fdef, _) = StringMap.find "index_check" function_decls in
                   ignore(L.build_call fdef [| e1'; r |] "" builder);
                   ignore(L.build_call fdef [| e2'; c |] "" builder);
                   L.build_gep mat [|index|] "element_ptr_ptr" builder
                  in
                  ignore(L.build_store e' idx builder); e'                 
            | _ -> raise (Failure "Assign Failiure!"))

      | SBinop (e1, op, e2) ->
      (let c1 = expr builder e1 in
      let c2 = expr builder e2 in

      let check1 = is_matrix c1 in
      let check2 = is_matrix c2 in
      match (check1,check2) with
        | (false,false) ->
          (match e1 with
            | (A.Float,_) ->
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
            | _ ->
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
              ) e1' e2' "tmp" builder)
        | (true, false) ->
           let (r1,c1) = lookup_size e1 in
           let e1' = expr builder e1 and e2' = expr builder e2 in
           (match op with
            | A.Mult -> mat_float_operation e1' r1 c1 e2' builder
            | A.Div -> mat_float_operation e1' r1 c1 (L.build_fdiv (L.const_float float_t 1.0) e2' "tmp" builder) builder
           )
        | (false, true) ->
           let (r2,c2) = lookup_size e2 in
           let e1' = expr builder e1 and e2' = expr builder e2 in
           (match op with
            | A.Mult -> mat_float_operation e2' r2 c2 e1' builder)
        | (true,true) ->
          let (r1,c1) = lookup_size e1 in
          let (r2,c2) = lookup_size e2 in
          let e1' = expr builder e1 and e2' = expr builder e2 in
          (match op with
              A.Add -> mat_mat_operation e1' e2' r1 c1 r2 c2 0 "yes" builder
            | A.Sub -> mat_mat_operation e1' e2' r1 c1 r2 c2 1 "yes" builder
            | A.Mult -> mat_mat_operation e1' e2' r1 c1 r2 c2 4 "not" builder
            | A.Elemult -> mat_mat_operation e1' e2' r1 c1 r2 c2 2 "yes" builder
            | A.Elediv -> mat_mat_operation e1' e2' r1 c1 r2 c2 3 "yes" builder
          )
      )

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


      | SCall ("imread", [s;e;e2;e3]) ->
        let path = expr builder s in
        let str_ptr = expr builder e in
        let r = match e2 with (_,SIntlit(r)) -> r in
        let c = match e3 with (_,SIntlit(c)) -> c in
      
        let mat1_ptr = L.build_load (L.build_struct_gep str_ptr 0 ("R") builder) ("sR") builder in
        let mat2_ptr = L.build_load (L.build_struct_gep str_ptr 1 ("G") builder) ("sG") builder in
        let mat3_ptr = L.build_load (L.build_struct_gep str_ptr 2 ("B") builder) ("sB") builder in
        let mat1 = L.build_load (L.build_struct_gep (mat1_ptr) 0 "m_mat" builder) "mat_mat" builder in
        let mat2 = L.build_load (L.build_struct_gep (mat2_ptr) 0 "m_mat" builder) "mat_mat" builder in
        let mat3 = L.build_load (L.build_struct_gep (mat3_ptr) 0 "m_mat" builder) "mat_mat" builder in
        ignore (L.build_call load_cpp_func [| path;mat1;mat2;mat3 |] "" builder );str_ptr


      | SCall ("imwrite", [s;e]) ->
        let path = expr builder s in
        let str_ptr = expr builder e in
        let mat1_ptr = L.build_load (L.build_struct_gep str_ptr 0 ("R") builder) ("sR") builder in
        let mat2_ptr = L.build_load (L.build_struct_gep str_ptr 1 ("G") builder) ("sG") builder in
        let mat3_ptr = L.build_load (L.build_struct_gep str_ptr 2 ("B") builder) ("sB") builder in
        let mat1 = L.build_load (L.build_struct_gep (mat1_ptr) 0 "m_mat" builder) "mat_mat" builder in
        let mat2 = L.build_load (L.build_struct_gep (mat2_ptr) 0 "m_mat" builder) "mat_mat" builder in
        let mat3 = L.build_load (L.build_struct_gep (mat3_ptr) 0 "m_mat" builder) "mat_mat" builder in
        let r = L.build_load (L.build_struct_gep (mat1_ptr) 1 "m_mat" builder) "mat_mat" builder in
        let c = L.build_load (L.build_struct_gep (mat1_ptr) 2 "m_mat" builder) "mat_mat" builder in

        ignore(L.build_call save_cpp_func [| path;mat1;mat2;mat3;r;c |] "" builder);mat1

      | SCall("abort",[]) -> 
        ignore(L.build_call printf_func [| string_format_str ; L.build_global_stringptr "Error: Matrix index out of bound" "tmp" builder|] "printf" builder);
        L.build_call abort_func [| |] "" builder

      | SCall ("height",[e]) ->
        let r = L.build_load (L.build_struct_gep (expr builder e) 1 "m_r" builder) "r_mat" builder in
        r
      
      | SCall ("width", [e]) ->
        let c = L.build_load (L.build_struct_gep (expr builder e) 2 "m_c" builder) "c_mat" builder in
        c

      | SCall ("sum", [e]) ->
        let (r,c) = lookup_size e in
        let mat = L.build_load (L.build_struct_gep (expr builder e) 0 "m_mat" builder) "mat_mat" builder in

        let sum = L.build_alloca float_t "sumOfEle" builder in
        (for x=0 to r*c-1 do
          let ele_ptr_ptr = (L.build_gep mat [|L.const_int i32_t x|] "element_ptr_ptr" builder) in
          let ele = L.build_load ele_ptr_ptr "element_ptr" builder in
          let tmp_sum = L.build_fadd (L.build_load sum "addsum" builder) ele "tmp_sum" builder in 
            ignore(L.build_store tmp_sum sum builder);
        done);
        L.build_load sum "addsum" builder
        
      | SCall ("mean", [e]) ->
        let (r,c) = lookup_size e in
        let mat = L.build_load (L.build_struct_gep (expr builder e) 0 "m_mat" builder) "mat_mat" builder in

        let sum = L.build_alloca float_t "sumOfEle" builder in
        (for x=0 to r*c-1 do
          let ele_ptr_ptr = (L.build_gep mat [|L.const_int i32_t x|] "element_ptr_ptr" builder) in
          let ele = L.build_load ele_ptr_ptr "element_ptr" builder in
          let tmp_sum = L.build_fadd (L.build_load sum "addsum" builder) ele "tmp_sum" builder in 
            ignore(L.build_store tmp_sum sum builder);
        done);
        L.build_fdiv (L.build_load sum "addsum" builder) (L.const_float float_t 4.0) "mean_sum" builder

      | SCall ("trans", [e]) ->
        let (r,c) = lookup_size e in
        let mat = L.build_load (L.build_struct_gep (expr builder e) 0 "m_mat" builder) "mat_mat" builder in
        let res_mat = build_default_mat (c,r) builder in
        let res = L.build_load (L.build_struct_gep res_mat 0 "m_mat" builder) "mat" builder in
        ignore (L.build_call trans_cpp_func [| mat;res;(L.const_int i32_t r);(L.const_int i32_t c) |] "" builder );
        res_mat
      
      | SCall ("cov_openCV", [k;e1;e2;sd]) ->
        let fromPath = expr builder e1 in
        let toPath = expr builder e2 in
        let d = expr builder sd in
        let str_ptr = expr builder k in
        let kernel = L.build_load (L.build_struct_gep (str_ptr) 0 "k_mat" builder) "mat_mat_k" builder in
        L.build_call filter_cpp_func [| kernel; fromPath; toPath;d |] "" builder 

      | SCall ("cov", [e1;e2]) ->
        let (r1,c1) = lookup_size e1 in
        let (r2,c2) = lookup_size e2 in
        let d = (r2-1)/2 in
        let mat1 = L.build_load (L.build_struct_gep (expr builder e1) 0 "m_mat" builder) "mat_mat" builder in
        let mat2 = L.build_load (L.build_struct_gep (expr builder e2) 0 "m_mat" builder) "mat_mat" builder in
        let res_mat = build_default_mat (r1,c1) builder in
        let res = L.build_load (L.build_struct_gep res_mat 0 "m_mat" builder) "mat" builder in

        let tmp_mat = build_default_mat (r1+2*d,c1+2*d) builder in
        let tmp = L.build_load (L.build_struct_gep tmp_mat 0 "m_mat" builder) "mat" builder in

        (for i=0 to (r1+d) do
          (for j=0 to (c1+d) do
            let ele = 
              (if (((i-d)<0) || ((j-d)<0) || ((i+d)>(r1+d)) || ((j+d)>(c1+d))) then (L.const_float float_t 0.0)
              else (let ele_ptr_ptr = (L.build_gep mat1 [|L.const_int i32_t ((i-d)*c1+j-d)|] "element_ptr_ptr" builder) in
                    L.build_load ele_ptr_ptr "element_ptr" builder))
            in 
            let tmp_ptr_ptr = L.build_gep tmp [|L.const_int i32_t (i*(c1+2*d)+j)|] "tmp_ptr_ptr" builder in
            ignore(L.build_store ele tmp_ptr_ptr builder)
          done);
        done);
        
        (for i=d to r1 do
          (for j=d to c1 do
            let res_val_tmp = L.build_alloca float_t "sumOfEle" builder in
            let ptr = ref 0 in
            (for x=(i-d) to (i+d) do
              (for y=(j-d) to (j+d) do
                let mat2_ele_ptr_ptr = (L.build_gep mat2 [|L.const_int i32_t !ptr|] "mat2_ele_ptr_ptr" builder) in
                let mat2_ele = L.build_load mat2_ele_ptr_ptr "mat2_ele_ptr" builder in
                let tmp_ele_ptr_ptr = (L.build_gep tmp [|L.const_int i32_t (x*(c1+2*d)+y)|] "mat2_ele_ptr_ptr" builder) in
                let tmp_ele = L.build_load tmp_ele_ptr_ptr "tmp_ele_ptr" builder in

                let tmp_res = L.build_fadd (L.build_fmul mat2_ele tmp_ele "tmp_res" builder) 
                                           (L.build_load res_val_tmp "addres" builder) "new_res" builder in
                                           ignore(L.build_store tmp_res res_val_tmp builder);
                ptr := !ptr+1;
              done);
            done);
            ignore(L.build_store (L.build_load res_val_tmp "res" builder) 
                  (L.build_gep res [|L.const_int i32_t ((i-d)*(c1)+j-d)|] "tmp_ptr_ptr" builder) builder)
          done);
        done); res_mat

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
      | SDefaultmat (name, r, c) -> (L.build_store (build_default_mat (r,c) builder) (lookup name) builder);builder
      | SIniStrucct (var, strucname, mems) -> 
        let (sdef,_) = StringMap.find strucname struct_decls in
        let llmems = List.rev (List.map (expr builder) (List.rev mems)) in
        let str_ptr = L.build_malloc sdef (var ^ "_malloc") builder in
        let build_struct i llmem = 
          ignore(L.build_store llmem (L.build_struct_gep str_ptr i (var ^ "_" ^string_of_int i) builder) builder); i+1
        in ignore(List.fold_left build_struct 0 llmems);
        ignore(L.build_store str_ptr (lookup var) builder); builder
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