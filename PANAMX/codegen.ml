(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions, structs) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Panamx" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and void_t     = L.void_type   context
  and pointer_t  = L.pointer_type
  and array_t    = L.array_type
  and matrix_t   = L.pointer_type (L.named_struct_type context "Matrix")
  in

  (* Return the LLVM type for a MicroC type *)
  let ltype_of_typ = function
      A.Int    -> i32_t
    | A.Bool   -> i1_t
    | A.String -> pointer_t i8_t
    | A.Float  -> float_t
    | A.Void   -> void_t
    | A.Matrix -> matrix_t
    | A.Struct e -> pointer_t (L.named_struct_type context e)
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = match t with
          A.Float -> L.const_float (ltype_of_typ t) 0.0
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let struct_decls : (L.lltype * sstruct_decl) StringMap.t =
    let define_struct m sdecl =
      let get_sbody_ty ((ty, _) : A.bind) = ltype_of_typ ty in
      let sname = sdecl.ssname
      and svar_type = Array.of_list (List.map get_sbody_ty sdecl.ssvar) in
      let stype = L.named_struct_type context sname in
      L.struct_set_body stype svar_type false;
      StringMap.add sname (stype, sdecl) m in
    List.fold_left define_struct StringMap.empty structs
  in

  let printf_t : L.lltype =
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in

  (* let printbig_t : L.lltype =
      L.function_type i32_t [| i32_t |] in
  let printbig_func : L.llvalue =
      L.declare_function "printbig" printbig_t the_module in *)

  let build_matrix_empty_t : L.lltype =
      L.function_type matrix_t [| i32_t; i32_t |] in
  let build_matrix_empty_func : L.llvalue =
      L.declare_function "buildMatrixEmpty" build_matrix_empty_t the_module in

  let matrix_access_t : L.lltype =
      L.function_type float_t [| matrix_t; i32_t; i32_t |] in
  let matrix_access_func : L.llvalue =
      L.declare_function "matrixAccess" matrix_access_t the_module in

  let matrix_assign_t : L.lltype =
      L.function_type float_t [| matrix_t; i32_t; i32_t; float_t |] in
  let matrix_assign_funct : L.llvalue =
      L.declare_function "matrixAssign" matrix_assign_t the_module in

  let matrix_slice_t : L.lltype =
      L.function_type matrix_t [| matrix_t; i32_t; i32_t; i32_t; i32_t |] in
  let matrix_slice_func : L.llvalue =
      L.declare_function "matrixSlice" matrix_slice_t the_module in

  let print_matrix_t : L.lltype =
      L.function_type i32_t [| matrix_t |] in
  let print_matrix_func : L.llvalue =
      L.declare_function "printMatrix" print_matrix_t the_module in

  let free_matrix_t : L.lltype =
      L.function_type i32_t [| matrix_t |] in
  let free_matrix_func : L.llvalue =
      L.declare_function "freeMatrix" free_matrix_t the_module in

  let matrix_height_t : L.lltype =
      L.function_type i32_t [| matrix_t |] in
  let matrix_height_func : L.llvalue =
      L.declare_function "getHeight" matrix_height_t the_module in

  let matrix_width_t : L.lltype =
      L.function_type i32_t [| matrix_t |] in
  let matrix_width_func : L.llvalue =
      L.declare_function "getWidth" matrix_width_t the_module in

  let add_md_t : L.lltype =
      L.function_type matrix_t [| matrix_t; float_t; i32_t |] in
  let add_md_func : L.llvalue =
      L.declare_function "addMatrixDouble" add_md_t the_module in

  let add_mm_t : L.lltype =
      L.function_type matrix_t [| matrix_t; matrix_t; i32_t |] in
  let add_mm_func : L.llvalue =
      L.declare_function "addMatrixMatrix" add_mm_t the_module in

  let sub_dm_t : L.lltype =
      L.function_type matrix_t [| float_t; matrix_t |] in
  let sub_dm_func : L.llvalue =
      L.declare_function "subDoubleMatrix" sub_dm_t the_module in

  let mul_md_t : L.lltype =
      L.function_type matrix_t [| matrix_t; float_t; i32_t |] in
  let mul_md_func : L.llvalue =
      L.declare_function "mulMatrixDouble" mul_md_t the_module in

  let mul_mm_t : L.lltype =
      L.function_type matrix_t [| matrix_t; matrix_t |] in
  let mul_mm_func : L.llvalue =
      L.declare_function "mulMatrixMatrix" mul_mm_t the_module in

  let mul_ew_mm_t : L.lltype =
      L.function_type matrix_t [| matrix_t; matrix_t; i32_t |] in
  let mul_ew_mm_func : L.llvalue =
      L.declare_function "mulElementWiseMatrix" mul_ew_mm_t the_module in

  let matrix_sum_t : L.lltype =
      L.function_type float_t [| matrix_t |] in
  let matrix_sum_func : L.llvalue =
      L.declare_function "sum" matrix_sum_t the_module in

  let matrix_mean_t : L.lltype =
      L.function_type float_t [| matrix_t |] in
  let matrix_mean_func : L.llvalue =
      L.declare_function "mean" matrix_mean_t the_module in

  let matrix_trans_t : L.lltype =
      L.function_type matrix_t [| matrix_t |] in
  let matrix_trans_func : L.llvalue =
      L.declare_function "trans" matrix_trans_t the_module in

  let matrix_rref_t : L.lltype =
      L.function_type matrix_t [| matrix_t |] in
  let matrix_rref_func : L.llvalue =
      L.declare_function "rref" matrix_rref_t the_module in

  let matrix_rank_t : L.lltype =
      L.function_type float_t [| matrix_t |] in
  let matrix_rank_func : L.llvalue =
      L.declare_function "rank" matrix_rank_t the_module in

  let matrix_det_t : L.lltype =
      L.function_type float_t [| matrix_t |] in
  let matrix_det_func : L.llvalue =
      L.declare_function "det" matrix_det_t the_module in

  let matrix_inv_t : L.lltype =
      L.function_type matrix_t [| matrix_t |] in
  let matrix_inv_func : L.llvalue =
    L.declare_function "inv" matrix_inv_t the_module in

  let matrix_concatTB_t : L.lltype =
      L.function_type matrix_t [| matrix_t; matrix_t |] in
  let matrix_concatTB_func : L.llvalue =
    L.declare_function "concatTB" matrix_concatTB_t the_module in

  let matrix_concatLR_t : L.lltype =
      L.function_type matrix_t [| matrix_t; matrix_t |] in
  let matrix_concatLR_func : L.llvalue =
    L.declare_function "concatLR" matrix_concatLR_t the_module in

  let int_sqrti_t : L.lltype =
      L.function_type i32_t [| i32_t |] in
  let int_sqrti_func : L.llvalue =
      L.declare_function "sqrti" int_sqrti_t the_module in

  let float_sqrtd_t : L.lltype =
      L.function_type float_t [| float_t |] in
  let float_sqrtd_func : L.llvalue =
      L.declare_function "sqrtd" float_sqrtd_t the_module in

  let nrooti_t : L.lltype =
      L.function_type i32_t [| i32_t ; i32_t |] in
  let nrooti_func : L.llvalue =
      L.declare_function "nrooti" nrooti_t the_module in

  let nrootd_t : L.lltype =
      L.function_type i32_t [| i32_t ; float_t |] in
  let nrootd_func : L.llvalue =
      L.declare_function "nrootd" nrootd_t the_module in

  let absi_t : L.lltype =
      L.function_type i32_t [| i32_t |] in
  let absi_func : L.llvalue =
      L.declare_function "absi" absi_t the_module in

  let absd_t : L.lltype =
      L.function_type float_t [| float_t |] in
  let absd_func : L.llvalue =
      L.declare_function "absd" absd_t the_module in

  let poweri_t : L.lltype =
      L.function_type i32_t [| i32_t ; i32_t |] in
  let poweri_func : L.llvalue =
      L.declare_function "poweri" poweri_t the_module in

  let powerd_t : L.lltype =
      L.function_type float_t [| float_t ; i32_t|] in
  let powerd_func : L.llvalue =
      L.declare_function "powerd" powerd_t the_module in


  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder
    and string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

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
      let local_var = match t with
          A.Struct e ->
            let (sty, _) = StringMap.find e struct_decls
            in L.build_alloca (pointer_t sty) n builder
        | _ -> L.build_alloca (ltype_of_typ t) n builder
      in StringMap.add n local_var m
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals
    in

    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
                  StringMap.empty (fdecl.sformals @ fdecl.slocals)
    in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder ((_, e) : sexpr) = match e with
	      SLiteral i  -> L.const_int i32_t i
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SStrLit s   -> L.build_global_stringptr s "tmp" builder
      | SFliteral l -> L.const_float_of_string float_t l
      | SNoexpr     -> L.const_int i32_t 0
      | SId s       -> L.build_load (lookup s) s builder

      | SMatLit (rows, cols, e) ->
        let build_matrix_t : L.lltype =
            L.function_type matrix_t [| i32_t; i32_t; pointer_t float_t |] in
        let build_matrix_func : L.llvalue =
            L.declare_function "buildMatrix" build_matrix_t the_module in
        let llarray = L.const_array float_t (Array.of_list (List.map (int_to_float builder) e)) in
        let array_ptr_ty = array_t float_t (rows * cols) in
        let llptr = L.build_alloca array_ptr_ty "matrix_array" builder in
        ignore(L.build_store llarray llptr builder);
        let ptr = L.build_gep llptr [| L.const_int i32_t 0; L.const_int i32_t 0 |] "get_array_ptr" builder in
        L.build_call build_matrix_func
          [| L.const_int i32_t rows; L.const_int i32_t cols; ptr |] "init_matrix" builder

      | SMatLitEmpty (i, j) ->
        let rows = expr builder i
        and cols = expr builder j in
        L.build_call build_matrix_empty_func [| rows; cols |] "init_matrix_empty" builder

      | SMatIndex (s, i, j) ->
        let i' = expr builder i
        and j' = expr builder j
        and s' = L.build_load (lookup s) s builder in
        L.build_call matrix_access_func [| s'; i'; j' |] "matrix_access" builder

      | SMatAssign (s, i, j, e) ->
        let i' = expr builder i
        and j' = expr builder j
        and e' = int_to_float builder e
        and s' = L.build_load (lookup s) s builder in
        L.build_call matrix_assign_funct [| s'; i'; j'; e' |] "matrix_assign" builder

      | SMatSlice (s, i, j, k, l) ->
        let i' = expr builder i
        and j' = expr builder j
        and k' = expr builder k
        and l' = expr builder l
        and s' = L.build_load (lookup s) s builder in
        L.build_call matrix_slice_func [| s'; i'; j'; k'; l' |] "matrix_slice" builder

      | SStructLit id ->
        let (sty, _) = StringMap.find id struct_decls
        in L.build_malloc sty (id ^ "_body") builder

      | SMember (s, m) -> let mem_p = get_smem_ptr builder s m in
        L.build_load mem_p (s ^ m) builder

      | SMemAssign (s, m, e) ->
        let e' = expr builder e in
        let mem_p = get_smem_ptr builder s m in
        ignore(L.build_store e' mem_p builder); e'

      | SAssign (s, e) -> let e' = expr builder e in
                          ignore(L.build_store e' (lookup s) builder); e'
      | SBinop (((ty1, _) as e1), op, ((ty2, _) as e2)) when ty1 = A.Int && ty2 = A.Int ->
        let e1' = expr builder e1
        and e2' = expr builder e2 in
          (match op with
            A.Add     -> L.build_add
          | A.Sub     -> L.build_sub
          | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
          | A.Mod     -> L.build_srem
          | A.And     -> L.build_and
          | A.Or      -> L.build_or
          | A.Equal   -> L.build_icmp L.Icmp.Eq
          | A.Neq     -> L.build_icmp L.Icmp.Ne
          | A.Less    -> L.build_icmp L.Icmp.Slt
          | A.Leq     -> L.build_icmp L.Icmp.Sle
          | A.Greater -> L.build_icmp L.Icmp.Sgt
          | A.Geq     -> L.build_icmp L.Icmp.Sge
          | _         -> raise (Failure "should not happen")
          ) e1' e2' "tmp" builder
      | SBinop (((ty1, _) as e1), op, ((ty2, _) as e2)) when ty1 = A.Bool && ty2 = A.Bool ->
        let e1' = expr builder e1
        and e2' = expr builder e2 in
          (match op with
            A.And   -> L.build_and
          | A.Or    -> L.build_or
          | A.Equal -> L.build_icmp L.Icmp.Eq
          | A.Neq   -> L.build_icmp L.Icmp.Ne
          | _       -> raise (Failure "invalid operation on bool")
          ) e1' e2' "tmp" builder
      | SBinop (((ty1, _) as e1), op, ((ty2, _) as e2)) when ty1 = A.Matrix || ty2 = A.Matrix ->
        let e1' = int_to_float builder e1
        and e2' = int_to_float builder e2 in
          (match op with
            A.Add  ->
              if ty1 = ty2 && ty1 = A.Matrix
              then L.build_call add_mm_func [| e1'; e2'; L.const_int i32_t 0 |] "add_matrix" builder
              else if ty1 = A.Matrix
              then L.build_call add_md_func [| e1'; e2'; L.const_int i32_t 0 |] "add_matrix" builder
              else L.build_call add_md_func [| e2'; e1'; L.const_int i32_t 0 |] "add_matrix" builder
          | A.Sub  ->
              if ty1 = ty2 && ty1 = A.Matrix
              then L.build_call add_mm_func [| e1'; e2'; L.const_int i32_t 1 |] "sub_matrix" builder
              else if ty1 = A.Matrix
              then L.build_call add_md_func [| e1'; e2'; L.const_int i32_t 1 |] "sub_matrix" builder
              else L.build_call sub_dm_func [| e1'; e2' |] "sub_matrix" builder
          | A.Mult ->
              if ty1 = ty2 && ty1 = A.Matrix
              then L.build_call mul_mm_func [| e1'; e2' |] "mul_matrix" builder
              else if ty1 = A.Matrix
              then L.build_call mul_md_func [| e1'; e2'; L.const_int i32_t 0 |] "mul_matrix" builder
              else L.build_call mul_md_func [| e2'; e1'; L.const_int i32_t 0 |] "mul_matrix" builder
          | A.Div ->
              if ty1 = A.Matrix && ty2 != A.Matrix
              then L.build_call mul_md_func [| e1'; e2'; L.const_int i32_t 1 |] "div_matrix" builder
              else raise (Failure "illegal operation / on matrix")
          | A.Mmul when ty1 = ty2 && ty1 = A.Matrix ->
              L.build_call mul_ew_mm_func [| e1'; e2'; L.const_int i32_t 0 |] "elewise_matrix" builder
          | A.Mdiv when ty1 = ty2 && ty1 = A.Matrix ->
              L.build_call mul_ew_mm_func [| e1'; e2'; L.const_int i32_t 1 |] "elewise_matrix" builder
          | _      -> raise (Failure
              ("illegal operation " ^ (A.string_of_op op) ^ " on matrix"))
          )
      | SBinop (e1, op, e2) ->
        let e1' = int_to_float builder e1
        and e2' = int_to_float builder e2 in
          (match op with
            A.Add     -> L.build_fadd
          | A.Sub     -> L.build_fsub
          | A.Mult    -> L.build_fmul
          | A.Div     -> L.build_fdiv
          | A.Mod     -> L.build_frem
          | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
          | A.Neq     -> L.build_fcmp L.Fcmp.One
          | A.Less    -> L.build_fcmp L.Fcmp.Olt
          | A.Leq     -> L.build_fcmp L.Fcmp.Ole
          | A.Greater -> L.build_fcmp L.Fcmp.Ogt
          | A.Geq     -> L.build_fcmp L.Fcmp.Oge
          | A.And | A.Or ->
              raise (Failure "internal error: semant should have rejected and/or on float")
          | _         -> raise (Failure "should not happen")
          ) e1' e2' "tmp" builder
      | SUnop(op, ((t, ex) as e)) ->
          let e' = expr builder e in
          (match op with
            A.Neg when t = A.Float -> L.build_fneg e' "tmp" builder
          | A.Neg                  -> L.build_neg e' "tmp" builder
          | A.Not                  -> L.build_not e' "tmp" builder
          | A.Inc                  ->
            L.build_store (L.build_add e' (L.const_int i32_t 1) "tmp" builder) (lookup (match ex with
              SId id -> id
            | _ -> raise (Failure "increment error"))) builder
          | A.Dec                  ->
            L.build_store (L.build_sub e' (L.const_int i32_t 1) "tmp" builder) (lookup (match ex with
              SId id -> id
            | _ -> raise (Failure "decrement error"))) builder
          )
      | SCall ("print", [e])
        (* -> let expr_val = expr builder e in
        (match e with ((typ, _) : sexpr) ->
           (match typ with
            | A.Matrix -> L.build_call print_matrix_func [| expr_val |] "print_matrix" builder
            | A.Int | A.Bool -> L.build_call printf_func [| int_format_str ; (expr_val) |]
                                  "printf" builder
            | A.Float -> L.build_call printf_func [| float_format_str ; (expr_val) |]
                           "printf" builder
            | A.String -> L.build_call printf_func [| string_format_str; (expr builder e) |]
                            "printf" builder
            | A.Void -> raise (Failure ("Void type cannot be printed"))
            | _ -> raise (Failure ("cannot print"))
           )) *)
          (* (match tt with
            A.Int
          | A.Bool -> L.build_call printf_func
              [| int_format_str ; (expr builder e) |] "printf" builder
          | A.Float -> L.build_call printf_func
              [| float_format_str ; (expr builder e) |] "printf" builder
          | A.String -> L.build_call printf_func
              [| string_format_str; (expr builder e) |] "printf" builder
             | _ -> raise (Failure "cannot print")) *)
      | SCall ("printb", [e]) ->
	      L.build_call printf_func [| int_format_str ; (expr builder e) |]
          "printf" builder
      | SCall ("printf", [e]) ->
        L.build_call printf_func [| float_format_str ; (expr builder e) |]
          "printf" builder
      | SCall ("prints", [e]) ->
        L.build_call printf_func [| string_format_str; (expr builder e) |]
          "printf" builder
      (* | SCall ("printbig", [e]) ->
        L.build_call printbig_func [| (expr builder e) |] "printbig" builder *)
      | SCall ("printm", [e]) ->
        L.build_call print_matrix_func [| expr builder e |] "print_matrix" builder
      | SCall ("free", [e]) ->
        L.build_call free_matrix_func [| expr builder e |] "free_matrix" builder
        (* L.build_free (expr builder e) builder *)
      | SCall ("matrixHeight", [e]) ->
        L.build_call matrix_height_func [| expr builder e |] "matrix_height" builder
      | SCall ("matrixWidth", [e]) ->
        L.build_call matrix_width_func [| expr builder e |] "matrix_width" builder

      | SCall ("sum", [e]) ->
        L.build_call matrix_sum_func [| expr builder e |] "matrix_sum" builder
      | SCall ("mean", [e]) ->
        L.build_call matrix_mean_func [| expr builder e |] "matrix_mean" builder
      | SCall ("trans", [e]) ->
        L.build_call matrix_trans_func [| expr builder e |] "matrix_trans" builder
      | SCall ("rref", [e]) ->
        L.build_call matrix_rref_func [| expr builder e |] "matrix_rref" builder
      | SCall ("rank", [e]) ->
        L.build_call matrix_rank_func [| expr builder e |] "matrix_rank" builder
      | SCall ("det", [e]) ->
        L.build_call matrix_det_func [| expr builder e |] "matrix_det" builder
      | SCall ("inv", [e]) ->
        L.build_call matrix_inv_func [| expr builder e |] "matrix_inv" builder
      | SCall ("concatTB", [e1; e2]) ->
        L.build_call matrix_concatTB_func [| expr builder e1; expr builder e2 |] "matrix_concatTB" builder
      | SCall ("concatLR", [e1; e2]) ->
        L.build_call matrix_concatLR_func [| expr builder e1; expr builder e2 |] "matrix_concatLR" builder

      | SCall ("sqrti", [e]) ->
        L.build_call int_sqrti_func [| expr builder e |] "int_sqrti" builder
      | SCall ("sqrtd", [e]) ->
        L.build_call float_sqrtd_func [| expr builder e |] "float_sqrtd" builder
      | SCall ("nrooti", [e1; e2]) ->
        L.build_call nrooti_func [| expr builder e1; expr builder e2 |] "nrooti" builder
      | SCall ("nrootd", [e1; e2]) ->
        L.build_call nrootd_func [| expr builder e1; expr builder e2 |] "nrooti" builder
      | SCall ("absi", [e]) ->
        L.build_call absi_func [| expr builder e |] "absi" builder
      | SCall ("absd", [e]) ->
        L.build_call absd_func [| expr builder e|] "absd" builder
      | SCall ("poweri", [e1; e2]) ->
        L.build_call poweri_func [| expr builder e1; expr builder e2 |] "poweri" builder
      | SCall ("powerd", [e1; e2]) ->
        L.build_call powerd_func [| expr builder e1; expr builder e2 |] "powerd" builder

      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
          let llargs = List.rev (List.map (expr builder) (List.rev args)) in
          let result = (match fdecl.styp with
                        A.Void -> ""
                      | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list llargs) result builder

    and int_to_float builder e =
          let e' = expr builder e in
          match fst e with
            A.Int -> L.build_sitofp e' float_t "tmp" builder
          | _     -> e'

    (* get the member m's pointer in struct *)
    and get_smem_ptr builder s m =
          let sname = match (type_of_identifier s) with
                A.Struct n -> n
              | _ -> raise (Failure ("Invalid access(.) operation for " ^ s)) in
          let (_, sdecl) = StringMap.find sname struct_decls in
          let idx =
            let rec find_idx = function
                [] -> raise (Failure ("Struct " ^ sname ^ " does not have member " ^ m))
              | (_, var) :: _ when var = m -> 0
              | _ :: tl -> 1 + find_idx tl
            in find_idx sdecl.ssvar in
          let struct_p = L.build_load (lookup s) s builder in
          L.build_struct_gep struct_p idx (sname ^ m) builder

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
      | SReturn e -> ignore(match fdecl.styp with
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
      in

    (* Build the code for each statement in the function *)
      let builder = stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
      add_terminal builder (match fdecl.styp with
          A.Void -> L.build_ret_void
        | A.Float -> L.build_ret (L.const_float float_t 0.0)
        | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
