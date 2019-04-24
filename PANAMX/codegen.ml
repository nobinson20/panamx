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
let translate (globals, functions) =
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
  let rec ltype_of_typ = function
      A.Int    -> i32_t
    | A.Bool   -> i1_t
    | A.String -> pointer_t i8_t
    | A.Float  -> float_t
    | A.Void   -> void_t
    | A.Arrays(ty, len) -> array_t (ltype_of_typ ty) len
    | A.Matrix -> matrix_t
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = match t with
          A.Float -> L.const_float (ltype_of_typ t) 0.0
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

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

  let print_matrix_t : L.lltype =
      L.function_type i32_t [| matrix_t |] in
  let print_matrix_func : L.llvalue =
      L.declare_function "printMatrix" print_matrix_t the_module in

  (* let free_matrix_t : L.lltype =
      L.function_type i32_t [| matrix_t |] in
  let free_matrix_func : L.llvalue =
      L.declare_function "freeMatrix" free_matrix_t the_module in *)

  let matrix_height_t : L.lltype =
      L.function_type i32_t [| matrix_t |] in
  let matrix_height_func : L.llvalue =
      L.declare_function "getHeight" matrix_height_t the_module in

  let matrix_width_t : L.lltype =
      L.function_type i32_t [| matrix_t |] in
  let matrix_width_func : L.llvalue =
      L.declare_function "getWidth" matrix_width_t the_module in

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
      let local_var = L.build_alloca (ltype_of_typ t) n builder
      in StringMap.add n local_var m
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder ((ty, e) : sexpr) = match e with
	      SLiteral i  -> L.const_int i32_t i
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SStrLit s   -> L.build_global_stringptr s "tmp" builder
      | SFliteral l -> L.const_float_of_string float_t l
      | SNoexpr     -> L.const_int i32_t 0
      | SId s       -> L.build_load (lookup s) s builder

      | SArrayLit e -> L.const_array (ltype_of_typ ty) (Array.of_list (List.map (expr builder) e))

      | SArrayIndex (id, e) -> let idx = expr builder e in 
        let p = L.build_gep (lookup id) [| L.const_int i32_t 0; idx |] "tmp" builder 
        in L.build_load p "tmp" builder

      | SArrayAssign (id, idx, e) -> 
        let idx' = expr builder idx 
        and e' = expr builder e in
        let p = L.build_gep (lookup id) [| L.const_int i32_t 0; idx' |] "tmp" builder 
        in ignore(L.build_store e' p builder); e'

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

      | SAssign (s, e) -> let e' = expr builder e in
                          ignore(L.build_store e' (lookup s) builder); e'
      | SBinop (e1, op, e2) when fst e1 = A.Int && fst e2 = A.Int ->
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
          ) e1' e2' "tmp" builder
      | SBinop (e1, op, e2) when fst e1 = A.Bool && fst e2 = A.Bool ->
        let e1' = expr builder e1
        and e2' = expr builder e2 in
          (match op with
            A.And   -> L.build_and
          | A.Or    -> L.build_or
          | A.Equal -> L.build_icmp L.Icmp.Eq
          | A.Neq   -> L.build_icmp L.Icmp.Ne
          | _       -> raise (Failure "invalid operation on bool")
          ) e1' e2' "tmp" builder
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
        (* L.build_call free_matrix_func [| expr builder e |] "free_matrix" builder *)
        L.build_free (expr builder e) builder
      | SCall ("matrixHeight", [e]) ->
        L.build_call matrix_height_func [| expr builder e |] "matrix_height" builder
      | SCall ("matrixWidth", [e]) ->
        L.build_call matrix_width_func [| expr builder e |] "matrix_width" builder

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
