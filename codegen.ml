
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
module StringHash = Hashtbl.Make(struct
  type t = string
  let equal x y = x = y
  let hash = Hashtbl.hash
end)


type observable_class = SingleObservable | DoubleObservable

let global_vars : L.llvalue StringHash.t = StringHash.create 10
let global_structs = StringHash.create 10
(* must keep list of types to know what to cast to when doing array reference *)
let arr_types = StringHash.create 10

(* translate : Sast.program -> Llvm.module *)
let translate (globs) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Seaflow" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  (* and char_t     = L.i8_type     context *)
  and void_t     = L.void_type   context
  and void_ptr_t = L.pointer_type (L.i8_type context)
  and struct_t   = L.struct_type context     in

  (* function to convert bind list to array of lltypes *)
  let rec list_to_lltype l = Array.of_list (List.map ltype_of_typ l)

  (* Return the LLVM type for a Seaflow type *)
  and ltype_of_typ : A.typ -> L.lltype = function
      A.Int   -> i32_t
    | A.Char  -> i8_t
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.Void  -> void_t
    | A.Arr(_) -> L.pointer_type void_ptr_t
    | A.Struct(str) -> 
      let (_, tlist, _) = try (StringHash.find global_structs ("struct " ^ str))
        with Not_found -> raise (Failure "Struct type mismatch")
      in 
      let t = struct_t (Array.of_list (List.map ltype_of_typ tlist)) in
      L.pointer_type t
    | A.Func(param_types, rtype) ->
      let param_ltypes = (List.map ltype_of_typ param_types) in
      let rltype = ltype_of_typ rtype in
      L.pointer_type (L.function_type rltype (Array.of_list param_ltypes))
    | A.Observable _ -> L.pointer_type obv_t
    | _ -> raise (Failure "Unknown type 2200")


  and obv_t = L.named_struct_type context "observable" in
  let obv_pt = L.pointer_type obv_t in
  let subscription_t  = L.named_struct_type context "subscription" in
  let subscription_pt = L.pointer_type subscription_t in
  let converter_t = L.pointer_type (L.function_type void_t
    ([| void_ptr_t; void_ptr_t; void_ptr_t; void_ptr_t |])) in

  ignore(L.struct_set_body obv_t [|
    void_ptr_t;       (* pointer to current value *)
    void_ptr_t;       (* pointer to upstream value 1 *)
    void_ptr_t;       (* pointer to upstream value 2 *)
    void_ptr_t;       (* pointer to function *)
    subscription_pt;  (* pointer to subscription linked list *)
    i32_t;            (* observable type 1 if single, 2 if double *)
    converter_t       (* pointer to the converter *)
  |] false);

  ignore(L.struct_set_body subscription_t [|
    subscription_pt;  (* pointer to next subscription *)
    obv_pt            (* pointer to observable *)
  |] false);


  let main_ftype = L.function_type i32_t [| |] in
  let main_function = L.define_function "main" main_ftype the_module in

  let global_builder = L.builder_at_end context (L.entry_block main_function) in



  let printi_t : L.lltype =
    L.function_type i32_t [| i32_t |] in
  let printf_t : L.lltype =
    L.function_type i32_t [| float_t |] in 
  let printc_t : L.lltype =
    L.function_type i32_t [| i8_t |] in
  let print_string_t : L.lltype =
    L.function_type (L.pointer_type void_ptr_t) [| L.pointer_type void_ptr_t |] in

  let _ : L.llvalue =
    let f = L.declare_function "print_int" printi_t the_module in
    let store = L.define_global "printi" (L.const_null (L.pointer_type printi_t)) the_module in
    ignore(L.build_store f store global_builder);
    ignore(StringHash.add global_vars "printi" store);
    f
  in

  let _ : L.llvalue =
    let f = L.declare_function "print_char" printc_t the_module in
    let store = L.define_global "printc" (L.const_null (L.pointer_type printc_t)) the_module in
    ignore(L.build_store f store global_builder);
    ignore(StringHash.add global_vars "printc" store);
    f
  in

  let _ : L.llvalue =
    let f = L.declare_function "print_float" printf_t the_module in
    let store = L.define_global "printd" (L.const_null (L.pointer_type printf_t)) the_module in
    ignore(L.build_store f store global_builder);
    ignore(StringHash.add global_vars "printf" store);
    f
  in

  let _ : L.llvalue =
      let f = L.declare_function "print_string" print_string_t the_module in
      let store = L.define_global "prints" (L.const_null (L.pointer_type print_string_t)) the_module in
      ignore(L.build_store f store global_builder);
      ignore(StringHash.add global_vars "prints" store);
      f
  in


  let array_concat_t : L.lltype =
    L.function_type (L.pointer_type void_ptr_t) 
        [| L.pointer_type void_ptr_t ; L.pointer_type void_ptr_t ; L.pointer_type void_ptr_t |] in
  let array_concat_func : L.llvalue =
      L.declare_function "array_concat" array_concat_t the_module in

  let get_base (t: A.typ) = match t with
      A.Float -> L.const_float float_t 0.0
    | A.Int   -> L.const_int i32_t 0
    | A.Bool  -> L.const_int i1_t 0
    | A.Char  -> L.const_int i8_t 0
    | A.Arr(ty) -> L.const_null (ltype_of_typ ty)
    | A.Func(_, _) as f -> L.const_null (ltype_of_typ f)
    | A.Struct(_) as s -> L.const_pointer_null (ltype_of_typ s)
    | A.Void  -> L.const_int i8_t 0
    | _ as x -> raise (Failure ("No match found for " ^ A.string_of_typ x))
  in

  (* LLVM insists each basic block end with exactly one "terminator"
     instruction that transfers control.  This function runs "instr builder"
     if the current block does not already have a terminator.  Used,
     e.g., to handle the "fall off the end of the function" case. *)
  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
    | None -> ignore (instr builder) in

  let next_ftype = L.function_type void_t [| obv_pt |] in
  let next_function = L.define_function "onNext" next_ftype the_module in
  StringHash.add global_vars "onNext" next_function;
  let subscribe_ftype = L.function_type void_t [| obv_pt; obv_pt |] in
  let subscribe_function = L.define_function "subscribe" subscribe_ftype the_module in
  StringHash.add global_vars "subscribe" subscribe_function;
  let complete_ftype = L.function_type void_t [| obv_pt |] in 
  let complete_function = L.define_function "complete" complete_ftype the_module in 
  StringHash.add global_vars "complete" complete_function;


  let lookup vars n = try StringHash.find vars n
      with Not_found ->
        try StringHash.find global_vars n
        with Not_found -> raise (Failure ("Variable " ^ n ^ " not found"))
  in

  let add_store_arr ptr builder i e = 
    let idx = L.const_int i32_t i in
    let eptr = L.build_gep ptr [|idx|] "e" builder in
    let cptr = L.build_pointercast eptr 
        (L.pointer_type (L.pointer_type (L.type_of e))) "p" builder in
    let ealloc = L.build_alloca (L.type_of e) "ealloc" builder in
    ignore(L.build_store e ealloc builder);
    ignore(L.build_store ealloc cptr builder); i+1
  in

  let get_array_len a builder = 
    let idx = L.const_int i32_t 0 in 
    let len = L.build_gep a [| idx |] "" builder in
    let v = L.build_load len "vptr" builder in
    let iptr = L.build_pointercast v (L.pointer_type i32_t) "iptr" builder in
    L.build_load iptr "a" builder
  in

  let arr_ty_of_expr = function  
    | (_, SId(s)) -> ltype_of_typ (StringHash.find arr_types s)
    | (_, SAliteral(t, _)) -> ltype_of_typ t
    | _ -> raise (Failure "Wrong type for arr_ty_of_expr")
  in

  let rec expr vars builder ((tt, e) : sexpr) = match e with
      SLiteral i   -> L.const_int i32_t i
    | SBliteral b  -> L.const_int i1_t (if b then 1 else 0)
    | SChliteral c -> L.const_int i8_t (Char.code c)
    | SFliteral l  -> L.const_float_of_string float_t l
    | SId s        -> L.build_load (lookup vars s) s builder
    | SAliteral (_, a) ->
      let elems = List.map (expr vars builder) a in
      let num = List.length elems in
      let ptr = L.build_array_alloca void_ptr_t (L.const_int i32_t (num+1)) "a" builder in
      ignore(add_store_arr ptr builder 0 (L.const_int i32_t num)); 
      ignore(List.fold_left (add_store_arr ptr builder) 1 elems); ptr
    | SRef(str_name, type_of_struct, fieldname) ->
      let loc = L.build_load (lookup vars str_name) str_name builder in
      let (_, tlist, flist) = StringHash.find global_structs type_of_struct in
      let rec find x lst =
        match lst with
        | [] -> raise (Failure "Struct name not found")
        | h :: t -> if x = h then 0 else 1 + find x t
      in
      let idx = find fieldname flist in
      let p = L.build_struct_gep loc idx "tmp" builder in
      let ty' = ltype_of_typ (List.nth tlist idx) in
      let ptr = L.build_pointercast p (L.pointer_type ty') "ptr" builder in
      L.build_load ptr "z" builder
    | SSliteral(expr_list) ->
        let elems = List.map (expr vars builder) expr_list in
        let ty = struct_t (Array.of_list (List.map L.type_of elems)) in
        let ptr = L.build_alloca ty "struct" builder in
        let add_store i e = 
          let eptr = L.build_struct_gep ptr i "e" builder in
          let cptr = L.build_pointercast eptr (L.pointer_type (L.type_of e)) "p" builder in
          ignore(L.build_store e cptr builder) ; i+1
        in
        ignore(List.fold_left add_store 0 elems) ;
        L.build_pointercast ptr (L.pointer_type ty) "pcast" builder 
    | SArr_Ref(e1, e2) ->
      let ty = arr_ty_of_expr e1 in
      let arr = expr vars builder e1 in
      let idx = expr vars builder e2 in 
      let sum = L.build_add idx (L.const_int i32_t 1) "sum" builder in
      let vptr = L.build_gep arr [| sum |] "" builder in
      let v = L.build_load vptr "vptr" builder in
      let iptr = L.build_pointercast v (L.pointer_type ty) "iptr" builder in
      L.build_load iptr "a" builder 
    | SLen(e) -> get_array_len (expr vars builder e) builder
    | SFuncExpr(params, rt, sstmts) ->
      build_function (params, rt, sstmts)
    | SBinop ((A.Arr(_),_ ) as e1, _, e2) -> 
        let arr = expr vars builder e1 in
        let arr2 = expr vars builder e2 in
        let ptr = L.build_gep arr [| L.const_int i32_t 0 |] "ptr" builder in
        let ptr2 = L.build_gep arr2 [| L.const_int i32_t 0 |] "ptr" builder in

        let len1 = get_array_len arr builder in
        let len2 = get_array_len arr2 builder in
        let total_len = L.build_add len1 len2 "sum" builder in
        let make_len = L.build_add total_len (L.const_int i32_t 1) "sum2" builder in

        let ptr3 = L.build_array_alloca void_ptr_t make_len "a" builder in
        ignore(add_store_arr ptr3 builder 0 total_len) ;
        ignore(L.build_call array_concat_func [| ptr ; ptr2 ; ptr3 |] "array_concat" builder);
        ptr3

    | SBinop (e1, op, e2) ->
      let e1' = expr vars builder e1
      and e2' = expr vars builder e2 in
      let t1 = L.type_of e1'
      and t2 = L.type_of e2' in
      let same = t1 = t2 in
      let same_int_or_char = (same && (t1 = i32_t || t1 = i8_t))
      and same_float = (same && t1 = float_t)
      and same_bool = (same && t1 = i1_t)
      and not_same = (not(same) && (t1 = i32_t || t1 = float_t) && (t2 = i32_t || t2 = float_t))
      and float_left = (t1 = float_t && t2 = i32_t)
      and float_right = (t1 = i32_t && t2 = float_t) in
      let to_float v = L.build_sitofp v float_t "tmp" builder in
      (match op with
        A.Add when same_int_or_char       -> L.build_add e1' e2' "tmp" builder
      | A.Sub when same_int_or_char       -> L.build_sub e1' e2' "tmp" builder
      | A.Mult when same_int_or_char      -> L.build_mul e1' e2' "tmp" builder
      | A.Div when same_int_or_char       -> L.build_sdiv e1' e2' "tmp" builder
      | A.Equal when same_int_or_char     -> L.build_icmp L.Icmp.Eq e1' e2' "tmp" builder
      | A.Neq when same_int_or_char       -> L.build_icmp L.Icmp.Ne e1' e2' "tmp" builder
      | A.Less when same_int_or_char      -> L.build_icmp L.Icmp.Slt e1' e2' "tmp" builder
      | A.Leq when same_int_or_char       -> L.build_icmp L.Icmp.Sle e1' e2' "tmp" builder
      | A.Greater when same_int_or_char   -> L.build_icmp L.Icmp.Sgt e1' e2' "tmp" builder
      | A.Geq when same_int_or_char       -> L.build_icmp L.Icmp.Sge e1' e2' "tmp" builder

      | A.And when same_bool              -> L.build_and e1' e2' "tmp" builder
      | A.Or when same_bool               -> L.build_or e1' e2' "tmp" builder
      
      | A.Add when same_float     -> L.build_fadd e1' e2' "tmp" builder
      | A.Sub when same_float     -> L.build_fsub e1' e2' "tmp" builder
      | A.Mult when same_float    -> L.build_fmul e1' e2' "tmp" builder
      | A.Div when same_float     -> L.build_fdiv e1' e2' "tmp" builder
      | A.Equal when same_float   -> L.build_fcmp L.Fcmp.Oeq e1' e2' "tmp" builder
      | A.Neq when same_float     -> L.build_fcmp L.Fcmp.One e1' e2' "tmp" builder
      | A.Less when same_float    -> L.build_fcmp L.Fcmp.Olt e1' e2' "tmp" builder
      | A.Leq when same_float     -> L.build_fcmp L.Fcmp.Ole e1' e2' "tmp" builder
      | A.Greater when same_float -> L.build_fcmp L.Fcmp.Ogt e1' e2' "tmp" builder
      | A.Geq when same_float     -> L.build_fcmp L.Fcmp.Oge e1' e2' "tmp" builder

      | A.Add when not_same     -> L.build_fadd (to_float e1') (to_float e2') "tmp" builder
      | A.Sub when not_same     -> L.build_fsub (to_float e1') (to_float e2') "tmp" builder
      | A.Mult when not_same    -> L.build_fmul (to_float e1') (to_float e2') "tmp" builder
      | A.Div when not_same     -> L.build_fdiv (to_float e1') (to_float e2') "tmp" builder
      | A.Equal when not_same   -> L.build_fcmp L.Fcmp.Oeq (to_float e1') (to_float e2') "tmp" builder
      | A.Neq when not_same     -> L.build_fcmp L.Fcmp.One (to_float e1') (to_float e2') "tmp" builder
      | A.Less when not_same    -> L.build_fcmp L.Fcmp.Olt (to_float e1') (to_float e2') "tmp" builder
      | A.Leq when not_same     -> L.build_fcmp L.Fcmp.Ole (to_float e1') (to_float e2') "tmp" builder
      | A.Greater when not_same -> L.build_fcmp L.Fcmp.Ogt (to_float e1') (to_float e2') "tmp" builder
      | A.Geq when not_same     -> L.build_fcmp L.Fcmp.Oge (to_float e1') (to_float e2') "tmp" builder

      | A.And | A.Or when (same_float || float_left || float_right) ->
          raise (Failure "internal error: semant should have rejected and/or on float")
      | _ -> raise (Failure "Unknown Binop case")
      )
    | SUnop(op, ((t, _) as e)) ->
      let e' = expr vars builder e in
      (match op with
        A.Neg when t = A.Float -> L.build_fneg
      | A.Neg                  -> L.build_neg) e' "tmp" builder
    | SCall(f, args) ->
      let fdef = expr vars builder f in

      let llargs = List.rev (List.map (expr vars builder) (List.rev args)) in
      let result = (match tt with
                     A.Void -> ""
                   | _ -> "f_result") in

      L.build_call fdef (Array.of_list llargs) result builder
    | _ as x -> raise (Failure ("Not Implemented 2003 " ^ (string_of_sexpr (tt, x))))

  (* Define each function (arguments and return type) so we can
  call it even before we've created its body *)
  and build_function (params, rt, sstmts) =
    let name = "user_func"
    and formal_types = Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) params) in

    let ftype = L.function_type (ltype_of_typ rt) formal_types in
    let the_function = L.define_function name ftype the_module in

    let local_builder = L.builder_at_end context (L.entry_block the_function) in

    let local_vars = StringHash.create 10 in


    let _add_store (t, n) = (* Returns a point to local store *)
      let _ = match StringHash.find_opt local_vars n with
        | Some(_) -> raise (Failure "Cannot have duplicate formal!")
        | None -> ()
      in
      let _store = L.build_alloca (ltype_of_typ t) n local_builder in
      let _ = match t with
        | A.Arr(x) -> StringHash.add arr_types n x
        | _ -> ()
      in
      StringHash.add local_vars n _store ;
      _store
    in

    let _fill_store v store = (* fills store *)
      ignore (L.build_store v store local_builder); ()
    in

    let add_formal (t, n) p =
      let local_store = _add_store (t, n) in
      L.set_value_name n p;
      _fill_store p local_store;
      let _ = match t with
        | A.Arr(x) -> StringHash.add arr_types n x
        | _ -> ()
      in 
      ()
    in

    let _ = List.iter2 add_formal params
      (Array.to_list (L.params the_function))
    in
    
    let rec build_local_stmt builder = function
        SExpr e -> ignore(expr local_vars builder e); builder
      | SBlock(stmt_list) -> List.fold_left build_local_stmt builder stmt_list
      | SDecl(t, s, e) ->
        let local_store = _add_store (t, s) in
        let e' = expr local_vars builder e in
        _fill_store e' local_store;
        builder
      | SReturn e -> ignore(match rt with
                (* Special "return nothing" instr *)
                A.Void -> L.build_ret_void builder
                (* Build return statement *)
              | _ -> L.build_ret (expr local_vars builder e) builder ); builder
      | SIf(ltyp, var, cond, then_stmt, else_stmt) ->
        let local = L.build_alloca (ltype_of_typ ltyp) var builder in
        let bool_val = expr local_vars builder cond in
        let merge_bb = L.append_block context "merge" the_function in
        let then_bb = L.append_block context "then" the_function in
        let then_builder = L.builder_at_end context then_bb in
        let then_val = expr local_vars then_builder then_stmt in
        L.set_value_name var then_val;
        ignore(L.build_store then_val local then_builder) ;
        StringHash.add local_vars var local ;
        ignore(L.build_br merge_bb then_builder) ;
        let else_bb = L.append_block context "else" the_function in
        let else_builder = L.builder_at_end context else_bb in
        let else_val = expr local_vars else_builder else_stmt in
        ignore(L.set_value_name var else_val) ;
        ignore(L.build_store else_val local else_builder);
        StringHash.add local_vars var local ;
        ignore(L.build_br merge_bb else_builder) ;
        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context merge_bb
      | _ -> raise (Failure "Not Implemented 2005")
    in

    let local_builder_out = build_local_stmt local_builder (SBlock sstmts) in

    add_terminal local_builder_out (match rt with
            A.Void -> L.build_ret_void
          | A.Float -> L.build_ret (L.const_float float_t 0.0)
          | t -> L.build_ret (L.const_int (ltype_of_typ t) 0));
    the_function
  in


  let make_observable (t, e) builder =
    let v_store = L.define_global "target" (get_base t) the_module in
    let _ = match e with
      | Some x -> ignore(L.build_store x v_store builder)
      | None -> ()
    in
    let obs = L.build_malloc obv_t "__obs" builder in

    let curr_vpp = L.build_struct_gep obs 0 "__curr_vpp" builder in
    let curr_vp = L.build_bitcast v_store void_ptr_t "store_to_i8" builder in

    ignore(L.build_store curr_vp curr_vpp builder);

    obs
  in


  (*
    A new custom type converter function is created for every subscription.
    Takes 4 void pointers, and internally converts them to correct types and pushes the result
  *)
  let build_type_converter ((rtype: A.typ), typ1, typ2) (cls: observable_class) =
    let llrtype = match rtype with
      | A.Void -> i8_t
      | _ as x -> ltype_of_typ x
    in
    let lltyp1  = ltype_of_typ typ1 in
    let lltyp2  = ltype_of_typ typ2 in

    let nftype = L.function_type void_t ([| void_ptr_t; void_ptr_t; void_ptr_t; void_ptr_t |]) in
    let nfunction = L.define_function "converter" nftype the_module in

    let nbuilder = L.builder_at_end context (L.entry_block nfunction) in
    L.set_value_name "r"  (L.params nfunction).(0);
    L.set_value_name "p1" (L.params nfunction).(1);
    L.set_value_name "p2" (L.params nfunction).(2);
    L.set_value_name "f"  (L.params nfunction).(3);

    let _rv  = L.build_bitcast (L.params nfunction).(0) (L.pointer_type llrtype) "i8_to_r"  nbuilder in
    let _p1v = L.build_bitcast (L.params nfunction).(1) (L.pointer_type lltyp1) "i8_to_p1" nbuilder in

    let build_call = match cls with
      | SingleObservable ->
        let dest_func_type = L.pointer_type (L.function_type llrtype ([| lltyp1 |])) in
        let _fv  = L.build_bitcast (L.params nfunction).(3) dest_func_type        "i8_to_f"  nbuilder in

        let p1 = L.build_load _p1v "p1" nbuilder in
        
        L.build_call _fv [| p1 |]
      | DoubleObservable ->
        let dest_func_type = L.pointer_type (L.function_type llrtype ([| lltyp1; lltyp2 |])) in
        let _p2v = L.build_bitcast (L.params nfunction).(2) (L.pointer_type lltyp2) "i8_to_p2" nbuilder in
        let _fv  = L.build_bitcast (L.params nfunction).(3) dest_func_type        "i8_to_f"  nbuilder in

        let p1 = L.build_load _p1v "p1" nbuilder in
        let p2 = L.build_load _p2v "p2" nbuilder in
        L.build_call _fv [| p1; p2 |]
    in

    let _ = match rtype with
      | A.Void -> build_call "" nbuilder;
      | _ ->
        let result = build_call "result" nbuilder in
        L.build_store result _rv nbuilder
    in
    ignore(L.build_ret_void nbuilder) ;
    nfunction
  in


  let rec oexpr vars builder ((_, oe) : soexpr) = match oe with
      SOId s -> L.build_load (lookup vars s) s builder
    | SMap(e, oe) ->
      (*   Basically SSubscribe, but returns obs  *)
      (* 1 *)

      let (rt, p1t) = match fst e with
        | A.Func([x], r) -> (r, x)
        | _ -> raise  (Failure "Subscriber must be a function")
      in

      let func_p = expr global_vars builder e in
      let upstream = oexpr global_vars builder oe in
      let obs = make_observable (rt, None) builder in

      let obs_store = L.define_global "sub_obs" (L.const_null obv_pt) the_module in
      ignore(L.build_store obs obs_store builder) ;


      let obs_func_vpp = L.build_struct_gep obs 3 "__func" builder in
      let func_vp = L.build_bitcast func_p void_ptr_t "func_to_i8" builder in
      ignore(L.build_store func_vp obs_func_vpp builder);

      (* 2 *)
      ignore(L.build_call 
          (lookup global_vars "subscribe") [| upstream; obs |] "" builder) ;

      let upstream_curr_vpp = L.build_struct_gep upstream 0 "__ups_curr_vpp" builder in
      let upstream_curr_vp = L.build_load upstream_curr_vpp "__ups_curr_vp" builder in
      let obs_upv_vpp = L.build_struct_gep obs 1 "__dwn_upv_vpp" builder in
      ignore(L.build_store upstream_curr_vp obs_upv_vpp builder) ;

      (* 3 *)
      let converter = build_type_converter (rt, p1t, A.Void) SingleObservable in
      let obs_converter_p = L.build_struct_gep obs 6 "__converter" builder in
      ignore(L.build_store converter obs_converter_p builder) ;


      (* 4 *)
      let _r  = L.build_load (L.build_struct_gep obs 0 "_r"  builder) "_r"  builder in
      let _p1 = L.build_load (L.build_struct_gep obs 1 "_p1" builder) "_p1" builder in
      let _f  = L.build_load (L.build_struct_gep obs 3 "_f"  builder) "_f"  builder in
      let _c  = L.build_load (L.build_struct_gep obs 6 "_c"  builder) "_c"  builder in
      ignore(L.build_call _c [| _r; _p1; _p1; _f |] "" builder) ;
    

      (* 3 *)

      (* won't need but won't hurt to have *)
      ignore(L.build_call (lookup global_vars "onNext") [| obs |] "" builder) ;  
      ; obs
    | SCombine(e, oe1, oe2) ->
      (*   Basically SSubscribe, but returns obs  *)
      (* 1 *)

      let (rt, p1t, p2t) = match fst e with
        | A.Func([x; y], r) -> (r, x, y)
        | _ -> raise  (Failure "Subscriber must be a function")
      in

      let func_p = expr global_vars builder e in
      let upstream1 = oexpr global_vars builder oe1 in
      let upstream2 = oexpr global_vars builder oe2 in
      let obs = make_observable (rt, None) builder in

      let obs_store = L.define_global "sub_obs" (L.const_null obv_pt) the_module in
      ignore(L.build_store obs obs_store builder) ;


      let obs_func_vpp = L.build_struct_gep obs 3 "__func" builder in
      let func_vp = L.build_bitcast func_p void_ptr_t "func_to_i8" builder in
      ignore(L.build_store func_vp obs_func_vpp builder);

      (* 2 *)
      ignore(L.build_call 
          (lookup global_vars "subscribe") [| upstream1; obs |] "" builder) ;
      ignore(L.build_call 
          (lookup global_vars "subscribe") [| upstream2; obs |] "" builder) ;

      let upstream1_curr_vpp = L.build_struct_gep upstream1 0 "__ups1_curr_vpp" builder in
      let upstream1_curr_vp = L.build_load upstream1_curr_vpp "__ups1_curr_vp" builder in
      let obs_upv1_vpp = L.build_struct_gep obs 1 "__dwn_upv1_vpp" builder in
      ignore(L.build_store upstream1_curr_vp obs_upv1_vpp builder) ;

      let upstream2_curr_vpp = L.build_struct_gep upstream2 0 "__ups2_curr_vpp" builder in
      let upstream2_curr_vp = L.build_load upstream2_curr_vpp "__ups2_curr_vp" builder in
      let obs_upv2_vpp = L.build_struct_gep obs 2 "__dwn_upv2_vpp" builder in
      ignore(L.build_store upstream2_curr_vp obs_upv2_vpp builder) ;


      (* 3 *)
      let converter = build_type_converter (rt, p1t, p2t) DoubleObservable in
      let obs_converter_p = L.build_struct_gep obs 6 "__converter" builder in
      ignore(L.build_store converter obs_converter_p builder) ;


      (* 4 *)
      let _r  = L.build_load (L.build_struct_gep obs 0 "_r"  builder) "_r"  builder in
      let _p1 = L.build_load (L.build_struct_gep obs 1 "_p1" builder) "_p1" builder in
      let _p2 = L.build_load (L.build_struct_gep obs 2 "_p2" builder) "_p2" builder in
      let _f  = L.build_load (L.build_struct_gep obs 3 "_f"  builder) "_f"  builder in
      let _c  = L.build_load (L.build_struct_gep obs 6 "_c"  builder) "_c"  builder in
      ignore(L.build_call _c [| _r; _p1; _p2; _f |] "" builder ) ;


      (* 3 *)

      (* won't need but won't hurt to have *)
      ignore(L.build_call (lookup global_vars "onNext") [| obs |] "" builder) ;  
      ; obs

    | SOBinop1(_, _, _) -> raise (Failure ("Not Implemented 2020"))
    | SOBinop2(_, _, _) -> raise (Failure ("Not Implemented 2020"))
    | SOBinop3(_, _, _) -> raise (Failure ("Not Implemented 2020"))
    | SOUnop(_, _) -> raise (Failure ("Not Implemented 2020"))
  in
  let build_global_stmt builder = function
      SExpr e -> ignore(expr global_vars builder e); builder
    | SDecl(t, s, e) ->
        let _ = match StringHash.find_opt global_vars s with
          | Some(_) -> raise (Failure "Cannot declare global more than once!")
          | None -> ()
        in

        let init t' = match t' with
            A.Float -> L.const_float float_t 0.0
          | A.Int   -> L.const_int i32_t 0
          | A.Bool  -> L.const_int i1_t 0
          | A.Char  -> L.const_int i8_t 0
          | A.Arr(_) -> L.const_null (L.pointer_type void_ptr_t)
          | A.Func(_, _) as f -> L.const_null (ltype_of_typ f)
          | A.Struct(_) as s -> L.const_pointer_null (ltype_of_typ s)
          | _ as x -> raise (Failure ("Cannot have global of type: " ^ A.string_of_typ x))
        in
        let store = L.define_global s (init t) the_module in
        StringHash.add global_vars s store;
        let e' = expr global_vars builder e in
      
        let _ = match t with
          | A.Arr(x) -> StringHash.add arr_types s x
          | _ -> ()
        in 
        ignore(L.build_store e' store builder);
        builder
    | SStr_Def(s, b_list) ->
        let tlist = List.map fst b_list in
        let flist = List.map snd b_list in
        let ty = struct_t (list_to_lltype tlist)  in
        StringHash.add global_structs ("struct " ^ s) (ty, tlist, flist) ; builder
    | _ -> raise (Failure "Global statement not implemented")
  in




  let build_obs_stmt builder = function
      SObs(_, _) -> builder

    | SODecl(lt, s, e) ->
      let e' = expr global_vars builder e in
      let lt' = match lt with
        | A.Observable x -> x
        | _ -> raise (Failure "Observable declaration must use observable")
      in
      let obv_ptr = make_observable (lt', Some e') builder in
      let store = L.define_global s (L.const_null (L.pointer_type obv_t)) the_module in
      ignore(L.build_store obv_ptr store builder) ;
      StringHash.add global_vars s store;
      builder
    | SOODecl(_, s, oe) ->
      let oe' = oexpr global_vars builder oe in
      let store = L.define_global s (L.const_null (L.pointer_type obv_t)) the_module in
      ignore(L.build_store oe' store builder) ;
      StringHash.add global_vars s store;
      builder
    | SOAssign(lt, s, e) ->
      let e' = expr global_vars builder e in
      let lt' = match lt with
        | A.Observable x -> x
        | _ -> raise (Failure "Observable declaration must use observable")
      in
      let obs = L.build_load (lookup global_vars s) s builder in

      let curr_vpp = L.build_struct_gep obs 0 "__curr_vpp" builder in                      (* curr_vpp: i8**  *)
      let curr_vp = L.build_load curr_vpp "__curr_vp" builder in                           (* curr_vp : i8*   *)
      let curr_p = L.build_bitcast curr_vp (L.pointer_type (ltype_of_typ lt')) "i8_to_curr" builder in  (* curr_p  : i32*  *)
      ignore(L.build_store e' curr_p builder) ;
      ignore(L.build_call (lookup global_vars "onNext") [| obs |] "" builder) ;
      builder

    | SOOAssign(_, _, _) ->
      (* Maybe we should not allow this *)
      builder
    | SComplete(_, oe) ->
      let upstream = oexpr global_vars builder oe in
      ignore(L.build_call (lookup global_vars "complete") [| upstream |] "" builder) ;
      builder
    | SSubscribe(_, e, oe) ->
      (*
        1. Create new observable
          1.1. allocate value holder in the stack
          1.2. malloc observable in the heap
          1.3. store function pointer to observable
          1.4. store 1.1 to observable
        2. Connect to upstream
          2.1. go to upstream and store itself as the child
          2.2. copy upstreamValue address to itself
          2.3. 
        3. Define converter function
        4. Call function on upstream value
      *)

      (* 1 *)
      let (rt, p1t) = match fst e with
      | A.Func([x], r) -> (r, x)
      | _ -> raise  (Failure "Subscriber must be a function")
      in
      let func_p = expr global_vars builder e in

      let upstream = oexpr global_vars builder oe in
      let obs = make_observable (rt, None) builder in

      let obs_store = L.define_global "sub_obs" (L.const_null obv_pt) the_module in
      ignore(L.build_store obs obs_store builder) ;


      let obs_func_vpp = L.build_struct_gep obs 3 "__func" builder in
      let func_vp = L.build_bitcast func_p void_ptr_t "func_to_i8" builder in
      ignore(L.build_store func_vp obs_func_vpp builder);

      (* 2 *)

      let upstream_curr_vpp = L.build_struct_gep upstream 0 "__ups_curr_vpp" builder in
      let upstream_curr_vp = L.build_load upstream_curr_vpp "__ups_curr_vp" builder in
      let obs_upv_vpp = L.build_struct_gep obs 1 "__dwn_upv_vpp" builder in
      ignore(L.build_store upstream_curr_vp obs_upv_vpp builder) ;
      ignore(L.build_call 
          (lookup global_vars "subscribe") [| upstream; obs |] "" builder) ;

      (* 3 *)
      let converter = build_type_converter (rt, p1t, A.Void) SingleObservable in
      let obs_converter_p = L.build_struct_gep obs 6 "__converter" builder in
      ignore(L.build_store converter obs_converter_p builder) ;


      (* 4 *)
      let _r  = L.build_load (L.build_struct_gep obs 0 "_r"  builder) "_r"  builder in
      let _p1 = L.build_load (L.build_struct_gep obs 1 "_p1" builder) "_p1" builder in
      let _f  = L.build_load (L.build_struct_gep obs 3 "_f"  builder) "_f"  builder in
      let _c  = L.build_load (L.build_struct_gep obs 6 "_c"  builder) "_c"  builder in
      ignore(L.build_call _c [| _r; _p1; _p1; _f |] "" builder) ;



      (* 4 *)

      (* won't need but won't hurt to have *)
      ignore(L.build_call (lookup global_vars "onNext") [| obs |] "" builder) ;  
      builder
    | _ -> raise (Failure ("Not Implemented 2100"))
  in


  let translate_line glob = match glob with
        SStmt stmt -> ignore(build_global_stmt global_builder stmt)
      | SObs_Stmt obs_stmt -> ignore(build_obs_stmt global_builder obs_stmt)

  in
  let () = List.iter translate_line globs in
  let () = add_terminal global_builder (L.build_ret (L.const_int i32_t 0)) in

  (* define_subscribe *)
  let _ =
    (*
    void subscribe(observable* upstream, observable* downstream) {

      subscription *new = (subscription^) malloc(sizeof(subscription));
      new->__obs = downstream;
      new->__next = upstream->subscription;

      upstream->subscription = new
    }
    *)

    let subscribe_function = StringHash.find global_vars "subscribe" in
    let builder = L.builder_at_end context (L.entry_block subscribe_function) in
    L.set_value_name "upstream" (L.params subscribe_function).(0);
    L.set_value_name "downstream" (L.params subscribe_function).(1);
    let up_local   = L.build_alloca obv_pt "upstream" builder in
    let down_local = L.build_alloca obv_pt "downstream" builder in
    ignore(L.build_store (L.params subscribe_function).(0) up_local builder) ;
    ignore(L.build_store (L.params subscribe_function).(1) down_local builder) ;
    let upstream = L.build_load up_local "__ups" builder in      (* upstream   : observable*  *)
    let downstream = L.build_load down_local "__dws" builder in  (* downstream : observable*  *)


    let next' = L.build_struct_gep upstream 4 "__subscription_pp" builder in  (* next' : subscription**  &(upstream->sub) *)
    let next  = L.build_load next' "__subscription_p" builder in              (* next  : subscription*   upstream->sub    *)


    let new_sub = L.build_malloc subscription_t "new" builder in     (* new_sub : subscription*                    *)
    let n_next' = L.build_struct_gep new_sub 0 "__next" builder in   (* n_next' : subscription**  &(new_sub->next) *)
    let n_obs'  = L.build_struct_gep new_sub 1 "__obs" builder in    (* n_obs'  : observable**    &(new_sub->obs)  *)
    ignore(L.build_store next n_next' builder) ;
    ignore(L.build_store downstream n_obs' builder) ; 

    ignore(L.build_store new_sub next' builder) ;
    L.build_ret_void builder;
  in

  (* define_complete *)
  let _ = 
    (*
    void complete(observable* upstream) {
      upstream->__next = Null
    }
    *)
    let complete_function = StringHash.find global_vars "complete" in 
    let builder = L.builder_at_end context (L.entry_block complete_function) in 
    L.set_value_name "upstream" (L.params complete_function).(0);
    let up_local = L.build_alloca obv_pt "upstream" builder in
    ignore(L.build_store (L.params complete_function).(0) up_local builder) ;
    let upstream = L.build_load up_local "__ups" builder in
    let next' = L.build_struct_gep upstream 4 "__subscription_pp" builder in  (* next' : subscription**  &(upstream->sub) *)
    let new_sub = L.const_pointer_null subscription_pt in     (* new_sub : subscription*                    *)
    ignore(L.build_store new_sub next' builder) ;
    L.build_ret_void builder;

  in

  (* define_next *)
  let _ = 
    (*
    void onNext(observable* obs) {

      subscription* current = obs->__currentscription;
      while (current != NULL) {
        observable* d = current->__obs;
        d->__curr = d->__func(d->__upstreamValue);
        onNext(d);
        current = current->__next;
        return;
      }
      return;
    }
    *)

    (* Define function *)
    let next_function = StringHash.find global_vars "onNext" in
    let builder = L.builder_at_end context (L.entry_block next_function) in
    L.set_value_name "upstream" (L.params next_function).(0);
    let up_local = L.build_alloca obv_pt "upstream" builder in
    ignore(L.build_store (L.params next_function).(0) up_local builder) ;
    let upstream = L.build_load up_local "__ups" builder in


    let _sub' = L.build_struct_gep upstream 4 "__sub_pp" builder in      (* sub'     : subscription**  *)
    let _sub  = L.build_load _sub' "__sub_p" builder in                  (* sub      : subscription*   *)
    let current' = L.build_alloca subscription_pt "current_p" builder in (* current' : subscription**  *)
    ignore(L.build_store _sub current' builder) ;

    (* child blocks *)
    let while_pred_bb = L.append_block context "while_pred" next_function in
    let while_pred_builder = L.builder_at_end context while_pred_bb in

    let while_body_bb = L.append_block context "while_body" next_function in
    let while_body_builder = L.builder_at_end context while_body_bb in

    let merge_bb = L.append_block context "merge" next_function in
    let merge_builder = L.builder_at_end context merge_bb in


    (* while_pred block *)
    let current = L.build_load current' "current" while_pred_builder in    (* current : subscription*   *)

    let cond = L.build_icmp L.Icmp.Ne (L.const_pointer_null subscription_pt) current "tmp" while_pred_builder in

    let d' = L.build_struct_gep current 1 "__d_ref" while_body_builder in  (* d' : observable**  *)
    let d  = L.build_load d' "__d" while_body_builder in                   (* d  : observable*   *)

    let next_sub' = L.build_struct_gep current 0 "__next_p" while_body_builder in  (* next_sub' : subscription**  *)
    let next_sub  = L.build_load next_sub' "__next" while_body_builder in          (* next_sub  : subscription*   *)
    ignore(L.build_store next_sub current' while_body_builder) ;



    (* single block *)

    let _r  = L.build_load (L.build_struct_gep d 0 "_r"  while_body_builder) "_r"  while_body_builder in
    let _p1 = L.build_load (L.build_struct_gep d 1 "_p1" while_body_builder) "_p1" while_body_builder in
    let _p2 = L.build_load (L.build_struct_gep d 2 "_p2" while_body_builder) "_p2" while_body_builder in
    let _f  = L.build_load (L.build_struct_gep d 3 "_f"  while_body_builder) "_f"  while_body_builder in
    let _c  = L.build_load (L.build_struct_gep d 6 "_c"  while_body_builder) "_c"  while_body_builder in
    let _ = L.build_call _c [| _r; _p1; _p2; _f |] "" while_body_builder in 

    (*    onNext(d); *)
    ignore(L.build_call next_function [| d |] "" while_body_builder) ;




    (* Connect blocks *)
    ignore(L.build_br while_pred_bb builder) ;
    ignore(L.build_cond_br cond while_body_bb merge_bb while_pred_builder) ;
    ignore(L.build_br while_pred_bb while_body_builder);
    ignore(L.build_ret_void merge_builder)
  in

  the_module
