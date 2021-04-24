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
open Printf
open Utils

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
  (* and i1_t       = L.i1_type     context *)
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
    | A.Float -> float_t
    | A.Void  -> void_t
    | A.Arr(x) -> L.pointer_type void_ptr_t
    | A.Struct(str) -> 
      let (ty, tlist, _) = try (StringHash.find global_structs ("struct " ^ str))
        with Not_found -> raise (Failure "Struct type mismatch")
      in 
      let t = struct_t (Array.of_list (List.map ltype_of_typ tlist)) in
      L.pointer_type t
    | A.Func(param_types, rtype) ->
      let param_ltypes = (List.map ltype_of_typ param_types) in
      let rltype = ltype_of_typ rtype in
      L.pointer_type (L.function_type rltype (Array.of_list param_ltypes))
    | A.Observable t -> L.pointer_type obv_t


  and obv_t = L.named_struct_type context "observable" in
  let obv_pt = L.pointer_type obv_t in
  let subscription_t  = L.named_struct_type context "subscription" in
  let subscription_pt = L.pointer_type subscription_t in

  ignore(L.struct_set_body obv_t [|
    void_ptr_t;       (* pointer to current value *)
    void_ptr_t;       (* pointer to upstream value 1 *)
    void_ptr_t;       (* pointer to upstream value 2 *)
    void_ptr_t;       (* pointer to function *)
    subscription_pt;  (* pointer to subscription linked list *)
    i32_t             (* observable type 1 if single, 2 if double *)
  |] false);

  ignore(L.struct_set_body subscription_t [|
    subscription_pt;  (* pointer to next subscription *)
    obv_pt            (* pointer to observable *)
  |] false);

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in (* L.declare_function returns the function if it already exits in the module *)


  let main_ftype = L.function_type i32_t [| |] in
  let main_function = L.define_function "main" main_ftype the_module in

  let global_builder = L.builder_at_end context (L.entry_block main_function) in

  let add_global_var (t, s, v, builder) =
    let rec init t' = match t' with
        A.Float -> L.const_float float_t 0.0
      | A.Int   -> L.const_int i32_t 0
      | A.Char  -> L.const_int i8_t 0
      | A.Arr(ty) -> L.const_null (L.type_of v)
      | A.Func(_, _) as f -> L.const_null (ltype_of_typ f)
      | A.Struct(x) as s -> L.const_pointer_null (ltype_of_typ s)
    in
    let store = L.define_global s (init t) the_module in
    L.build_store v store builder ; StringHash.add global_vars s store
  in

  (* LLVM insists each basic block end with exactly one "terminator"
     instruction that transfers control.  This function runs "instr builder"
     if the current block does not already have a terminator.  Used,
     e.g., to handle the "fall off the end of the function" case. *)
  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
    | None -> ignore (instr builder) in

  let int_format_str builder = L.build_global_stringptr "%d\n" "fmt" builder in
  let float_format_str builder = L.build_global_stringptr "%f\n" "fmt" builder in
  let char_format_str builder = L.build_global_stringptr "%c\n" "fmt" builder in
  let str_format_str builder = L.build_global_stringptr "%s\n" "fmt" builder in


  let next_ftype = L.function_type void_t [| obv_pt |] in
  let next_function = L.define_function "onNext" next_ftype the_module in
  StringHash.add global_vars "onNext" next_function;
  let subscribe_ftype = L.function_type void_t [| obv_pt; obv_pt |] in
  let subscribe_function = L.define_function "subscribe" subscribe_ftype the_module in
  StringHash.add global_vars "subscribe" subscribe_function;


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
    L.build_store e ealloc builder ;
    L.build_store ealloc cptr builder; i+1
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
    | (_, SAliteral(t, a)) -> ltype_of_typ t
  in

  (*
      int[] a = [1,2,3];
      int[] b = [1,2,3];
      int[] c = []; // but of len 6
      int cidx = 0;

      int i = 0;
      while (i < a.length){
          int tmp = a[i];
          c[cidx] = tmp
          cidx++;
          i++;
      }
      i = 0;
      while (i < b.length){
          int tmp = b[i];
          c[cidx] = tmp
          cidx++;
          i++;
      }
  
  *)

  let arr_concat a1 a2 ty builder = 
    (* initialization of idx var - source *)
    let init_idx = L.const_int i32_t 1 in 
    let idxp = L.build_alloca (L.type_of init_idx) "idx" builder in
    let s = L.build_store init_idx idxp builder in
    let src_idx = L.build_load s "idx" builder in

    (* initialization of idx var - dest *)
    let init_idx = L.const_int i32_t 1 in 
    let idxp = L.build_alloca (L.type_of init_idx) "idx" builder in
    let s = L.build_store init_idx idxp builder in
    let dest_idx = L.build_load s "idx" builder in
    
    (* get length of 1st array *)
    let flen = get_array_len a1 builder in

    (* get length of second array *)
    let slen = get_array_len a2 builder in

    let dest_len = L.build_add flen slen "sum" builder in
    let size = L.build_add dest_len (L.const_int i32_t 1) "size" builder in

    (* create destination array *)
    let ptr = L.build_array_alloca void_ptr_t size "a" builder in

    (* main_function should be replaced - this should be a builtin function like onNext? *)
    let pred_bb = L.append_block context "while" main_function in
    ignore(L.build_br pred_bb builder);

    let body_bb = L.append_block context "while_body" main_function in
    let body_builder = L.builder_at_end context body_bb in

    (* loop body *)

    (* int tmp = a[i]; *)


    (* c[cidx] = tmp *)
    (* cidx++; *)
    (* i++; *)


    (* add_terminal (stmt (L.builder_at_end context body_bb) body) *)
    (L.build_br pred_bb);

    let pred_builder = L.builder_at_end context pred_bb in 
    let bool_val = L.build_icmp L.Icmp.Slt src_idx dest_len "comp" pred_builder in

    let merge_bb = L.append_block context "merge" main_function in
    ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
    L.builder_at_end context merge_bb ; ptr
  in

  let rec expr vars builder ((tt, e) : sexpr) = match e with
      SLiteral i   -> L.const_int i32_t i
    | SChliteral c -> L.const_int i8_t (Char.code c)
    | SFliteral l  -> L.const_float_of_string float_t l
    | SId s        -> L.build_load (lookup vars s) s builder
    | SAliteral (ty, a) ->
      let elems = List.map (expr vars builder) a in
      let num = List.length elems in
      let ptr = L.build_array_alloca void_ptr_t (L.const_int i32_t (num+1)) "a" builder in
      add_store_arr ptr builder 0 (L.const_int i32_t num) ; 
      List.fold_left (add_store_arr ptr builder) 1 elems ; ptr
    | SRef(str_name, type_of_struct, fieldname) ->
      let loc = L.build_load (lookup vars str_name) str_name builder in
      let (ty, tlist, flist) = StringHash.find global_structs type_of_struct in
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
          L.build_store e cptr builder; i+1
        in
        List.fold_left add_store 0 elems ;
        L.build_pointercast ptr (L.pointer_type ty) "pcast" builder 
    | SArr_Ref(e1, e2) ->
      (* how to check for out of bounds index? *)
      let ty = arr_ty_of_expr e1 in
      let arr = expr vars builder e1 in
      let idx = expr vars builder e2 in 
      let sum = L.build_add idx (L.const_int i32_t 1) "sum" builder in
      let vptr = L.build_gep arr [| sum |] "" builder in
      let v = L.build_load vptr "vptr" builder in
      let iptr = L.build_pointercast v (L.pointer_type ty) "iptr" builder in
      L.build_load iptr "a" builder 
    | SLen(e) -> get_array_len (expr vars builder e) builder
    | SIf (e1, e2, e3) ->
      let e1' = expr vars builder e1
      and e2' = expr vars builder e2
      and e3' = expr vars builder e3 in
      L.build_select e1' e2' e3' "tmp" builder
    | SFuncExpr(params, rt, sstmts) ->
      build_function (params, rt, sstmts)
    | SBinop ((A.Arr(_),_ ) as e1, op, e2) -> 
        let elem_ty = arr_ty_of_expr e1 in
        arr_concat (expr vars builder e1) (expr vars builder e2) elem_ty builder
    | SBinop (e1, op, e2) ->
      let e1' = expr vars builder e1
      and e2' = expr vars builder e2 in
      let t1 = L.type_of e1'
      and t2 = L.type_of e2' in
      let same = t1 = t2 in
      let same_int_or_char = (same && (t1 = i32_t || t1 = i8_t))
      and same_float = (same && t1 = float_t)
      and float_left = (t1 = float_t && t2 = i32_t)
      and float_right = (t1 = i32_t && t2 = float_t) in
      (match op with
        A.Add when same_int_or_char       -> L.build_add e1' e2' "tmp" builder
      | A.Sub when same_int_or_char       -> L.build_sub e1' e2' "tmp" builder
      | A.Mult when same_int_or_char      -> L.build_mul e1' e2' "tmp" builder
      | A.Div when same_int_or_char       -> L.build_sdiv e1' e2' "tmp" builder
      | A.And when same_int_or_char       -> L.build_and e1' e2' "tmp" builder
      | A.Or when same_int_or_char        -> L.build_or e1' e2' "tmp" builder
      | A.Equal when same_int_or_char     -> L.build_icmp L.Icmp.Eq e1' e2' "tmp" builder
      | A.Neq when same_int_or_char       -> L.build_icmp L.Icmp.Ne e1' e2' "tmp" builder
      | A.Less when same_int_or_char      -> L.build_icmp L.Icmp.Slt e1' e2' "tmp" builder
      | A.Leq when same_int_or_char       -> L.build_icmp L.Icmp.Sle e1' e2' "tmp" builder
      | A.Greater when same_int_or_char   -> L.build_icmp L.Icmp.Sgt e1' e2' "tmp" builder
      | A.Geq when same_int_or_char       -> L.build_icmp L.Icmp.Sge e1' e2' "tmp" builder
      
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

      | A.Add when float_left     -> L.build_fadd e1' (L.build_sitofp e2' float_t "tmp" builder) "tmp" builder
      | A.Sub when float_left     -> L.build_fsub e1' (L.build_sitofp e2' float_t "tmp" builder) "tmp" builder
      | A.Mult when float_left    -> L.build_fmul e1' (L.build_sitofp e2' float_t "tmp" builder) "tmp" builder
      | A.Div when float_left     -> L.build_fdiv e1' (L.build_sitofp e2' float_t "tmp" builder) "tmp" builder
      | A.Equal when float_left   -> L.build_fcmp L.Fcmp.Oeq e1' (L.build_sitofp e2' float_t "tmp" builder) "tmp" builder
      | A.Neq when float_left     -> L.build_fcmp L.Fcmp.One e1' (L.build_sitofp e2' float_t "tmp" builder) "tmp" builder
      | A.Less when float_left    -> L.build_fcmp L.Fcmp.Olt e1' (L.build_sitofp e2' float_t "tmp" builder) "tmp" builder
      | A.Leq when float_left     -> L.build_fcmp L.Fcmp.Ole e1' (L.build_sitofp e2' float_t "tmp" builder) "tmp" builder
      | A.Greater when float_left -> L.build_fcmp L.Fcmp.Ogt e1' (L.build_sitofp e2' float_t "tmp" builder) "tmp" builder
      | A.Geq when float_left     -> L.build_fcmp L.Fcmp.Oge e1' (L.build_sitofp e2' float_t "tmp" builder) "tmp" builder

      | A.Add when float_right     -> L.build_fadd (L.build_sitofp e1' float_t "tmp" builder) e2' "tmp" builder
      | A.Sub when float_right     -> L.build_fsub (L.build_sitofp e1' float_t "tmp" builder) e2' "tmp" builder
      | A.Mult when float_right    -> L.build_fmul (L.build_sitofp e1' float_t "tmp" builder) e2' "tmp" builder
      | A.Div when float_right     -> L.build_fdiv (L.build_sitofp e1' float_t "tmp" builder) e2' "tmp" builder
      | A.Equal when float_right   -> L.build_fcmp L.Fcmp.Oeq (L.build_sitofp e1' float_t "tmp" builder) e2' "tmp" builder
      | A.Neq when float_right     -> L.build_fcmp L.Fcmp.One (L.build_sitofp e1' float_t "tmp" builder) e2' "tmp" builder
      | A.Less when float_right    -> L.build_fcmp L.Fcmp.Olt (L.build_sitofp e1' float_t "tmp" builder) e2' "tmp" builder
      | A.Leq when float_right     -> L.build_fcmp L.Fcmp.Ole (L.build_sitofp e1' float_t "tmp" builder) e2' "tmp" builder
      | A.Greater when float_right -> L.build_fcmp L.Fcmp.Ogt (L.build_sitofp e1' float_t "tmp" builder) e2' "tmp" builder
      | A.Geq when float_right     -> L.build_fcmp L.Fcmp.Oge (L.build_sitofp e1' float_t "tmp" builder) e2' "tmp" builder

      | A.And | A.Or when (same_float || float_left || float_right) ->
          raise (Failure "internal error: semant should have rejected and/or on float")
      )
    | SUnop(op, ((t, _) as e)) ->
      let e' = expr vars builder e in
      (match op with
        A.Neg when t = A.Float -> L.build_fneg
      | A.Neg                  -> L.build_neg) e' "tmp" builder
    | SBCall("printi", [e]) ->
        L.build_call printf_func [| int_format_str builder; (expr vars builder e) |]
          "printf" builder
    | SBCall("printc", [e]) ->
        L.build_call printf_func [| char_format_str builder; (expr vars builder e) |]
          "printf" builder
    | SBCall("printf", [e]) ->
        L.build_call printf_func [| float_format_str builder; (expr vars builder e) |]
          "printf" builder
    (* | SCall(n, args) -> print_endline n ; L.const_int i32_t 5 *)
    | SCall(f, args) ->
      let fdef = expr vars builder f in

      (* let (fdef, fdecl) = StringHash.find global_funcs f in *)
      let llargs = List.rev (List.map (expr vars builder) (List.rev args)) in
      let result = (match tt with
                     A.Void -> ""
                   | _ -> "f_result") in

      (* print_endline ("fdef: " ^ (L.string_of_lltype (L.type_of fdef)));  *)

      L.build_call fdef (Array.of_list llargs) result builder
    | _ as x -> raise (Failure ("Not Implemented 2003 " ^ (string_of_sexpr (tt, x))))

  (* Define each function (arguments and return type) so we can
  call it even before we've created its body *)
  and build_function (params, rt, sstmts) =
    let name = "user_func"
    and formal_types = Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) params) in

    let ftype = L.function_type (ltype_of_typ rt) formal_types in
    let the_function = L.define_function name ftype the_module in
    (* let () = StringHash.add global_funcs name (the_function, 0) in *)

    let local_builder = L.builder_at_end context (L.entry_block the_function) in

    let local_vars = StringHash.create 10 in

    let add_formal (t, n) p =
      L.set_value_name n p;
      let local = L.build_alloca (ltype_of_typ t) n local_builder in
      ignore (L.build_store p local local_builder);
      let a = match t with
        | Arr(x) -> StringHash.add arr_types n x
        | _ -> ()
      in 
      StringHash.add local_vars n local ;
    in

    let add_local_arr n p ltyp = 
      L.set_value_name n p ; 
      let local = L.build_alloca ltyp n local_builder in
      ignore (L.build_store p local local_builder);
      StringHash.add local_vars n local
    in

    let add_local (t, n) =
      let local = L.build_alloca (ltype_of_typ t) n local_builder in
      StringHash.add local_vars n local
    in

    let _ = List.iter2 add_formal params
      (Array.to_list (L.params the_function))
    in


    let build_local_stmt builder = function
        SExpr e -> ignore(expr local_vars builder e)
      | SDecl(t, s, e) -> let e' = expr local_vars builder e in
          ignore(add_formal (t, s) e')
      | SReturn e -> ignore(match rt with
                (* Special "return nothing" instr *)
                A.Void -> L.build_ret_void builder
                (* Build return statement *)
              | _ -> L.build_ret (expr local_vars builder e) builder )
      | _ -> raise (Failure "Not Implemented 2005")
    in

    List.iter (build_local_stmt local_builder) sstmts;

    add_terminal local_builder (match rt with
            A.Void -> L.build_ret_void
          | A.Float -> L.build_ret (L.const_float float_t 0.0)
          (* | A.Arr(t) -> L.build_ret (L.const_pointer_null (ltype_of_typ t)) *)
          | t -> L.build_ret (L.const_int (ltype_of_typ t) 0));
    the_function
  in


  let make_observable (_, e') (cls: observable_class) builder =

    let v_store = L.define_global "target" (L.const_int i32_t 0) the_module in   (* v_store: i32*  *)
    L.build_store e' v_store builder;

    let obs = L.build_malloc obv_t "__obs" builder in

    let curr_vpp = L.build_struct_gep obs 0 "__curr_vpp" builder in
    let curr_vp = L.build_bitcast v_store void_ptr_t "store_to_i8" builder in

    let class_p = L.build_struct_gep obs 5 "__class" builder in
    let cls_number = match cls with
      | SingleObservable -> 1
      | DoubleObservable -> 2
    in
    ignore(L.build_store (L.const_int i32_t cls_number) class_p builder);
    ignore(L.build_store curr_vp curr_vpp builder);

    obs
  in


  let rec oexpr vars builder ((tt, oe) : soexpr) = match oe with
      SOId s -> L.build_load (lookup vars s) s builder
    | SMap(e, oe) ->
      (*   Basically SSubscribe, but returns obs  *)
      (* 1 *)
      let func_p = expr global_vars builder e in
      let upstream = oexpr global_vars builder oe in
      let obs = make_observable ((), L.const_int i32_t 0) SingleObservable builder in

      let obs_store = L.define_global "sub_obs" (L.const_null obv_pt) the_module in
      L.build_store obs obs_store builder;


      let obs_func_vpp = L.build_struct_gep obs 3 "__func" builder in
      let func_vp = L.build_bitcast func_p void_ptr_t "func_to_i8" builder in
      ignore(L.build_store func_vp obs_func_vpp builder);

      (* 2 *)
      L.build_call (lookup global_vars "subscribe") [| upstream; obs |] "" builder;

      (* let ups_child = L.build_struct_gep upstream 4 "__child" builder in
      ignore(L.build_store obs ups_child builder); *)

      let upstream_curr_vpp = L.build_struct_gep upstream 0 "__ups_curr_vpp" builder in
      let upstream_curr_vp = L.build_load upstream_curr_vpp "__ups_curr_vp" builder in
      let obs_upv_vpp = L.build_struct_gep obs 1 "__dwn_upv_vpp" builder in
      L.build_store upstream_curr_vp obs_upv_vpp builder ;

      (* 3 *)
      let upstream_curr_p = L.build_bitcast upstream_curr_vp (L.pointer_type i32_t) "pp" builder in
      let upstream_curr   = L.build_load upstream_curr_p "p" builder in
      let result = L.build_call func_p [| upstream_curr |] "result" builder in

      let obs_curr_vpp = L.build_struct_gep obs 0 "__obs_curr_vpp" builder in
      let obs_curr_vp  = L.build_load obs_curr_vpp "__obs_curr_vp" builder in
      let obs_curr_p   = L.build_bitcast obs_curr_vp (L.pointer_type i32_t) "__obs_curr_p" builder in
      L.build_store result obs_curr_p builder;

      L.build_call (lookup global_vars "onNext") [| obs |] "" builder;  (* won't need but won't hurt to have *)
      ; obs
    | SCombine(e, oe1, oe2) ->
      (*   Basically SSubscribe, but returns obs  *)
      (* 1 *)
      let func_p = expr global_vars builder e in
      let upstream1 = oexpr global_vars builder oe1 in
      let upstream2 = oexpr global_vars builder oe2 in
      let obs = make_observable ((), L.const_int i32_t 0) DoubleObservable builder in

      let obs_store = L.define_global "sub_obs" (L.const_null obv_pt) the_module in
      L.build_store obs obs_store builder;


      let obs_func_vpp = L.build_struct_gep obs 3 "__func" builder in
      let func_vp = L.build_bitcast func_p void_ptr_t "func_to_i8" builder in
      ignore(L.build_store func_vp obs_func_vpp builder);

      (* 2 *)
      L.build_call (lookup global_vars "subscribe") [| upstream1; obs |] "" builder;
      L.build_call (lookup global_vars "subscribe") [| upstream2; obs |] "" builder;

      (* let ups_child1 = L.build_struct_gep upstream1 4 "__child" builder in
      let ups_child2 = L.build_struct_gep upstream2 4 "__child" builder in
      ignore(L.build_store obs ups_child1 builder);
      ignore(L.build_store obs ups_child2 builder); *)

      let upstream1_curr_vpp = L.build_struct_gep upstream1 0 "__ups1_curr_vpp" builder in
      let upstream1_curr_vp = L.build_load upstream1_curr_vpp "__ups1_curr_vp" builder in
      let obs_upv1_vpp = L.build_struct_gep obs 1 "__dwn_upv1_vpp" builder in
      L.build_store upstream1_curr_vp obs_upv1_vpp builder ;

      let upstream2_curr_vpp = L.build_struct_gep upstream2 0 "__ups2_curr_vpp" builder in
      let upstream2_curr_vp = L.build_load upstream2_curr_vpp "__ups2_curr_vp" builder in
      let obs_upv2_vpp = L.build_struct_gep obs 2 "__dwn_upv2_vpp" builder in
      L.build_store upstream2_curr_vp obs_upv2_vpp builder ;

      (* 3 *)
      let upstream1_curr_p = L.build_bitcast upstream1_curr_vp (L.pointer_type i32_t) "pp1" builder in
      let upstream1_curr   = L.build_load upstream1_curr_p "p1" builder in
      let upstream2_curr_p = L.build_bitcast upstream2_curr_vp (L.pointer_type i32_t) "pp2" builder in
      let upstream2_curr   = L.build_load upstream2_curr_p "p2" builder in
      let result = L.build_call func_p [| upstream1_curr; upstream2_curr |] "result" builder in 

      let obs_curr_vpp = L.build_struct_gep obs 0 "__obs_curr_vpp" builder in
      let obs_curr_vp  = L.build_load obs_curr_vpp "__obs_curr_vp" builder in
      let obs_curr_p   = L.build_bitcast obs_curr_vp (L.pointer_type i32_t) "__obs_curr_p" builder in
      L.build_store result obs_curr_p builder;
    
      L.build_call (lookup global_vars "onNext") [| obs |] "" builder;  (* won't need but won't hurt to have *)
      ; obs

    | SOBinop1(oe1, op, e2) -> raise (Failure ("Not Implemented 2020"))
    | SOBinop2(e1, op, oe2) -> raise (Failure ("Not Implemented 2020"))
    | SOBinop3(oe1, op, oe2) -> raise (Failure ("Not Implemented 2020"))
    | SOUnop(op, oe) -> raise (Failure ("Not Implemented 2020"))
    | _ -> raise (Failure ("Not Implemented 2020"))
  in
  let build_global_stmt builder = function
      SExpr e -> ignore(expr global_vars builder e); builder
    | SDecl(t, s, e) -> let e' = expr global_vars builder e in
        let a = match t with
          | Arr(x) -> StringHash.add arr_types s x
          | _ -> ()
        in 
        let _ = add_global_var (t, s, e', builder) in builder
    | SStr_Def(s, b_list) ->
        let tlist = List.map fst b_list in
        let flist = List.map snd b_list in
        let ty = struct_t (list_to_lltype tlist)  in
        StringHash.add global_structs ("struct " ^ s) (ty, tlist, flist) ; builder
    | _ -> raise (Failure "Not Implemented 2002")
      (* let builder = L.builder_at_end context (L.entry_block the_function) in  *)
  in




  let build_obs_stmt builder = function
      SObs(t, e) -> builder

    | SODecl(lt, s, e) ->
      let e' = expr global_vars builder e in
      let obv_ptr = make_observable (lt, e') SingleObservable builder in
      let store = L.define_global s (L.const_null (L.pointer_type obv_t)) the_module in
      (* let () = printf "function name= %s wo\n%!" s in *)
      L.build_store obv_ptr store builder;
      StringHash.add global_vars s store;
      builder
    | SOODecl(lt, s, oe) ->
      let oe' = oexpr global_vars builder oe in
      let store = L.define_global s (L.const_null (L.pointer_type obv_t)) the_module in
      (* let () = printf "function name= %s wo\n%!" s in *)
      L.build_store oe' store builder;
      StringHash.add global_vars s store;
      builder
    | SOAssign(lt, s, e) ->
      let e' = expr global_vars builder e in
      let obs = L.build_load (lookup global_vars s) s builder in

      let curr_vpp = L.build_struct_gep obs 0 "__curr_vpp" builder in                      (* curr_vpp: i8**  *)
      let curr_vp = L.build_load curr_vpp "__curr_vp" builder in                           (* curr_vp : i8*   *)
      let curr_p = L.build_bitcast curr_vp (L.pointer_type i32_t) "i8_to_curr" builder in  (* curr_p  : i32*  *)
      L.build_store e' curr_p builder;
      L.build_call (lookup global_vars "onNext") [| obs |] "" builder;
      builder

    | SOOAssign(lt, s, oe) ->
      (* Maybe we should not allow this *)
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
        3. Call function on upstream value
      *)

      (* 1 *)
      let func_p = expr global_vars builder e in
      let upstream = oexpr global_vars builder oe in
      let obs = make_observable ((), L.const_int i32_t 0) SingleObservable builder in

      let obs_store = L.define_global "sub_obs" (L.const_null obv_pt) the_module in
      L.build_store obs obs_store builder;


      let obs_func_vpp = L.build_struct_gep obs 3 "__func" builder in
      let func_vp = L.build_bitcast func_p void_ptr_t "func_to_i8" builder in
      ignore(L.build_store func_vp obs_func_vpp builder);

      (* 2 *)

      (* let ups_child = L.build_struct_gep upstream 4 "__child" builder in
      ignore(L.build_store obs ups_child builder); *)

      let upstream_curr_vpp = L.build_struct_gep upstream 0 "__ups_curr_vpp" builder in
      let upstream_curr_vp = L.build_load upstream_curr_vpp "__ups_curr_vp" builder in
      let obs_upv_vpp = L.build_struct_gep obs 1 "__dwn_upv_vpp" builder in
      L.build_store upstream_curr_vp obs_upv_vpp builder;
      L.build_call (lookup global_vars "subscribe") [| upstream; obs |] "" builder;


      (*
      let d_func_vpp = L.build_struct_gep obs 3 "__func_vpp" builder in
      let d_func_p  = L.build_load d_func_vpp "__func_p" builder in

      let result = L.build_call d_func_p [| L.const_int i32_t 5 |] "result" builder in 
      *)

      (* 3 *)
      let upstream_curr_p = L.build_bitcast upstream_curr_vp (L.pointer_type i32_t) "pp" builder in
      let upstream_curr   = L.build_load upstream_curr_p "p" builder in
      let result = L.build_call func_p [| upstream_curr |] "result" builder in 
      L.build_call (lookup global_vars "onNext") [| obs |] "" builder;  (* won't need but won't hurt to have *)
      builder
    | _ -> raise (Failure ("Not Implemented 2100"))
  in


  let translate_line glob = match glob with
        SStmt stmt -> ignore(build_global_stmt global_builder stmt)
      | SObs_Stmt obs_stmt -> ignore(build_obs_stmt global_builder obs_stmt)
      (* | SFdecl func -> let top_local_vars = StringHash.create 10 in
          ignore(build_function (func, top_local_vars)) *)

  in
  let () = List.iter translate_line globs in
  let () = add_terminal global_builder (L.build_ret (L.const_int i32_t 0)) in


  let define_subscribe =
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
    L.build_store (L.params subscribe_function).(0) up_local builder;
    L.build_store (L.params subscribe_function).(1) down_local builder;
    let upstream = L.build_load up_local "__ups" builder in      (* upstream   : observable*  *)
    let downstream = L.build_load down_local "__dws" builder in  (* downstream : observable*  *)


    let next' = L.build_struct_gep upstream 4 "__subscription_pp" builder in  (* next' : subscription**  &(upstream->sub) *)
    let next  = L.build_load next' "__subscription_p" builder in              (* next  : subscription*   upstream->sub    *)


    let new_sub = L.build_malloc subscription_t "new" builder in     (* new_sub : subscription*                    *)
    let n_next' = L.build_struct_gep new_sub 0 "__next" builder in   (* n_next' : subscription**  &(new_sub->next) *)
    let n_obs'  = L.build_struct_gep new_sub 1 "__obs" builder in    (* n_obs'  : observable**    &(new_sub->obs)  *)
    L.build_store next n_next' builder;
    L.build_store downstream n_obs' builder; 

    L.build_store new_sub next' builder;
    L.build_ret_void builder;
  in

  let define_next = 
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
    L.build_store (L.params next_function).(0) up_local builder;
    let upstream = L.build_load up_local "__ups" builder in


    let _sub' = L.build_struct_gep upstream 4 "__sub_pp" builder in      (* sub'     : subscription**  *)
    let _sub  = L.build_load _sub' "__sub_p" builder in                  (* sub      : subscription*   *)
    let current' = L.build_alloca subscription_pt "current_p" builder in (* current' : subscription**  *)
    L.build_store _sub current' builder;

    (* obs->__downstreamObservable != NULL *)
    (* let cond = L.build_icmp L.Icmp.Ne (L.const_pointer_null subscription_pt) sub "tmp" builder in
    let cond2 = L.build_icmp L.Icmp.Eq (L.const_int i32_t 1) d_class "tmp2" then_builder in *)


    (* child blocks *)
    let while_pred_bb = L.append_block context "while_pred" next_function in
    let while_pred_builder = L.builder_at_end context while_pred_bb in

    let while_body_bb = L.append_block context "while_body" next_function in
    let while_body_builder = L.builder_at_end context while_body_bb in

    let single_bb = L.append_block context "single" next_function in
    let single_builder = L.builder_at_end context single_bb in

    let double_bb = L.append_block context "double" next_function in
    let double_builder = L.builder_at_end context double_bb in

    let merge_bb = L.append_block context "merge" next_function in
    let merge_builder = L.builder_at_end context merge_bb in


    (* while_pred block *)
    let current = L.build_load current' "current" while_pred_builder in    (* current : subscription*   *)

    let cond = L.build_icmp L.Icmp.Ne (L.const_pointer_null subscription_pt) current "tmp" while_pred_builder in

    let d' = L.build_struct_gep current 1 "__d_ref" while_body_builder in  (* d' : observable**  *)
    let d  = L.build_load d' "__d" while_body_builder in                   (* d  : observable*   *)
    (* L.build_call printf_func [| int_format_str builder; (L.const_int i32_t 100) |] "printf" while_pred_builder; *)



    (* while_body block *)
    let d_class' = L.build_struct_gep d 5 "__d_class_p" while_body_builder in      (* d_class' : i32*    *)
    let d_class  = L.build_load d_class' "__d_class" while_body_builder in         (* d_class  : i32     *)
    let cond2 = L.build_icmp L.Icmp.Eq (L.const_int i32_t 1) d_class "tmp2" while_body_builder in

    let next_sub' = L.build_struct_gep current 0 "__next_p" while_body_builder in  (* next_sub' : subscription**  *)
    let next_sub  = L.build_load next_sub' "__next" while_body_builder in          (* next_sub  : subscription*   *)
    L.build_store next_sub current' while_body_builder;



    (* single block *)
    (* TODO: variable types *)
    let temp_func_pt_typ = L.pointer_type (L.function_type i32_t [| i32_t |]) in

    let d_func_vpp = L.build_struct_gep d 3 "__func_vpp" single_builder in
    let d_func_vp  = L.build_load d_func_vpp "__func_vp" single_builder in
    let d_func_p   = L.build_bitcast d_func_vp temp_func_pt_typ "i8_to_func" single_builder in

    let d_upv_vpp = L.build_struct_gep d 1 "__upv_vpp" single_builder in                         (* d_upv_vpp: i8**  *)
    let d_upv_vp  = L.build_load d_upv_vpp "__upv_vp" single_builder in                          (* d_upv_vp : i8*   *)
    let d_upv_p = L.build_bitcast d_upv_vp (L.pointer_type i32_t) "i8_to_upv" single_builder in  (* d_upv_p  : i32*  *)
    let d_upv   = L.build_load d_upv_p "__upv" single_builder in                                 (* d_upv    : i32   *)

    let result = L.build_call d_func_p [| d_upv |] "result" single_builder in 

    let d_curr_vpp = L.build_struct_gep d 0 "__curr_vpp" single_builder in
    let d_curr_vp  = L.build_load d_curr_vpp "__curr_vp" single_builder in
    let d_curr_p   = L.build_bitcast d_curr_vp (L.pointer_type i32_t) "i8_to_curr" single_builder in
    L.build_store result d_curr_p single_builder;

    (*    onNext(d); *)
    L.build_call next_function [| d |] "" single_builder ;



    (* double block *)
    let temp_func_pt_typ2 = L.pointer_type (L.function_type i32_t [| i32_t; i32_t |]) in

    let d_func2_vpp = L.build_struct_gep d 3 "__func2_vpp" double_builder in
    let d_func2_vp  = L.build_load d_func2_vpp "__func2_vp" double_builder in
    let d_func2_p   = L.build_bitcast d_func2_vp temp_func_pt_typ2 "i8_to_func2" double_builder in

    let d_upv1_vpp = L.build_struct_gep d 1 "__upv1_vpp" double_builder in                          (* d_upv1_vpp: i8**  *)
    let d_upv1_vp  = L.build_load d_upv1_vpp "__upv1_vp" double_builder in                          (* d_upv1_vp : i8*   *)
    let d_upv1_p = L.build_bitcast d_upv1_vp (L.pointer_type i32_t) "i8_to_upv1" double_builder in  (* d_upv1_p  : i32*  *)
    let d_upv1   = L.build_load d_upv1_p "__upv1" double_builder in 

    let d_upv2_vpp = L.build_struct_gep d 2 "__upv2_vpp" double_builder in                          (* d_upv2_vpp: i8**  *)
    let d_upv2_vp  = L.build_load d_upv2_vpp "__upv2_vp" double_builder in                          (* d_upv2_vp : i8*   *)
    let d_upv2_p = L.build_bitcast d_upv2_vp (L.pointer_type i32_t) "i8_to_upv2" double_builder in  (* d_upv2_p  : i32*  *)
    let d_upv2   = L.build_load d_upv2_p "__upv2" double_builder in

    let result2 = L.build_call d_func2_p [| d_upv1; d_upv2 |] "result2" double_builder in

    let d_curr2_vpp = L.build_struct_gep d 0 "__curr_vpp2" double_builder in
    let d_curr2_vp  = L.build_load d_curr2_vpp "__curr_vp2" double_builder in
    let d_curr2_p   = L.build_bitcast d_curr2_vp (L.pointer_type i32_t) "i8_to_curr2" double_builder in
    L.build_store result2 d_curr2_p double_builder;

    (*    onNext(d); *)
    L.build_call next_function [| d |] "" double_builder ;



    (* Connect blocks *)
    L.build_br while_pred_bb builder;
    L.build_cond_br cond while_body_bb merge_bb while_pred_builder;
    L.build_cond_br cond2 single_bb double_bb while_body_builder;
    L.build_br while_pred_bb single_builder;
    L.build_br while_pred_bb double_builder;
    L.build_ret_void merge_builder;
    ()
  in

  the_module
