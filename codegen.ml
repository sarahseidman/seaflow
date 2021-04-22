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
    | A.Struct(str) -> let (ty, _, _) = try (StringHash.find global_structs ("struct " ^ str))
        with Not_found -> raise (Failure "Struct type mismatch")
      in ty
    | A.Func(param_types, rtype) ->
      let param_ltypes = (List.map ltype_of_typ param_types) in
      let rltype = ltype_of_typ rtype in
      L.pointer_type (L.function_type rltype (Array.of_list param_ltypes))
    | A.Observable t -> L.pointer_type obv_t


  and obv_t = L.named_struct_type context "observable" in
  let obv_pt = L.pointer_type obv_t in
  ignore(L.struct_set_body obv_t [|
    void_ptr_t;   (* pointer to current value *)
    void_ptr_t;   (* pointer to upstream value 1 *)
    void_ptr_t;   (* pointer to upstream value 2 *)
    void_ptr_t;   (* pointer to function *)
    obv_pt;       (* pointer to observable *)
    i32_t         (* observable type 1 if single, 2 if double *)
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
      | A.Func(_, _) as f -> L.const_null (ltype_of_typ f)
      | A.Struct(x) ->
        let (ty, tlist, flist) = StringHash.find global_structs ("struct " ^ x) in
        let empty_vars = List.map init tlist in
        L.const_struct context (Array.of_list empty_vars)
    in
    let store = L.define_global s (init t) the_module in
    (* let () = printf "function name= %s wo\n%!" s in *)
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


  let next_ftype = L.function_type void_t [| obv_pt |] in
  let next_function = L.define_function "onNext" next_ftype the_module in
  StringHash.add global_vars "onNext" next_function;


  let lookup vars n = try StringHash.find vars n
      with Not_found ->
        try StringHash.find global_vars n
        with Not_found -> raise (Failure ("Variable " ^ n ^ " not found"))
  in


  let rec expr vars builder ((tt, e) : sexpr) = match e with
      SLiteral i   -> L.const_int i32_t i
    | SChliteral c -> L.const_int i8_t (Char.code c)
    | SFliteral l  -> L.const_float_of_string float_t l
    | SId s        -> L.build_load (lookup vars s) s builder
    | SRef(str_name, type_of_struct, fieldname) ->
      let loc = lookup vars str_name in
      let (ty, tlist, flist) = StringHash.find global_structs type_of_struct in
      (* https://www.howtobuildsoftware.com/index.php/how-do/bRlD/list-find-ocaml-ml-memory-consumption-finding-an-item-in-a-list-and-returning-its-index-ocaml *)
      let rec find x lst =
        match lst with
        | [] -> raise (Failure "Not Found")
        | h :: t -> if x = h then 0 else 1 + find x t
      in
      let idx = find fieldname flist in
      let p = L.build_struct_gep loc idx "tmp" builder in
      L.build_load p "z" builder

    | SIf (e1, e2, e3) ->
      let e1' = expr vars builder e1
      and e2' = expr vars builder e2
      and e3' = expr vars builder e3 in
      L.build_select e1' e2' e3' "tmp" builder
    | SFuncExpr(params, rt, sstmts) ->
      build_function (params, rt, sstmts)
    | SBinop (e1, op, e2) ->
      let e1' = expr vars builder e1
      and e2' = expr vars builder e2 in
      (match op with
        A.Add     -> L.build_add
      | A.Sub     -> L.build_sub
      | A.Mult    -> L.build_mul
      | A.Div     -> L.build_sdiv
      | A.And     -> L.build_and
      | A.Or      -> L.build_or
      | A.Equal   -> L.build_icmp L.Icmp.Eq
      | A.Neq     -> L.build_icmp L.Icmp.Ne
      | A.Less    -> L.build_icmp L.Icmp.Slt
      | A.Leq     -> L.build_icmp L.Icmp.Sle
      | A.Greater -> L.build_icmp L.Icmp.Sgt
      | A.Geq     -> L.build_icmp L.Icmp.Sge
      ) e1' e2' "tmp" builder
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
          (* ignore(add_local (t, s, e', builder)) *)
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
          | t -> L.build_ret (L.const_int (ltype_of_typ t) 0));
    the_function
  in


    (* let local_vars =
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
    in *)



  (* let build_global_func fdecl =
    function_decl fdecl *)


  (* SFdecl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    (* locals : bind list; *)
    sbody : sstmt list;
  } *)




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
      let ups_child = L.build_struct_gep upstream 4 "__child" builder in
      ignore(L.build_store obs ups_child builder);

      let upstream_curr_vpp = L.build_struct_gep upstream 0 "__ups_curr_vpp" builder in
      let upstream_curr_vp = L.build_load upstream_curr_vpp "__ups_curr_vp" builder in
      let obs_upv_vpp = L.build_struct_gep obs 1 "__dwn_upv_vpp" builder in
      L.build_store upstream_curr_vp obs_upv_vpp builder ;

      (* 3 *)
      L.build_call (lookup global_vars "onNext") [| upstream |] "" builder;
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
      let ups_child1 = L.build_struct_gep upstream1 4 "__child" builder in
      let ups_child2 = L.build_struct_gep upstream2 4 "__child" builder in
      ignore(L.build_store obs ups_child1 builder);
      ignore(L.build_store obs ups_child2 builder);

      let upstream1_curr_vpp = L.build_struct_gep upstream1 0 "__ups1_curr_vpp" builder in
      let upstream1_curr_vp = L.build_load upstream1_curr_vpp "__ups1_curr_vp" builder in
      let obs_upv1_vpp = L.build_struct_gep obs 1 "__dwn_upv1_vpp" builder in
      L.build_store upstream1_curr_vp obs_upv1_vpp builder ;

      let upstream2_curr_vpp = L.build_struct_gep upstream2 0 "__ups2_curr_vpp" builder in
      let upstream2_curr_vp = L.build_load upstream2_curr_vpp "__ups2_curr_vp" builder in
      let obs_upv2_vpp = L.build_struct_gep obs 2 "__dwn_upv2_vpp" builder in
      L.build_store upstream2_curr_vp obs_upv2_vpp builder ;

      (* 3 *)
      L.build_call (lookup global_vars "onNext") [| upstream1 |] "" builder;
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
        let _ = add_global_var (t, s, e', builder) in builder
    | SStr_Def(s, b_list) ->
        let tlist = List.map fst b_list in
        let flist = List.map snd b_list in
        let ty = struct_t (list_to_lltype tlist)  in
        StringHash.add global_structs ("struct " ^ s) (ty, tlist, flist) ; builder
    | SStr_Decl(ty, str, expr_list) ->
        let expr_list' = List.map (expr global_vars builder) expr_list in
        (* let namedty = try fst (StringHash.find global_structs (A.string_of_typ ty))
            with Not_found -> raise (Failure "Struct not found")
        in *)
        let init = L.const_struct context (Array.of_list expr_list') in
        let store = L.define_global str init the_module in

        (* let store = L.build_alloca namedty str builder in  *)
        (* let alloc = L.build_alloca namedty str builder in *)
        (* need an llvalue that is the 2 ints *)
        (* L.struct_set_body namedty expr_list' ; *)
        (* L.const_insertvalue store (L.const_int i32_t 3) 0 ; *)

        (* L.build_store init store builder ; *)
        StringHash.add global_vars str store ; builder
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
      let ups_child = L.build_struct_gep upstream 4 "__child" builder in
      ignore(L.build_store obs ups_child builder);

      let upstream_curr_vpp = L.build_struct_gep upstream 0 "__ups_curr_vpp" builder in
      let upstream_curr_vp = L.build_load upstream_curr_vpp "__ups_curr_vp" builder in
      let obs_upv_vpp = L.build_struct_gep obs 1 "__dwn_upv_vpp" builder in
      L.build_store upstream_curr_vp obs_upv_vpp builder;


      (*
      let d_func_vpp = L.build_struct_gep obs 3 "__func_vpp" builder in
      let d_func_p  = L.build_load d_func_vpp "__func_p" builder in

      let result = L.build_call d_func_p [| L.const_int i32_t 5 |] "result" builder in 
      *)

      (* 3 *)
      L.build_call (lookup global_vars "onNext") [| upstream |] "" builder;
      builder
    (* | SComplete(_, oe) -> 
       *)
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


  let define_next = 
    (*
    void onNext(observable* obs) {

      if (obs->__downstreamObservable != NULL) {
        observable* d = obs->__downstreamObservable;
        d->__curr = d->__func(d->__upstreamValue);
        onNext(d);
        return;
      }
      return;
    *)

    (* Define function *)
    let next_function = StringHash.find global_vars "onNext" in
    let builder = L.builder_at_end context (L.entry_block next_function) in
    L.set_value_name "upstream" (L.params next_function).(0);
    let up_local = L.build_alloca obv_pt "upstream" builder in
    L.build_store (L.params next_function).(0) up_local builder;
    let upstream = L.build_load up_local "__ups" builder in


    let d' = L.build_struct_gep upstream 4 "__d_ref" builder in  (* d' : observable**  *)
    let d = L.build_load d' "__d" builder in            (* d  : observable*   *)

    (* observable* d = obs->__downstreamObservable; *)
    let then_bb = L.append_block context "then" next_function in
    let then_builder = L.builder_at_end context then_bb in

    let d_class' = L.build_struct_gep d 5 "__d_class_p" then_builder in  (* d_class' : i32*  *)
    let d_class  = L.build_load d_class' "__d_class" then_builder in     (* d_class  : i32   *)

    (* obs->__downstreamObservable != NULL *)
    let cond = L.build_icmp L.Icmp.Ne (L.const_pointer_null obv_pt) d "tmp" builder in
    let cond2 = L.build_icmp L.Icmp.Eq (L.const_int i32_t 1) d_class "tmp2" then_builder in



    let single_bb = L.append_block context "single" next_function in
    let single_builder = L.builder_at_end context single_bb in

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
    

    (* onNext(d); *)
    L.build_call next_function [| d |] "" single_builder ;



    (* double observble *)
    
    let double_bb = L.append_block context "double" next_function in
    let double_builder = L.builder_at_end context double_bb in
    
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


    (* onNext(d); *)
    L.build_call next_function [| d |] "" double_builder ;



    let merge_bb = L.append_block context "merge" next_function in
    let merge_builder = L.builder_at_end context merge_bb in

    L.build_cond_br cond then_bb merge_bb builder;
    L.build_cond_br cond2 single_bb double_bb then_builder;
    L.build_ret_void single_builder;
    L.build_ret_void double_builder;
    L.build_ret_void merge_builder;
    ()
  in

  the_module
