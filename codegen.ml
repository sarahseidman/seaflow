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

let global_vars = StringHash.create 10
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
  and struct_t   = L.struct_type context     in

  (* function to convert bind list to array of lltypes *)
  let rec list_to_lltype l = Array.of_list (List.map ltype_of_typ l)

  (* Return the LLVM type for a Seaflow type *)
  and ltype_of_typ = function
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
  in

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
    | SArr_Ref(s, e) ->
      let arr = lookup vars s in
      (* how to check for out of bounds index? *)
      let e' = expr vars builder e in
      let p = L.build_struct_gep arr 0 "tmp" builder in
      let p' = L.build_in_bounds_gep p [|e'|] "tmp" builder in
      L.build_load p' "tmp" builder
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
        let init = L.const_struct context (Array.of_list expr_list') in
        let store = L.define_global str init the_module in
        StringHash.add global_vars str store ; builder
    | SArr_Decl(ty, str, expr_list) ->
        let expr_list' = List.map (expr global_vars builder) expr_list in
        let ty' = ltype_of_typ ty in
        let init = L.const_array ty' (Array.of_list expr_list') in
        let store = L.define_global str init the_module in
        StringHash.add global_vars str store ; builder
    | _ -> raise (Failure "Not Implemented 2002")
      (* let builder = L.builder_at_end context (L.entry_block the_function) in  *)
  in

  let translate_line glob = match glob with
        SStmt stmt -> ignore(build_global_stmt global_builder stmt)
      | SObs_Stmt obs_stmt -> raise (Failure "Not Implemented 2000")
      (* | SFdecl func -> let top_local_vars = StringHash.create 10 in
          ignore(build_function (func, top_local_vars)) *)

  in
  let () = List.iter translate_line globs in
  let () = add_terminal global_builder (L.build_ret (L.const_int i32_t 0)) in


  the_module
