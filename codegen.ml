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
  and void_t     = L.void_type   context in

  (* Return the LLVM type for a MicroC type *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Char  -> i8_t
    | A.Float -> float_t
    | A.Void  -> void_t
  in

  let printf_t : L.lltype = 
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
    L.declare_function "printf" printf_t the_module in (* L.declare_function returns the function if it already exits in the module *)


  let global_vars : L.llvalue StringMap.t = StringMap.empty in

  let global_funcs : L.llvalue StringMap.t = StringMap.empty in

  let main_ftype = L.function_type i32_t [| |] in
  let main_function = L.define_function "main" main_ftype the_module in
  let _ = StringMap.add "main" (main_function, ()), global_funcs in

  let global_builder = L.builder_at_end context (L.entry_block main_function) in


  let add_global_var (t, s, v, builder) =
    let init = match t with
        A.Float -> L.const_float float_t 0.0
      | A.Int   -> L.const_int i32_t 0
      | A.Char  -> L.const_int i8_t 0
    in 
    let store = L.define_global s init the_module in
    let _ = StringMap.add s store global_vars in
    L.build_store v store builder
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


  
  let lookup n = try StringMap.find n global_vars
                 with Not_found -> raise (Failure "Variable not found")
  in

  
  let rec expr builder ((_, e) : sexpr) = match e with
      SLiteral i   -> L.const_int i32_t i
    | SChliteral c -> L.const_int i8_t (Char.code c)
    | SFliteral l  -> L.const_float_of_string float_t l
    | SId s        -> L.build_load (lookup s) s builder
    | SCall("printi", [e]) ->
        L.build_call printf_func [| int_format_str builder; (expr builder e) |]
          "printf" builder
    | SCall("printc", [e]) ->
        L.build_call printf_func [| char_format_str builder; (expr builder e) |]
          "printf" builder
    | SCall("printf", [e]) ->
        L.build_call printf_func [| float_format_str builder; (expr builder e) |]
          "printf" builder
    | _ -> raise (Failure "Not Implemented 2003")
  in
    
  
  let build_global_stmt builder = function
      SExpr e -> ignore(expr builder e); builder
    | SDecl(t, s, e) -> let e' = expr builder e in
        ignore(add_global_var (t, s, e', builder)); builder
    | _ -> raise (Failure "Not Implemented 2002")
      (* let builder = L.builder_at_end context (L.entry_block the_function) in  *)
  in

  let translate_line glob = match glob with
        SStmt stmt -> ignore(build_global_stmt global_builder stmt)
      | SObs_Stmt obs_stmt -> raise (Failure "Not Implemented 2000")
      | SFdecl func -> raise (Failure "Not Implemented 2001") 
  in 
  let () = List.iter translate_line globs in
  let () = add_terminal global_builder (L.build_ret (L.const_int i32_t 0)) in

  
  the_module
