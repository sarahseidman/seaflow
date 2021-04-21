(* Semantic checking for the MicroC compiler *)

open Ast
open Sast
open Printf
open Utils

module StringMap = Map.Make(String)

module StringHash = Hashtbl.Make(struct
  type t = string
  let equal x y = x = y
  let hash = Hashtbl.hash
end)

let global_vars = StringHash.create 10
(* let function_decls = StringHash.create 10 *)
let struct_defs = StringHash.create 10

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)


let match_struct_element_name name (t, element) =
  if name = element then true else false


let check (globs) =

  let print_hash name ht =
    let () = Printf.printf "Table: %s keys\n" name in
    StringHash.iter (fun x _ -> Printf.printf "  %s\n" x) ht
  in

  let _ = StringHash.add global_vars "printi" (Func([Int], Void)) in
  let _ = StringHash.add global_vars "printf" (Func([Float], Float)) in
  let _ = StringHash.add global_vars "printc" (Func([Char], Char)) in

  (*
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name {
      typ = Void;
      fname = name; 
      formals = [(ty, "x")];
      (*locals = [];*) body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("printi", Int);
                                                 ("printf", Float);
                                                 ("printbig", Int);
                                                 ("printc", Char); ]
  in

  let add_func fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringHash.mem function_decls n -> make_err dup_err  
       | _ ->  StringHash.add function_decls n fd
  in

  (* let function_decls = List.fold_left ad2d_func built_in_decls []
  in *)

  (* Return a function from our symbol table *)
  let find_func s = 
    try StringHash.find function_decls s
    with Not_found -> try StringMap.find s built_in_decls
      with Not_found ->
        raise (Failure ("unrecognized function " ^ s))
  in *)

  (* let global_vars = StringMap.empty in *)

  let type_of_identifier vars s =
    try StringHash.find vars s
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in


  let check_assign lvaluet rvaluet err =
    if lvaluet = rvaluet then lvaluet else raise (Failure err)
  in

  let rec expr vars = function
      Literal  l -> (Int, SLiteral l)
    | Fliteral l -> (Float, SFliteral l)
    | Chliteral c -> (Char, SChliteral c)
    | Id s       -> (type_of_identifier vars s, SId s)
    (* | Sid s      -> (type_of_identifier vars s, SSid s) *)
    | Ref(e, s) ->
      let (t', e') = expr vars e in
      let str_name = match e' with
        | SId(x) -> x
        | _ -> raise (Failure ("Struct field")) 
      in
      (* let str_name = Id(string_of_expr e) in *)
      let l = try StringHash.find struct_defs (string_of_typ t')
        with Not_found -> raise (Failure (string_of_expr e ^ " is not a struct")) in
      let element = List.find_opt (match_struct_element_name s) l in
      let element_type = match element with
        | Some (t2, _) -> t2 
        | None -> raise (Failure ("field " ^ s ^ " is not part of this struct"))
      in (element_type, SRef(str_name, string_of_typ t', s))
    | Noexpr     -> (Void, SNoexpr)
    | If (e1, e2, e3) ->
        let (t1, e1') = expr vars e1
        and (t2, e2') = expr vars e2
        and (t3, e3') = expr vars e3 in
        let same = t2 = t3 in
        (* e2 and e3 must be same type *)
        let ty = match t2 with
          Int when same -> Int
        | Float when same -> Float
        | Char when same -> Char
        | _ -> raise (Failure ("illegal if; types must match"))
        in (ty, SIf((t1, e1'), (t2, e2'), (t3, e3')))
    | Binop(e1, op, e2) as e -> 
        let (t1, e1') = expr vars e1 
        and (t2, e2') = expr vars e2 in
        (* All binary operators require operands of the same type *)
        let same = t1 = t2 in
        (* Determine expression type based on operator and operand types *)
        let ty = match op with
          Add | Sub | Mult | Div when same && t1 = Int   -> Int
        | Add | Sub | Mult | Div when same && t1 = Float -> Float
        | Add | Sub | Mult | Div when same && t1 = Char -> Char
        | Equal | Neq            when same               -> Int
        | Less | Leq | Greater | Geq
                   when same && (t1 = Int || t1 = Float || t1 = Char) -> Int
        | And | Or when same && t1 = Int -> Int
        | _ -> raise (Failure ("illegal binary operator  ^
                                string_of_typ t1 ^  ^ string_of_op op ^  ^
                                string_of_typ t2 ^  in  ^ string_of_expr e"))
        in (ty, SBinop((t1, e1'), op, (t2, e2')))
    | Unop(op, e) -> 
        let (ty, e') = expr vars e in (ty, SUnop(op, (ty, e')))
    | Call(f, args) as call -> 
        let (t', f') = expr vars f in
        (* let fd = find_func vars fname in *)
        (* let (formals, rtype) = match type_of_identifier vars fname with
          | Func(param_types, rtype) -> (param_types, rtype)
          | _ ->  raise (Failure ("must be Func type"))
        in *)
        

        let (formals, rtype) = match t' with
          | Func(formals, rtype) -> (formals, rtype) 
          | _ ->  raise (Failure ("must be Func type"))
        in

        let param_length = List.length formals in
        if List.length args != param_length then
          raise (Failure ("expecting ^ string_of_int param_length ^ 
                            arguments in  ^ string_of_expr call"))
        else let check_call ft e = 
          let (et, e') = expr vars e in 
          let err = "illegal argument found " ^ string_of_typ et ^
              " expected "  ^ string_of_typ ft ^ " in " ^ string_of_expr e
          in (check_assign ft et err, e')
        in 
        let args' = List.map2 check_call formals args in
        (match f' with
          | SId(x) when x = "printi"
            || x = "printc"
            || x = "printf" -> (rtype, SBCall(x, args'))
          | _ as x                   -> (rtype, SCall((t', x), args')))
    | FuncExpr(params, stmts) ->
        let (ftype, rtype, sstmts) = check_func_decl vars (params, stmts) in
        (ftype, SFuncExpr(params, rtype, sstmts))
  and check_stmt vars = function
      Expr e -> SExpr (expr vars e)
    | Decl(lt, var, e) as ex -> 
      let (rt, e') = expr vars e in
      let err = "illegal assignment  ^ string_of_typ lt ^  =  ^ 
        string_of_typ rt ^  in  ^ string_of_expr ex" in
      StringHash.add vars var lt ;
      (*(check_assign lt rt err, *) SDecl(lt, var, (rt, e'))
    | Return e -> let (t, e') = expr vars e in SReturn (t, e') 
      (* if t = func.typ then SReturn (t, e') 
      else raise (Failure ("return gives  ^ string_of_typ t ^  expected  ^
                            string_of_typ func.typ ^  in  ^ string_of_expr e")) *)
    | Str_Decl(t, s, expr_list) ->
      StringHash.add vars s t ;
      let check_sdecl (t, expr_list) =
        let l = try StringHash.find struct_defs (string_of_typ t)
          with Not_found -> raise (Failure ("undeclared struct " ^ string_of_typ t))
        in if List.length l == List.length expr_list then () else raise (Failure ("incorrect arguments number for " ^ string_of_typ t))
      in
      let _ = check_sdecl(t, expr_list) in
      let expr_list' = List.map (expr vars) expr_list
      in SStr_Decl(t, s, expr_list')
    | Str_Def(s, b_list) ->
      let _ = StringHash.add struct_defs ("struct " ^ s) b_list 
      in SStr_Def(s, b_list)
    | Block sl -> 
      let rec check_stmt_list v = function
          [Return _ as s] -> [check_stmt v s]
        | Return _ :: _   -> raise (Failure "nothing may follow a return")
        | Block sl :: ss  -> check_stmt_list v (sl @ ss) (* Flatten blocks *)
        | s :: ss         -> let parsed_stmt = check_stmt v s in parsed_stmt :: check_stmt_list v ss
        | []              -> []
      in SBlock(check_stmt_list vars sl)


  and check_func_decl vars anon_func : (Ast.typ * Ast.typ * Sast.sstmt list) = 
    let (params, body) = anon_func in

    let local_vars = StringHash.copy vars in
    let add_to_hash (vtype, vname) =
      StringHash.add local_vars vname vtype
    in
    List.iter add_to_hash params;

    let sbody = match check_stmt local_vars (Block body) with
        SBlock(sl) -> sl
      | _ -> raise (Failure ("internal error2: block didn't become a block?"))
    in

    let param_types = List.map fst params in

    let rtype = match list_last sbody with
      | SReturn(t, e) -> t
      | _ -> Void
    in
    (Func(param_types, rtype), rtype, sbody)
  in


  let check_obs_stmt = function
      Obs e -> SObs e
    | _ -> raise (Failure ("Not Implemented 1000"))
  in


  let fdecl_to_assign_stmt vars func =
    let (func_type, rtype, sstmt) = check_func_decl vars (func.formals, func.body) in

    StringHash.add vars func.fname func_type;
    SDecl(func_type, func.fname, (func_type, SFuncExpr(func.formals, rtype, sstmt)))
  in

  let check_glob glob = match glob with
      Stmt stmt -> SStmt(check_stmt global_vars stmt)
    | Obs_Stmt obs_stmt -> SObs_Stmt(check_obs_stmt obs_stmt)
    | Fdecl func -> SStmt(fdecl_to_assign_stmt global_vars func)
    (* | Fdecl func -> add_func func ; SFdecl(check_func_decl global_vars func) *)


  in (List.map check_glob globs)

