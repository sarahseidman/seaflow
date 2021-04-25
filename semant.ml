(* Semantic checking for the MicroC compiler *)

open Ast
open Sast
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
let struct_by_body = StringHash.create 10

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)


let match_struct_element_name name (_, element) =
  if name = element then true else false


(* Observable can't be observable of observable *)
let get_observable_inner_type (ot: typ) : typ = match ot with
  | Observable(t) -> (match t with
    | Observable(_)
    | Func(_, _) -> raise (Failure ("Observable can't be observable of observable or func"))
    | _ as x -> x)
  | _ as x -> raise (Failure (string_of_typ x ^ " is not an observable"))



let check (globs) =

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

  let find_body s = try StringHash.find struct_by_body (string_of_typ s)
              with Not_found -> raise (Failure ("struct type not found: " ^ string_of_typ s)) 
       in

  let check_assign lvaluet rvaluet err =
    match (lvaluet, rvaluet) with
      (Struct(_), Struct(_)) -> let xbody = find_body lvaluet in
        let ybody = find_body rvaluet in
        let same = xbody = ybody in
        if same then lvaluet else raise (Failure err)
    | (Struct(_), Sbody(_)) -> let xbody = find_body lvaluet in
        let same = xbody = rvaluet in
        if same then lvaluet else raise (Failure err)
    | _ -> let same = lvaluet = rvaluet in
        if same then lvaluet else raise (Failure err)
  in

  let rec expr vars = function
      Literal  l -> (Int, SLiteral l)
    | Bliteral l -> (Bool, SBliteral l)
    | Fliteral l -> (Float, SFliteral l)
    | Chliteral c -> (Char, SChliteral c)
    | Strliteral s ->
      let ty = Char in
      let split =
        let rec exp i l =
          if i < 2 then l else exp (i - 1) (Chliteral(s.[i]) :: l) in
        exp (String.length s - 3) [Chliteral(Char.chr 0)]
      in
      let e = List.map (expr vars) split in
      (Arr(ty), SAliteral(ty, e))
    | Aliteral a -> 
      let (ty, _) = expr vars (List.hd a) in
      let expr_list = List.map (expr vars) a in
      let compare x (y,_) =
        if x = y then () else raise (Failure("array literal: type mismatch " ^ string_of_typ x ^ " != " ^ string_of_typ y))
      in let _ = List.map (compare ty) expr_list in
      (Arr(ty), SAliteral (ty, expr_list))
    | Id s       -> (type_of_identifier vars s, SId s)
    | Ref(e, s) ->
      let (t', e') = expr vars e in
      (* this is ugly but it allows reference of array literals too! *)
      let str_name = match e' with
        | SId(x) -> x
        | SAliteral(_,_) -> "NONE"
        | _ -> raise (Failure ("Struct field")) 
      in
      let struct_ref ty str = 
        let l = StringHash.find struct_defs (string_of_typ ty) in
        let element = List.find_opt (match_struct_element_name str) l in
        let element_type = match element with
          | Some (t2, _) -> t2 
          | None -> raise (Failure ("field " ^ s ^ " is not part of this struct"))
        in (element_type, SRef(str_name, string_of_typ ty, str))
      in
      let arr_len name str = 
        let _ = if (str = "length") then () else raise (Failure ("invalid reference: is not struct or array length")) in
        let ty_of_lit = function 
            | SAliteral(x, _) -> Arr(x)
            | _ -> raise (Failure ("shouldn't happen"))
        in
        let ty = if (name = "NONE") then (ty_of_lit e') else type_of_identifier vars name in 
        let _ = try typ_of_arr ty
            with Match_failure(_) -> raise (Failure ("cannot take length of type " ^ string_of_typ ty)) in
        let e' = expr vars e in
        (Int, SLen(e'))
      in
      let found = StringHash.mem struct_defs (string_of_typ t') in
      let ret = match found with 
        false -> arr_len str_name s
        | true -> struct_ref t' s
      in ret
    | Sliteral(expr_list) -> 
      let e' = List.map (expr vars) expr_list in
      let ty = List.map fst e' in
      (Sbody(ty), SSliteral(e'))
    | Arr_Ref(e1, e2) ->
      let ty = match (expr vars e1) with
        | (_, SId(s)) -> typ_of_arr (type_of_identifier vars s)
        | (_, SAliteral(t, _)) -> t
        | _ -> raise (Failure ("can only reference an array!"))
      in 
      let e1' = expr vars e1 in
      (* basically just check that the array exists and that expr is an int *)
      let (idx_ty, e') = expr vars e2 in
      let _ = if idx_ty = Int then () 
          else raise(Failure ("array index must be of type int, not " ^ string_of_typ idx_ty)) in
      (ty, SArr_Ref(e1', (idx_ty, e')))
    | Noexpr     -> (Void, SNoexpr)
    | Binop(e1, op, e2) as e -> 
        let (t1, e1') = expr vars e1 
        and (t2, e2') = expr vars e2 in
        (* All binary operators require operands of the same type *)
        let same = t1 = t2 in
        (* Determine expression type based on operator and operand types *)

        (match op with
          | And | Or ->
            let (_t1, _e1') = match t1 with
              | Int -> expr vars (Binop(e1, Neq, Literal(0)))
              | Bool -> (t1, e1')
              | _ -> raise (Failure "&& and || must be used with integers")
            in
            let (_t2, _e2') = match t2 with
              | Int -> expr vars (Binop(e2, Neq, Literal(0)))
              | Bool -> (t2, e2')
              | _ -> raise (Failure "&& and || must be used with integers")
            in (Bool, SBinop((_t1, _e1'), op, (_t2, _e2')))
          | _ -> let ty = match op with
              Add | Sub | Mult | Div when same && t1 = Int   -> Int
            | Add | Sub | Mult | Div when same && t1 = Float -> Float
            | Add | Sub | Mult | Div when same && t1 = Char  -> Char
            | Equal | Neq            when same               -> Bool
            | Less | Leq | Greater | Geq
                      when same && (t1 = Int || t1 = Float || t1 = Char) -> Bool
            | Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq when ((t1 = Float && t2 = Int) || (t1 = Int && t2 = Float)) -> Float
            | Add when (typ_of_arr t1) = (typ_of_arr t2) -> t1
            | _ -> raise (Failure ("illegal binary operator "  ^
                                    string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                                    string_of_typ t2 ^ " in " ^ string_of_expr e))
            in (ty, SBinop((t1, e1'), op, (t2, e2'))))
    | Unop(op, e) -> 
        let (ty, e') = expr vars e in (ty, SUnop(op, (ty, e')))
    | Call(f, args) as call -> 
        let (t', f') = expr vars f in

        let (formals, rtype) = match t' with
          | Func(formals, rtype) -> (formals, rtype) 
          | _ ->  raise (Failure ("must be Func type"))
        in

        let param_length = List.length formals in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
        else let check_call ft e = 
          let (et, e') = expr vars e in 
          let err = "illegal argument: found "  ^ string_of_typ et ^
              " expected "  ^ string_of_typ ft ^  " in "  ^ string_of_expr e
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
    | _ -> raise(Failure ("expression not implemented"))
  and check_stmt vars = function
      Expr e -> SExpr (expr vars e)
    | Decl(lt, var, e) as ex -> 
      let _ = match lt with
          Void -> raise (Failure "cannot assign to void type!")
        | _ -> ()
      in
      let _ = match (StringHash.find_opt vars var) with
      | Some(_) -> raise (Failure "variable has already been assigned!")
      | None -> ()
      in
      StringHash.add vars var lt ;

      let (rt, e') = expr vars e in
      let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
        string_of_typ rt ^ " in " ^ string_of_stmt ex in
      ignore(check_assign lt rt err) ; SDecl(lt, var, (rt, e'))
    | Return e -> let (t, e') = expr vars e in SReturn (t, e') 
      (* if t = func.typ then SReturn (t, e') 
      else raise (Failure ("return gives  ^ string_of_typ t ^  expected  ^
                            string_of_typ func.typ ^  in  ^ string_of_expr e")) *)
    | Str_Def(s, b_list) ->
      let tlist = List.map fst b_list in
      let name = ("struct " ^ s) in
      StringHash.add struct_defs name b_list ; StringHash.add struct_by_body name (Sbody(tlist)) ;
      SStr_Def(s, b_list)
    | Block sl -> 
      let rec check_stmt_list v = function
          [Return _ as s] -> [check_stmt v s]
        | Return _ :: _   -> raise (Failure "nothing may follow a return")
        | Block sl :: ss  -> check_stmt_list v (sl @ ss) (* Flatten blocks *)
        | s :: ss         -> let parsed_stmt = check_stmt v s in parsed_stmt :: check_stmt_list v ss
        | []              -> []
      in SBlock(check_stmt_list vars sl)
    | If(ltyp, var, cond, e1, e2) ->
      let (tc, c') = expr vars cond
      and (t1, e1') = expr vars e1
      and (t2, e2') = expr vars e2 in
      let same = t1 = t2 in
      (* e1 and e2 must be same type *)
      let rtyp = match t1 with
        Int when same -> Int
      | Float when same -> Float
      | Char when same -> Char
      | Arr(x) when same -> Arr(x)
      | _ -> raise (Failure ("illegal if; types must match in then and else branches")) in
      let same2 = ltyp = rtyp in
      let _ = match ltyp with
          Int when same2 -> Int
        | Float when same2 -> Float
        | Char when same2 -> Char
        | Arr(x) when same2 -> Arr(x)
        | _ -> raise (Failure ("illegal if; types must match for lval and rval")) in
      StringHash.add vars var ltyp ;
      SIf(ltyp, var, (tc, c'), (t1, e1'), (t2, e2'))
    | _ -> raise (Failure ("statement not implemented"))


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
    
    let check_param param = (match param with
        Int -> ()
      | Void -> raise (Failure ("cannot have void formal!"))
      | _ -> ()
    )
    in

    let param_types = List.map fst params in
    let _ = List.map check_param param_types in

    let check_empty = match sbody with
      | [] -> raise (Failure ("a function cannot have an empty body"))
      | _ -> list_last sbody
    in

    let rtype = match check_empty with
      | SReturn(t, _) -> t
      | _ -> Void
    in
    (Func(param_types, rtype), rtype, sbody)
  in


  let rec oexpr vars = function
      OId(s) -> (type_of_identifier vars s, SOId s)
    | OBinop1(oe1, op, e2) ->
      let (ot1, oe1') = oexpr vars oe1 
      and (t2, e2') = expr vars e2 in

      let t1 = get_observable_inner_type ot1 in
      let same = t1 = t2 in
      let ty = match op with
        | Add | Sub | Mult | Div when same && t1 = Int   -> Int
        | Add | Sub | Mult | Div when same && t1 = Float -> Float
        | Add | Sub | Mult | Div when same && t1 = Char -> Char
        | Equal | Neq            when same               -> Int
        | Less | Leq | Greater | Geq
                  when same && (t1 = Int || t1 = Float || t1 = Char) -> Int
        | And | Or when same && t1 = Int -> Int
        | Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater
        | Geq when ((t1 = Float && t2 = Int) || (t1 = Int && t2 = Float)) -> Float
        | _ -> raise (Failure ("illegal binary operator "  ^
                                string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                                string_of_typ t2))
      in (Observable(ty), SMap(
        (Func([t1],ty), SFuncExpr(
          [(t1, "x")],
          ty,
          [SReturn (
            ty,
            SBinop(
              (t1, SId("x")),
              op,
              (t2, e2')
            )
          )]
        )),
        (ot1, oe1')
      ))
    | OBinop2(e1, op, oe2) ->
      let (t1, e1') = expr vars e1 
      and (ot2, oe2') = oexpr vars oe2 in

      let t2 = get_observable_inner_type ot2 in
      let same = t1 = t2 in
      let ty = match op with
        | Add | Sub | Mult | Div when same && t1 = Int   -> Int
        | Add | Sub | Mult | Div when same && t1 = Float -> Float
        | Add | Sub | Mult | Div when same && t1 = Char -> Char
        | Equal | Neq            when same               -> Int
        | Less | Leq | Greater | Geq
                  when same && (t1 = Int || t1 = Float || t1 = Char) -> Int
        | And | Or when same && t1 = Int -> Int
        | Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater
        | Geq when ((t1 = Float && t2 = Int) || (t1 = Int && t2 = Float)) -> Float
        | _ -> raise (Failure ("illegal binary operator "  ^
                                string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                                string_of_typ t2))
      in (Observable(ty), SMap(
        (Func([t2],ty), SFuncExpr(
          [(t2, "x")],
          ty,
          [SReturn (
            ty,
            SBinop(
              (t1, e1'),
              op,
              (t2, SId("x"))
            )
          )]
        )),
        (ot2, oe2')
      ))
    | OBinop3(oe1, op, oe2) ->
      let (ot1, oe1') = oexpr vars oe1 
      and (ot2, oe2') = oexpr vars oe2 in

      let t1 = get_observable_inner_type ot1 in
      let t2 = get_observable_inner_type ot2 in
      let same = t1 = t2 in
      let ty = match op with
          Add | Sub | Mult | Div when same && t1 = Int   -> Int
        | Add | Sub | Mult | Div when same && t1 = Float -> Float
        | Add | Sub | Mult | Div when same && t1 = Char -> Char
        | Equal | Neq            when same               -> Int
        | Less | Leq | Greater | Geq
                  when same && (t1 = Int || t1 = Float || t1 = Char) -> Int
        | And | Or when same && t1 = Int -> Int
        | Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater
        | Geq when ((t1 = Float && t2 = Int) || (t1 = Int && t2 = Float)) -> Float
        | _ -> raise (Failure ("illegal binary operator "  ^
                                string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                                string_of_typ t2))
      in (Observable(ty), SCombine(
        (Func([t1;t2],ty), SFuncExpr(
          [(t1, "x"); (t2, "y")],
          ty,
          [SReturn (
            ty,
            SBinop(
              (t1, SId("x")),
              op,
              (t2, SId("y"))
            )
          )]
        )),
        (ot1, oe1'),
        (ot2, oe2')
      ))
    | OUnop(op, oe) ->
      let (ty, oe') = oexpr vars oe in (ty, SOUnop(op, (ty, oe')))
    | Map(e, oe) -> 
      let (t, e') = expr vars e in
      let (ot, oe') = oexpr vars oe in

      let (args, rt) = match t with
        | Func(args, rt) when rt != Void -> (args, rt)
        | _ as x-> raise (Failure ("illegal expression of type " ^ string_of_typ x ^
                                   " with map()"))
      in

      let it' = match ot with
        | Observable x -> x
        | _ -> raise (Failure ("second arguement of map must be an observable"))
      in let _ = match args with
        | [a] when a = it' -> ()
        | _ -> raise (Failure ("map function type does not match"))
      in
      (Observable rt, SMap((t, e'), (ot, oe')))
    | Combine(e, oe1, oe2) -> 
      let (t, e') = expr vars e in
      let (ot1, oe1') = oexpr vars oe1 in
      let (ot2, oe2') = oexpr vars oe2 in

      let (args, rt) = match t with
        | Func(args, rt) when rt != Void -> (args, rt)
        | _ as x-> raise (Failure ("illegal expression of type " ^ string_of_typ x ^
                                    " with map()"))
      in

      let it1' = match ot1 with
        | Observable x -> x
        | _ -> raise (Failure ("second arguement of map must be an observable"))
      in let it2' = match ot2 with
        | Observable x -> x
        | _ -> raise (Failure ("third arguement of map must be an observable"))
      in let _ = match args with
        | [a;b] when a = it1' && b = it2' -> ()
        | _ -> raise (Failure ("map function type does not match"))
      in
      (Observable rt, SCombine((t, e'), (ot1, oe1'), (ot2, oe2')))
  in

  let check_obs_stmt vars = function
      Obs(t, e) -> SObs(t, e)
    | OExpr oe -> SOExpr(oexpr vars oe)
    | ODecl(lt, var, e) ->
      let (rt, e') = expr vars e in
      StringHash.add vars var lt;
      SODecl(lt, var, (rt, e'))
    | OODecl(lt, var, oe) ->
      let (rt, oe') = oexpr vars oe in
      StringHash.add vars var lt;
      SOODecl(lt, var, (rt, oe'))
    | OAssign(s, e) ->
      let (rt, e') = expr vars e in
      let lt = type_of_identifier vars s in
      SOAssign(lt, s, (rt, e'))
    | OOAssign(s, oe) ->
      let (ot, oe') = oexpr vars oe in
      let lt = type_of_identifier vars s in
      SOOAssign(lt, s, (ot, oe'))
    (*
    | OArr_Decl of typ * string * expr list
    | OStr_Decl of typ * string * expr list
    *)
    | Subscribe(e, oe) ->
      let (ft, e') = expr vars e in
      let (ot, oe') = oexpr vars oe in

      let (args, _) = match ft with
        | Func(args, rt) -> (args, rt)
        | _ as x-> raise (Failure ("illegal expression of type " ^ string_of_typ x ^
                                  " with map()"))
      in

      let it' = match ot with
        | Observable x -> x
        | _ -> raise (Failure ("second arguement of map must be an observable"))
      in let _ = match args with
        | [a] when a = it' -> ()
        | _ -> raise (Failure ("map function type does not match"))
      in
      SSubscribe("subscribe", (ft, e'), (ot, oe'))
    | Complete(oe) ->
      let (ot, oe') = oexpr vars oe in 
      let _ = match ot with
        | Observable x -> x
        | _ -> raise (Failure ("second arguement of map must be an observable"))
      in
      SComplete("complete", (ot, oe'))
    | _ -> raise (Failure ("Not Implemented 1000"))
  in


  let fdecl_to_assign_stmt vars func =

    let func_type = Func((List.map fst func.formals), func.typ) in
    StringHash.add vars func.fname func_type;

    let (func_type, rtype, sstmt) = check_func_decl vars (func.formals, func.body) in

    SDecl(func_type, func.fname, (func_type, SFuncExpr(func.formals, rtype, sstmt)))
  in

  let check_glob glob = match glob with
      Stmt stmt -> SStmt(check_stmt global_vars stmt)
    | Obs_Stmt obs_stmt -> SObs_Stmt(check_obs_stmt global_vars obs_stmt)
    | Fdecl func -> SStmt(fdecl_to_assign_stmt global_vars func)
    (* | Fdecl func -> add_func func ; SFdecl(check_func_decl global_vars func) *)


  in (List.map check_glob globs)

