(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

module StringHash = Hashtbl.Make(struct
  type t = string
  let equal x y = x = y
  let hash = Hashtbl.hash
end)

let global_vars = StringHash.create 10
let function_decls = StringHash.create 10

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)



let check (globs) =

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
  in



  (* let global_vars = StringMap.empty in *)

  let type_of_identifier s =
    try StringHash.find global_vars s
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in


  let check_assign lvaluet rvaluet err =
    if lvaluet = rvaluet then lvaluet else raise (Failure err)
  in

  let rec expr = function
      Literal  l -> (Int, SLiteral l)
    | Fliteral l -> (Float, SFliteral l)
    | Chliteral c -> (Char, SChliteral c)
    | Id s       -> (type_of_identifier s, SId s)
    | Binop(e1, op, e2) as e -> 
        let (t1, e1') = expr e1 
        and (t2, e2') = expr e2 in
        (* All binary operators require operands of the same type *)
        let same = t1 = t2 in
        (* Determine expression type based on operator and operand types *)
        let ty = match op with
          Add | Sub | Mult | Div when same && t1 = Int   -> Int
        | Add | Sub | Mult | Div when same && t1 = Float -> Float
        | Equal | Neq            when same               -> Int
        | Less | Leq | Greater | Geq
                   when same && (t1 = Int || t1 = Float) -> Int
        | And | Or when same && t1 = Int -> Int
        | _ -> raise (Failure ("illegal binary operator  ^
                                string_of_typ t1 ^  ^ string_of_op op ^  ^
                                string_of_typ t2 ^  in  ^ string_of_expr e"))
        in (ty, SBinop((t1, e1'), op, (t2, e2')))
    | Call(fname, args) as call -> 
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (Failure ("expecting ^ string_of_int param_length ^ 
                            arguments in  ^ string_of_expr call"))
        else let check_call (ft, _) e = 
          let (et, e') = expr e in 
          let err = "illegal argument found  ^ string_of_typ et ^
              expected  ^ string_of_typ ft ^  in  ^ string_of_expr e"
          in (check_assign ft et err, e')
        in 
        let args' = List.map2 check_call fd.formals args
        in (fd.typ, SCall(fname, args'))
  in


  let rec check_stmt = function
      Expr e -> SExpr (expr e)
    | Decl(lt, var, e) as ex -> 
      let (rt, e') = expr e in
      let err = "illegal assignment  ^ string_of_typ lt ^  =  ^ 
        string_of_typ rt ^  in  ^ string_of_expr ex" in
      StringHash.add global_vars var lt ;
      (*(check_assign lt rt err, *) SDecl(lt, var, (rt, e'))
    | Return e -> let (t, e') = expr e
      in SReturn (t, e') 
      (* if t = func.typ then SReturn (t, e') 
      else raise (Failure ("return gives  ^ string_of_typ t ^  expected  ^
                            string_of_typ func.typ ^  in  ^ string_of_expr e")) *)
    | Block sl -> 
      let rec check_stmt_list = function
          [Return _ as s] -> [check_stmt s]
        | Return _ :: _   -> raise (Failure "nothing may follow a return")
        | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
        | s :: ss         -> check_stmt s :: check_stmt_list ss
        | []              -> []
      in SBlock(check_stmt_list sl)
  in


  let check_obs_stmt = function
      Obs e -> SObs e
    | _ -> raise (Failure ("Not Implemented 1000"))
  in


  let check_func_decl func = { styp = func.typ;
      sfname = func.fname;
      sformals = func.formals;
      (* slocals  = func.locals; *)
      sbody = match check_stmt (Block func.body) with
          SBlock(sl) -> sl
        | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
  in

  

  let check_glob glob = match glob with
      Stmt stmt -> SStmt(check_stmt stmt)
    | Obs_Stmt obs_stmt -> SObs_Stmt(check_obs_stmt obs_stmt)
    | Fdecl func -> SFdecl(check_func_decl func)


  in (List.map check_glob globs)

