(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SLiteral of int
  | SBliteral of bool
  | SFliteral of string
  | SChliteral of char
  | SAliteral of typ * sexpr list
  | SId of string
  | SSid of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SCall of sexpr * sexpr list
  | SBCall of string * sexpr list (* Built-in function call *)
  | SRef of string * string * string
  | SArr_Ref of sexpr * sexpr
  | SFuncExpr of bind list * typ * sstmt list
  | SSliteral of sexpr list
  | SLen of sexpr
  | SNoexpr
  | SNull

and

sstmt =
    SBlock of sstmt list
  (* | Obs of string *)
  | SExpr of sexpr
  | SIf of typ * string * sexpr * sexpr * sexpr
  | SReturn of sexpr
  | SPrint of sexpr
  | SDecl of typ * string * sexpr
  | SStr_Def of string * bind list

type soexpr = typ * sox
and sox =
    SOId of string
  | SOBinop1 of soexpr * op * sexpr   (* For map operation *)
  | SOBinop2 of sexpr * op * soexpr
  | SOBinop3 of soexpr * op * soexpr  (* For combine operation *)
  | SOUnop of uop * soexpr
  | SMap of sexpr * soexpr
  | SCombine of sexpr * soexpr * soexpr

type sobs_stmt = 
    SObs of typ * string 
  | SOExpr of soexpr
  | SODecl of typ * string * sexpr
  | SOODecl of typ * string * soexpr
  | SOAssign of typ * string * sexpr
  | SOOAssign of typ * string * soexpr
  | SOArr_Decl of typ * string * sexpr list
  | SOStr_Decl of typ * string * sexpr list
  | SSubscribe of string * sexpr * soexpr
  | SComplete of string * soexpr

type sglob = 
    SStmt of sstmt
  | SObs_Stmt of sobs_stmt

type sprogram = sglob list

(* Pretty-printing functions *)

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SLiteral(l) -> string_of_int l
  | SBliteral(b) -> if b then "TRUE" else "FALSE"
  | SFliteral(l) -> l
  | SChliteral(l) -> "'" ^ String.make 1 l ^ "'"
  | SAliteral(_, l) -> "[" ^ String.concat "," (List.map string_of_sexpr l) ^ "]"
  | SId(s) -> "(id: " ^ s ^ ")"
  | SSid(s) -> "(struct: " ^ s ^ ")"
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SCall(ef, el) ->
      string_of_sexpr ef ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SBCall(s, el) -> (* Built-in function call *)
      s ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SArr_Ref(e1, e2) -> string_of_sexpr e1 ^ "[" ^ string_of_sexpr e2 ^ "]"
  | SRef(s1, _, s2) -> s1 ^ "." ^ s2
  | SFuncExpr(bind_list, _, stmt_list) -> "(" ^ 
      String.concat ", " (List.map string_of_bind bind_list) ^ ") -> {" ^
      String.concat "" (List.map string_of_sstmt stmt_list) ^ "}"
  | SSliteral(e_list) -> "(sliteral: { " ^ String.concat ", " (List.map string_of_sexpr e_list) ^ " })"
  | SLen(e) -> string_of_sexpr e ^ ".length"
  | SNull -> "null"
  | SNoexpr -> ""
        ) ^ ")"	

and

string_of_sstmt = function
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(sexpr) -> string_of_sexpr sexpr ^ ";\n";
  | SReturn(sexpr) -> "return " ^ string_of_sexpr sexpr ^ ";\n";
  | SPrint(expr) -> "print(" ^ string_of_sexpr expr ^ ");\n"
  | SDecl(t, s, expr) -> string_of_typ t ^ " " ^ s ^ " = "^ string_of_sexpr expr ^ ";\n"
  | SStr_Def(s, bind_list) -> "struct " ^ s ^ " { " ^ 
      String.concat "\n" (List.map string_of_bind bind_list) ^ "\n};\n"
  | SIf(t, s, e1, e2, e3) -> string_of_typ t ^ " " ^ s ^ " = if(" ^ string_of_sexpr e1 ^ ") "
    ^ string_of_sexpr e2 ^ " else " ^ string_of_sexpr e3


let rec string_of_soexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SOId(s) -> s
  | SOBinop1(e1, o, e2) ->
    string_of_soexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SOBinop2(e1, o, e2) ->
    string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_soexpr e2
  | SOBinop3(e1, o, e2) ->
    string_of_soexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_soexpr e2
  | SOUnop(o, e) -> string_of_uop o ^ string_of_soexpr e
  | SMap(e, oe) -> "map(" ^ string_of_sexpr e ^ ", " ^ string_of_soexpr oe ^ ")"
  | SCombine(e, oe1, oe2) -> "combine(" ^ string_of_sexpr e ^ ", " ^
      string_of_soexpr oe1 ^ ", " ^ string_of_soexpr oe2 ^ ")"

  ) ^ ")"

let string_of_sobs_stmt = function
    SObs(t, s) -> string_of_typ t ^ " " ^ s ^ ";\n"
  | SOExpr(e) -> string_of_soexpr e ^ ";\n"
  | SODecl(t, s, e) -> string_of_typ t ^ " " ^ s ^ " = " ^ string_of_sexpr e ^ ";\n"
  | SOODecl(t, s, e) -> string_of_typ t ^ " " ^ s ^ " = " ^ string_of_soexpr e ^ ";\n"
  | SOAssign(_, s, e) -> s ^ " = " ^ string_of_sexpr e ^ ";\n"
  | SOOAssign(_, s, e) -> s ^ " = " ^ string_of_soexpr e ^ ";\n"
  | SOArr_Decl(t, s, expr_list) -> string_of_typ t ^ " " ^ s ^ " = [" ^
      String.concat ", " (List.map string_of_sexpr expr_list) ^ "];\n"
  | SOStr_Decl(t, s, expr_list) -> string_of_typ t ^ " " ^ s ^ " = {" ^
      String.concat ", " (List.map string_of_sexpr expr_list) ^ "};\n" 
  | SSubscribe(s, e, oe) ->
      s ^ "(" ^ string_of_sexpr e ^ ", " ^ string_of_soexpr oe ^ ");\n"
  | SComplete(s, oe) ->
      s ^ "(" ^ string_of_soexpr oe ^ ");\n"

let sstr_of_glob = function
   SStmt(stmt) -> string_of_sstmt stmt
  | SObs_Stmt(obs_stmt) -> string_of_sobs_stmt obs_stmt

let sstring_of_program (globs) = 
  String.concat "" (List.map sstr_of_glob globs) ^ "\n"