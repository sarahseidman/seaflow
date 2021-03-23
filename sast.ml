(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

(* type sexpr = typ * sx
and sx =
    SLiteral of int
  | SFliteral of string
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  | SIf of sexpr * sexpr * sexpr
  | SNoexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr

type sobs_stmt = 
    

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    (* slocals : bind list; *)
    sbody : sstmt list;
  }

type sprogram = bind list * sfunc_decl list *)



type sexpr = typ * sx
and sx =
    SLiteral of int
  | SFliteral of string
  | SChliteral of char
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SCall of string * sexpr list
  | SRef of sexpr * string list
  | SArr_Ref of string * sexpr
  | SIf of sexpr * sexpr * sexpr
  | SAnon of bind list * sstmt list
  | SNoexpr
  | SVoid

and

sstmt =
    SBlock of sstmt list
  (* | Obs of string *)
  | SExpr of sexpr
  | SReturn of sexpr
  | SPrint of sexpr
  | SDecl of typ * string * sexpr
  | SArr_Decl of typ * string * sexpr list
  | SStr_Decl of typ * string * sexpr list
  | SStr_Def of string * bind list


type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    (* locals : bind list; *)
    sbody : sstmt list;
  }


type sobs_stmt = 
    SObs of string 
  | SOExpr of sexpr
  | SODecl of typ * string * sexpr
  | SOAssign of string * sexpr (* type?? *)
  | SOArr_Decl of typ * string * sexpr list
  | SOStr_Decl of typ * string * sexpr list
  (* glob_line:
  vdec { Vdecl($1) }
| fdecl { Fdecl($1) }
| odecl { Odecl($1) }
| stmt { Stmt($1) }
| obs_stmt { Obs_Stmt($1) } *)

(* type glob
  | Vdecl of  *)


type sglob = 
    SStmt of sstmt
  | SFdecl of sfunc_decl
  | SObs_Stmt of sobs_stmt

type sprogram = sglob list

(* Pretty-printing functions *)

(* let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SLiteral(l) -> string_of_int l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SFliteral(l) -> l
  | SId(s) -> s
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SNoexpr -> ""
				  ) ^ ")"				     

let rec string_of_sstmt = function
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(sexpr) -> string_of_sexpr sexpr ^ ";\n";
  | SReturn(sexpr) -> "return " ^ string_of_sexpr sexpr ^ ";\n";
  | SIf(e, s, SBlock([])) ->
      "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s

let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs) *)
