(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SLiteral of int
  | SFliteral of string
  | SChliteral of char
  | SId of string
  | SSid of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SCall of sexpr * sexpr list
  | SBCall of string * sexpr list (* Built-in function call *)
  | SRef of string * string * string
  | SArr_Ref of string * sexpr
  | SIf of sexpr * sexpr * sexpr
  | SFuncExpr of bind list * typ * sstmt list
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
    sbody : sstmt list;
  }


type sobs_stmt = 
    SObs of string 
  | SOExpr of sexpr
  | SODecl of typ * string * sexpr
  | SOAssign of string * sexpr
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

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SLiteral(l) -> string_of_int l
  | SFliteral(l) -> l
  | SChliteral(l) -> "'" ^ String.make 1 l ^ "'"
  | SId(s) -> "(id: " ^ s ^ ")"
  | SSid(s) -> "(struct: " ^ s ^ ")"
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SCall(ef, el) ->
      string_of_sexpr ef ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SBCall(s, el) -> (* Built-in function call *)
      s ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SArr_Ref(s, e) -> s ^ "[" ^ string_of_sexpr e ^ "]"
  (* | SRef(e, _, s) -> string_of_sexpr e ^ "." ^ s *)
  | SRef(s1, _, s2) -> s1 ^ "." ^ s2
  | SIf(e1, e2, e3) -> "if(" ^ string_of_sexpr e1 ^ ") " ^ string_of_sexpr e2 ^ " else "
      ^ string_of_sexpr e3
  | SFuncExpr(bind_list, t, stmt_list) -> "(" ^ 
      String.concat ", " (List.map string_of_bind bind_list) ^ ") -> {" ^
      String.concat "" (List.map string_of_sstmt stmt_list) ^ "}"
  | SVoid -> ""
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
  | SArr_Decl(t, s, expr_list) -> string_of_typ t ^ " " ^ s ^ " = [" ^ 
      String.concat ", " (List.map string_of_sexpr expr_list) ^ "];\n"
  | SStr_Decl(t, s, expr_list) -> string_of_typ t ^ " " ^ s ^ " = {" ^
      String.concat ", " (List.map string_of_sexpr expr_list) ^ "};\n"
  | SStr_Def(s, bind_list) -> "struct " ^ s ^ " { " ^ 
      String.concat "\n" (List.map string_of_bind bind_list) ^ "\n};\n"


let string_of_sobs_stmt = function
    SObs(s) -> "type? " ^ s ^ ";\n"
  | SOExpr(e) -> string_of_sexpr e ^ ";\n"
  | SODecl(t, s, e) -> string_of_typ t ^ " " ^ s ^ " = " ^ string_of_sexpr e ^ ";\n"
  | SOAssign(s, e) -> s ^ " = " ^ string_of_sexpr e ^ ";\n"
  | SOArr_Decl(t, s, expr_list) -> string_of_typ t ^ " " ^ s ^ " = [" ^
      String.concat ", " (List.map string_of_sexpr expr_list) ^ "];\n"
  | SOStr_Decl(t, s, expr_list) -> string_of_typ t ^ " " ^ s ^ " = {" ^
      String.concat ", " (List.map string_of_sexpr expr_list) ^ "};\n" 
      
let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let sstr_of_glob = function
   SStmt(stmt) -> string_of_sstmt stmt
  | SFdecl(func_decl) -> string_of_sfdecl func_decl
  | SObs_Stmt(obs_stmt) -> string_of_sobs_stmt obs_stmt

let sstring_of_program (globs) = 
  String.concat "" (List.map sstr_of_glob globs) ^ "\n"