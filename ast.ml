(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg

type typ = Int | Float | Void | Char | Arr of typ | Struct of string | Func of typ list * typ

type bind = typ * string

type expr =
    Literal of int
  | Fliteral of string
  | Chliteral of char
  | Id of string
  | Sid of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Call of string * expr list
  | Ref of expr * string
  | Arr_Ref of string * expr
  | If of expr * expr * expr
  | FuncExpr of bind list * stmt list
  | Noexpr
  | Void

and

stmt =
    Block of stmt list
  (* | Obs of string *)
  | Expr of expr
  | Return of expr
  | Print of expr
  | Decl of typ * string * expr
  | Arr_Decl of typ * string * expr list
  | Str_Decl of typ * string * expr list
  | Str_Def of string * bind list


type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : stmt list;
  }


type obs_stmt = 
    Obs of string 
  | OExpr of expr
  | ODecl of typ * string * expr
  | OAssign of string * expr
  | OArr_Decl of typ * string * expr list
  | OStr_Decl of typ * string * expr list
  (* glob_line:
  vdec { Vdecl($1) }
| fdecl { Fdecl($1) }
| odecl { Odecl($1) }
| stmt { Stmt($1) }
| obs_stmt { Obs_Stmt($1) } *)

(* type glob
  | Vdecl of  *)


type glob = 
    Stmt of stmt
  | Fdecl of func_decl
  | Obs_Stmt of obs_stmt

type program = glob list


(* Pretty-printing functions *)

let rec string_of_typ = function
    Int -> "int"
  | Float -> "float"
  | Void -> "void"
  | Char -> "char"
  | Arr(typ) -> string_of_typ typ ^ "[]"
  | Struct(str) -> "struct " ^ str
  | Func(typ_list, typ) -> "(" ^
      String.concat ", " (List.map string_of_typ typ_list)
      ^ ") -> (" ^ string_of_typ typ ^ ")"

let string_of_bind (t, id) = string_of_typ t ^ " " ^ id ^ ";"
let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Fliteral(l) -> l
  | Chliteral(l) -> "'" ^ String.make 1 l ^ "'"
  | Id(s) -> s
  | Sid(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Ref(e, s) -> string_of_expr e ^ "." ^ s
  | Arr_Ref(s, e) -> s ^ "[" ^ string_of_expr e ^ "]"
  | If(e1, e2, e3) -> "if(" ^ string_of_expr e1 ^ ") " ^ string_of_expr e2 ^ " else "
      ^ string_of_expr e3
  | FuncExpr(bind_list, stmt_list) -> "(" ^ 
      String.concat ", " (List.map string_of_bind bind_list) ^ ") -> {" ^
      String.concat "" (List.map string_of_stmt stmt_list) ^ "}"
  | Noexpr -> ""
  | Void -> ""

and

string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | Print(expr) -> "print(" ^ string_of_expr expr ^ ");\n"
  | Decl(t, s, expr) -> string_of_typ t ^ " " ^ s ^ " = "^ string_of_expr expr ^ ";\n"
  | Arr_Decl(t, s, expr_list) -> string_of_typ t ^ " " ^ s ^ " = [" ^ 
      String.concat ", " (List.map string_of_expr expr_list) ^ "];\n"
  | Str_Decl(t, s, expr_list) -> string_of_typ t ^ " " ^ s ^ " = {" ^
      String.concat ", " (List.map string_of_expr expr_list) ^ "};\n"
  | Str_Def(s, bind_list) -> "struct " ^ s ^ " { " ^ 
      String.concat "\n" (List.map string_of_bind bind_list) ^ "\n};\n"

let string_of_obs_stmt = function
    Obs(s) -> "type? " ^ s ^ ";\n"
  | OExpr(e) -> string_of_expr e ^ ";\n"
  | ODecl(t, s, e) -> string_of_typ t ^ " " ^ s ^ " = " ^ string_of_expr e ^ ";\n"
  | OAssign(s, e) -> s ^ " = " ^ string_of_expr e ^ ";\n"
  | OArr_Decl(t, s, expr_list) -> string_of_typ t ^ " " ^ s ^ " = [" ^
      String.concat ", " (List.map string_of_expr expr_list) ^ "];\n"
  | OStr_Decl(t, s, expr_list) -> string_of_typ t ^ " " ^ s ^ " = {" ^
      String.concat ", " (List.map string_of_expr expr_list) ^ "};\n" 


let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let str_of_glob = function
  Stmt(stmt) -> string_of_stmt stmt
  | Fdecl(func_decl) -> string_of_fdecl func_decl
  | Obs_Stmt(obs_stmt) -> string_of_obs_stmt obs_stmt

let string_of_program (globs) = 
  String.concat "" (List.map str_of_glob globs) ^ "\n"