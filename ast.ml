(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg

type typ = Int | Float | Void | Char | Arr of typ | Struct of string | Sbody of typ list | Func of typ list * typ
  | Observable of typ | Bool

(* type otyp = Observable of typ *)


type bind = typ * string

type expr =
    Literal of int
  | Bliteral of bool
  | Fliteral of string
  | Chliteral of char
  | Strliteral of string
  | Aliteral of expr list
  | Id of string
  | Sid of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Call of expr * expr list
  | Ref of expr * string
  | Arr_Ref of expr * expr
  | FuncExpr of bind list * stmt list
  | Sliteral of expr list
  | Noexpr
  | Null

and stmt =
    Block of stmt list
  | Expr of expr
  | If of typ * string * expr * expr * expr
  | Return of expr
  | Print of expr
  | Decl of typ * string * expr
  | Str_Def of string * bind list


type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : stmt list;
  }


type oexpr =
  | OId of string
  | OBinop1 of oexpr * op * expr   (* For map operation *)
  | OBinop2 of expr * op * oexpr
  | OBinop3 of oexpr * op * oexpr  (* For combine operation *)
  | OUnop of uop * oexpr 
  | Map of expr * oexpr
  | Combine of expr * oexpr * oexpr


type obs_stmt = 
    Obs of typ * string
  | OExpr of oexpr
  | ODecl of typ * string * expr
  | OODecl of typ * string * oexpr
  | OAssign of string * expr
  | OOAssign of string * oexpr
  | OArr_Decl of typ * string * expr list
  | OStr_Decl of typ * string * expr list
  | Subscribe of expr * oexpr
  | Complete of oexpr

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
  | Sbody(tlist) -> "{ " ^ String.concat ", " (List.map string_of_typ tlist) ^ " }"
  | Func(typ_list, typ) -> "(" ^
      String.concat ", " (List.map string_of_typ typ_list)
      ^ ") -> (" ^ string_of_typ typ ^ ")"
  | Observable(typ) -> string_of_typ typ ^ "$"
  | Bool -> "bool"

let typ_of_arr = function
  | Arr(typ) -> typ
  | _ -> raise (Failure ("Not an array"))

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
  | Bliteral(b) -> if b then "TRUE" else "FALSE"
  | Fliteral(l) -> l
  | Chliteral(l) -> "'" ^ String.make 1 l ^ "'"
  | Aliteral(l) -> "[" ^ String.concat "," (List.map string_of_expr l) ^ "]"
  | Strliteral(s) -> "\"" ^ s ^ "\""
  | Id(s) -> s
  | Sid(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Call(ef, el) ->
    string_of_expr ef ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Ref(e, s) -> string_of_expr e ^ "." ^ s
  | Arr_Ref(e1, e2) -> string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]"
  | FuncExpr(bind_list, stmt_list) -> "(" ^ 
      String.concat ", " (List.map string_of_bind bind_list) ^ ") -> {" ^
      String.concat "" (List.map string_of_stmt stmt_list) ^ "}"
  | Sliteral(e_list) -> "{ " ^ String.concat ", " (List.map string_of_expr e_list) ^ " }"
  | Noexpr -> ""
  | Null -> ""

and

string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | Print(expr) -> "print(" ^ string_of_expr expr ^ ");\n"
  | Decl(t, s, expr) -> string_of_typ t ^ " " ^ s ^ " = "^ string_of_expr expr ^ ";\n"
  | Str_Def(s, bind_list) -> "struct " ^ s ^ " { " ^ 
      String.concat "\n" (List.map string_of_bind bind_list) ^ "\n};\n"
  | If(t, s, e1, e2, e3) -> string_of_typ t ^ " " ^ s ^ " = if(" ^ string_of_expr e1 ^ ") "
      ^ string_of_expr e2 ^ " else " ^ string_of_expr e3 


let rec string_of_oexpr = function
    OId(s) -> s
  | OBinop1(e1, o, e2) ->
    string_of_oexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | OBinop2(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_oexpr e2
  | OBinop3(e1, o, e2) ->
    string_of_oexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_oexpr e2
  | OUnop(o, e) -> string_of_uop o ^ string_of_oexpr e
  | Map(e, oe) -> "map(" ^ string_of_expr e ^ ", " ^ string_of_oexpr oe ^ ")"
  | Combine(e, oe1, oe2) -> "combine(" ^ string_of_expr e ^ ", " ^
      string_of_oexpr oe1 ^ ", " ^ string_of_oexpr oe2 ^ ")"


let string_of_obs_stmt = function
    Obs(t, s) -> string_of_typ t ^ " " ^ s ^ ";\n"
  | OExpr(e) -> string_of_oexpr e ^ ";\n"
  | ODecl(t, s, e) -> string_of_typ t ^ " " ^ s ^ " = " ^ string_of_expr e ^ ";\n"
  | OODecl(t, s, e) -> string_of_typ t ^ " " ^ s ^ " = " ^ string_of_oexpr e ^ ";\n"
  | OAssign(s, e) -> s ^ " = " ^ string_of_expr e ^ ";\n"
  | OOAssign(s, e) -> s ^ " = " ^ string_of_oexpr e ^ ";\n"
  | OArr_Decl(t, s, expr_list) -> string_of_typ t ^ " " ^ s ^ " = [" ^
      String.concat ", " (List.map string_of_expr expr_list) ^ "];\n"
  | OStr_Decl(t, s, expr_list) -> string_of_typ t ^ " " ^ s ^ " = {" ^
      String.concat ", " (List.map string_of_expr expr_list) ^ "};\n"
  | Subscribe(e, oe) ->
      "subscribe(" ^ string_of_expr e ^ ", " ^ string_of_oexpr oe ^ ");\n"
  | Complete(oe) ->
      "complete(" ^ string_of_oexpr oe ^ ");\n"


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