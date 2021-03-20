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
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Call of string * expr list
  | Ref of expr * string list
  | Arr_Ref of string * expr
  | If of expr * expr * expr
  | Anon of bind list * stmt list
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
    (* locals : bind list; *)
    body : stmt list;
  }


type obs_stmt = 
    Obs of string 
  | Expr of expr
  | Decl of typ * string * expr
  | Assign of string * expr (* type?? *)
  | Arr_Decl of typ * string * expr list
  | Str_Decl of typ * string * expr list
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

(* 
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
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Void -> "void"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) *)
