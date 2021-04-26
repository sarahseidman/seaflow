(* Ocamllex scanner for Seaflow *)

{ open Seaflowparse }

let digit = ['0' - '9']
let char = ['a'-'z'] | ['A'-'Z'] | ['0'-'9'] | ' ' | '!' | ['#'-'~']
let charlit = "\'"char"\'"
let strlit = "\""char*"\""
let digits = digit+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRAKT }
| ']'      { RBRAKT }
| "->"     { ARROW }
| '.'      { DOT }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "if"     { IF }
| "else"   { ELSE }
| "return" { RETURN }
| "int"    { INT }
| "float"  { FLOAT }
| "char"   { CHAR }
| "void"   { VOID }
| "struct" { STRUCT }
| "map"       { MAP }
| "combine"   { COMBINE }
| "subscribe" { SUBSCRIBE }
| "complete"  { COMPLETE }
| charlit as lxm { CHLIT(lxm.[1])}
| strlit as lxm { STRLIT (String.escaped lxm)}
| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLIT(lxm) }
| ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']*             as lxm { ID(lxm) }
| ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*             as lxm { SID(lxm) }
| '$'['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']*  as lxm { OBS(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
