type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRAKT
  | RBRAKT
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | RETURN
  | IF
  | ELSE
  | INT
  | FLOAT
  | VOID
  | CHAR
  | STRUCT
  | PRINT
  | NULL
  | LITERAL of (int)
  | ID of (string)
  | FLIT of (string)
  | OBS of (string)
  | SID of (string)
  | CHLIT of (char)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "microcparse.mly"
open Ast
# 47 "microcparse.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* LBRAKT *);
  263 (* RBRAKT *);
  264 (* COMMA *);
  265 (* PLUS *);
  266 (* MINUS *);
  267 (* TIMES *);
  268 (* DIVIDE *);
  269 (* ASSIGN *);
  270 (* NOT *);
  271 (* EQ *);
  272 (* NEQ *);
  273 (* LT *);
  274 (* LEQ *);
  275 (* GT *);
  276 (* GEQ *);
  277 (* AND *);
  278 (* OR *);
  279 (* RETURN *);
  280 (* IF *);
  281 (* ELSE *);
  282 (* INT *);
  283 (* FLOAT *);
  284 (* VOID *);
  285 (* CHAR *);
  286 (* STRUCT *);
  287 (* PRINT *);
  288 (* NULL *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  289 (* LITERAL *);
  290 (* ID *);
  291 (* FLIT *);
  292 (* OBS *);
  293 (* SID *);
  294 (* CHLIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\007\000\007\000\007\000\
\006\000\006\000\009\000\009\000\005\000\005\000\005\000\005\000\
\005\000\005\000\010\000\010\000\003\000\003\000\003\000\012\000\
\012\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\013\000\013\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\014\000\014\000\
\015\000\015\000\016\000\016\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\008\000\000\000\002\000\002\000\
\000\000\001\000\002\000\004\000\001\000\001\000\001\000\001\000\
\003\000\002\000\000\000\002\000\003\000\005\000\006\000\000\000\
\002\000\002\000\003\000\003\000\005\000\008\000\004\000\004\000\
\000\000\001\000\001\000\001\000\001\000\001\000\001\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\002\000\004\000\003\000\007\000\001\000\003\000\
\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\061\000\000\000\013\000\014\000\015\000\016\000\
\000\000\001\000\003\000\004\000\000\000\018\000\000\000\000\000\
\000\000\019\000\017\000\021\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\036\000\
\035\000\000\000\037\000\039\000\000\000\000\000\020\000\000\000\
\011\000\000\000\000\000\000\000\050\000\051\000\000\000\000\000\
\022\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\023\000\000\000\006\000\000\000\053\000\
\000\000\000\000\000\000\000\000\000\000\041\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\012\000\
\000\000\052\000\000\000\024\000\005\000\000\000\000\000\000\000\
\000\000\000\000\007\000\008\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\026\000\000\000\
\028\000\000\000\025\000\027\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\031\000\032\000\000\000\029\000\
\000\000\000\000\000\000\000\000\030\000\000\000"

let yydgoto = "\002\000\
\003\000\004\000\011\000\012\000\040\000\026\000\079\000\092\000\
\027\000\023\000\093\000\096\000\098\000\122\000\067\000\068\000"

let yysindex = "\012\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\013\255\000\000\000\000\000\000\004\255\000\000\017\255\007\255\
\003\255\000\000\000\000\000\000\131\255\074\255\094\255\237\254\
\006\255\045\255\048\255\074\255\074\255\074\255\075\255\000\000\
\000\000\087\255\000\000\000\000\066\000\093\255\000\000\011\255\
\000\000\096\255\131\255\196\000\000\000\000\000\074\255\074\255\
\000\000\074\255\074\255\074\255\074\255\074\255\074\255\074\255\
\074\255\074\255\074\255\000\000\010\255\000\000\012\255\000\000\
\216\000\008\001\098\255\095\255\106\255\000\000\047\001\047\001\
\042\255\042\255\042\255\042\255\035\001\022\001\039\255\000\000\
\074\255\000\000\074\255\000\000\000\000\074\255\018\255\117\255\
\079\255\113\255\000\000\000\000\088\000\250\000\008\001\081\255\
\008\001\127\255\005\255\074\255\074\255\074\255\000\000\074\255\
\000\000\097\255\000\000\000\000\118\255\236\000\110\000\132\000\
\008\001\101\255\126\255\136\255\000\000\000\000\074\255\000\000\
\008\001\052\255\137\255\074\255\000\000\008\001"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\144\255\000\000\000\000\000\000\
\000\000\000\000\148\255\000\000\000\000\000\000\000\000\000\000\
\000\000\124\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\150\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\016\255\000\000\151\255\147\255\000\000\017\000\040\000\
\170\255\193\255\216\255\239\255\245\255\046\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\169\255\000\000\000\000\
\154\000\176\000\000\000\000\000\000\000\000\000\019\255\000\000\
\173\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\069\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\082\255\000\000\000\000\000\000\000\000\088\255"

let yygindex = "\000\000\
\000\000\000\000\235\255\000\000\255\255\000\000\000\000\080\000\
\000\000\000\000\234\255\000\000\000\000\000\000\000\000\000\000"

let yytablesize = 579
let yytable = "\037\000\
\010\000\039\000\013\000\020\000\021\000\044\000\045\000\046\000\
\018\000\016\000\020\000\016\000\001\000\019\000\014\000\022\000\
\016\000\016\000\059\000\025\000\018\000\060\000\022\000\059\000\
\065\000\066\000\060\000\069\000\070\000\071\000\072\000\073\000\
\074\000\075\000\076\000\077\000\078\000\017\000\109\000\041\000\
\028\000\063\000\084\000\085\000\061\000\080\000\014\000\042\000\
\029\000\015\000\050\000\014\000\030\000\051\000\099\000\043\000\
\123\000\091\000\094\000\124\000\095\000\086\000\031\000\097\000\
\005\000\006\000\007\000\008\000\087\000\088\000\032\000\033\000\
\089\000\035\000\090\000\028\000\047\000\110\000\111\000\112\000\
\048\000\113\000\028\000\029\000\084\000\105\000\055\000\030\000\
\048\000\055\000\029\000\101\000\056\000\060\000\030\000\056\000\
\121\000\031\000\038\000\062\000\082\000\126\000\083\000\086\000\
\031\000\032\000\033\000\034\000\035\000\036\000\106\000\088\000\
\032\000\033\000\089\000\035\000\090\000\051\000\100\000\005\000\
\006\000\007\000\008\000\009\000\038\000\102\000\038\000\108\000\
\038\000\119\000\115\000\038\000\038\000\114\000\109\000\038\000\
\120\000\125\000\038\000\038\000\038\000\038\000\038\000\038\000\
\038\000\038\000\009\000\040\000\038\000\040\000\010\000\040\000\
\057\000\058\000\040\000\040\000\005\000\006\000\007\000\008\000\
\024\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
\040\000\033\000\044\000\040\000\044\000\034\000\044\000\107\000\
\000\000\044\000\000\000\000\000\000\000\000\000\000\000\000\000\
\044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
\000\000\045\000\044\000\045\000\000\000\045\000\000\000\000\000\
\045\000\000\000\000\000\000\000\000\000\000\000\000\000\045\000\
\045\000\045\000\045\000\045\000\045\000\045\000\045\000\000\000\
\046\000\045\000\046\000\000\000\046\000\000\000\000\000\046\000\
\000\000\000\000\000\000\000\000\000\000\000\000\046\000\046\000\
\046\000\046\000\046\000\046\000\046\000\046\000\000\000\047\000\
\046\000\047\000\000\000\047\000\000\000\048\000\047\000\048\000\
\000\000\048\000\000\000\000\000\048\000\047\000\047\000\047\000\
\047\000\047\000\047\000\047\000\047\000\000\000\000\000\047\000\
\000\000\048\000\048\000\000\000\000\000\048\000\000\000\000\000\
\000\000\042\000\000\000\042\000\000\000\042\000\000\000\000\000\
\042\000\000\000\005\000\006\000\007\000\008\000\009\000\042\000\
\042\000\000\000\000\000\000\000\000\000\042\000\042\000\000\000\
\043\000\042\000\043\000\000\000\043\000\000\000\049\000\043\000\
\049\000\000\000\049\000\000\000\000\000\049\000\043\000\043\000\
\000\000\000\000\000\000\000\000\043\000\043\000\000\000\000\000\
\043\000\000\000\049\000\049\000\000\000\054\000\049\000\054\000\
\000\000\054\000\050\000\000\000\054\000\051\000\000\000\000\000\
\052\000\053\000\054\000\055\000\056\000\057\000\058\000\059\000\
\103\000\000\000\000\000\000\000\000\000\054\000\000\000\000\000\
\050\000\000\000\000\000\051\000\000\000\000\000\052\000\053\000\
\054\000\055\000\056\000\057\000\058\000\059\000\117\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\050\000\000\000\
\000\000\051\000\000\000\000\000\052\000\053\000\054\000\055\000\
\056\000\057\000\058\000\059\000\118\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\050\000\000\000\000\000\051\000\
\000\000\000\000\052\000\053\000\054\000\055\000\056\000\057\000\
\058\000\059\000\038\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\038\000\000\000\000\000\038\000\000\000\000\000\
\038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
\039\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\039\000\000\000\000\000\039\000\000\000\000\000\039\000\039\000\
\039\000\039\000\039\000\039\000\039\000\039\000\064\000\000\000\
\000\000\000\000\000\000\000\000\050\000\000\000\000\000\051\000\
\000\000\000\000\052\000\053\000\054\000\055\000\056\000\057\000\
\058\000\059\000\081\000\000\000\000\000\000\000\000\000\000\000\
\050\000\000\000\000\000\051\000\000\000\000\000\052\000\053\000\
\054\000\055\000\056\000\057\000\058\000\059\000\116\000\000\000\
\000\000\000\000\000\000\000\000\050\000\000\000\000\000\051\000\
\000\000\000\000\052\000\053\000\054\000\055\000\056\000\057\000\
\058\000\059\000\050\000\000\000\000\000\051\000\000\000\000\000\
\052\000\053\000\054\000\055\000\056\000\057\000\058\000\059\000\
\050\000\000\000\104\000\051\000\000\000\000\000\052\000\053\000\
\054\000\055\000\056\000\057\000\058\000\059\000\050\000\000\000\
\000\000\051\000\000\000\000\000\052\000\053\000\054\000\055\000\
\056\000\057\000\058\000\050\000\000\000\000\000\051\000\000\000\
\000\000\052\000\053\000\054\000\055\000\056\000\057\000\050\000\
\000\000\000\000\051\000\000\000\000\000\000\000\000\000\054\000\
\055\000\056\000\057\000"

let yycheck = "\022\000\
\000\000\023\000\004\000\001\001\002\001\028\000\029\000\030\000\
\004\001\006\001\001\001\006\001\001\000\007\001\034\001\013\001\
\006\001\006\001\003\001\021\000\004\001\003\001\013\001\008\001\
\047\000\048\000\008\001\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\058\000\059\000\034\001\034\001\034\001\
\002\001\043\000\004\001\005\001\034\001\034\001\034\001\003\001\
\010\001\037\001\009\001\034\001\014\001\012\001\037\001\008\001\
\005\001\079\000\081\000\008\001\083\000\023\001\024\001\086\000\
\026\001\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\034\001\035\001\036\001\002\001\002\001\100\000\101\000\102\000\
\002\001\104\000\002\001\010\001\004\001\005\001\005\001\014\001\
\002\001\008\001\010\001\013\001\005\001\001\001\014\001\008\001\
\119\000\024\001\005\001\004\001\003\001\124\000\008\001\023\001\
\024\001\032\001\033\001\034\001\035\001\036\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\012\001\002\001\026\001\
\027\001\028\001\029\001\030\001\001\001\013\001\003\001\001\001\
\005\001\004\001\013\001\008\001\009\001\037\001\034\001\012\001\
\001\001\001\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\003\001\001\001\025\001\003\001\003\001\005\001\
\003\001\003\001\008\001\009\001\026\001\027\001\028\001\029\001\
\030\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\001\001\001\001\025\001\003\001\001\001\005\001\096\000\
\255\255\008\001\255\255\255\255\255\255\255\255\255\255\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\255\255\001\001\025\001\003\001\255\255\005\001\255\255\255\255\
\008\001\255\255\255\255\255\255\255\255\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\255\255\
\001\001\025\001\003\001\255\255\005\001\255\255\255\255\008\001\
\255\255\255\255\255\255\255\255\255\255\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\255\255\001\001\
\025\001\003\001\255\255\005\001\255\255\001\001\008\001\003\001\
\255\255\005\001\255\255\255\255\008\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\255\255\255\255\025\001\
\255\255\021\001\022\001\255\255\255\255\025\001\255\255\255\255\
\255\255\001\001\255\255\003\001\255\255\005\001\255\255\255\255\
\008\001\255\255\026\001\027\001\028\001\029\001\030\001\015\001\
\016\001\255\255\255\255\255\255\255\255\021\001\022\001\255\255\
\001\001\025\001\003\001\255\255\005\001\255\255\001\001\008\001\
\003\001\255\255\005\001\255\255\255\255\008\001\015\001\016\001\
\255\255\255\255\255\255\255\255\021\001\022\001\255\255\255\255\
\025\001\255\255\001\001\022\001\255\255\001\001\025\001\003\001\
\255\255\005\001\009\001\255\255\008\001\012\001\255\255\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\001\001\255\255\255\255\255\255\255\255\025\001\255\255\255\255\
\009\001\255\255\255\255\012\001\255\255\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\001\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\009\001\255\255\
\255\255\012\001\255\255\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\001\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\009\001\255\255\255\255\012\001\
\255\255\255\255\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\001\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\009\001\255\255\255\255\012\001\255\255\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\001\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\009\001\255\255\255\255\012\001\255\255\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\003\001\255\255\
\255\255\255\255\255\255\255\255\009\001\255\255\255\255\012\001\
\255\255\255\255\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\003\001\255\255\255\255\255\255\255\255\255\255\
\009\001\255\255\255\255\012\001\255\255\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\003\001\255\255\
\255\255\255\255\255\255\255\255\009\001\255\255\255\255\012\001\
\255\255\255\255\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\009\001\255\255\255\255\012\001\255\255\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\009\001\255\255\025\001\012\001\255\255\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\009\001\255\255\
\255\255\012\001\255\255\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\009\001\255\255\255\255\012\001\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\009\001\
\255\255\255\255\012\001\255\255\255\255\255\255\255\255\017\001\
\018\001\019\001\020\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LBRAKT\000\
  RBRAKT\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  NOT\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  AND\000\
  OR\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  INT\000\
  FLOAT\000\
  VOID\000\
  CHAR\000\
  STRUCT\000\
  PRINT\000\
  NULL\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  ID\000\
  FLIT\000\
  OBS\000\
  SID\000\
  CHLIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 32 "microcparse.mly"
            ( _1 )
# 378 "microcparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "microcparse.mly"
                 ( ([], [])               )
# 384 "microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 36 "microcparse.mly"
                ( ((_2 :: fst _1), snd _1) )
# 392 "microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 37 "microcparse.mly"
                ( (fst _1, (_2 :: snd _1)) )
# 400 "microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'list) in
    Obj.repr(
# 41 "microcparse.mly"
     ( { typ = _1;
	 fname = _2;
	 formals = List.rev _4;
	 body = List.rev _7 } )
# 413 "microcparse.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "microcparse.mly"
                    ( ([], []) )
# 419 "microcparse.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 48 "microcparse.mly"
                ( () )
# 427 "microcparse.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 49 "microcparse.mly"
                ( () )
# 435 "microcparse.ml"
               : 'list))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "microcparse.mly"
                  ( [] )
# 441 "microcparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 53 "microcparse.mly"
                  ( _1 )
# 448 "microcparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "microcparse.mly"
                             ( [(_1,_2)]     )
# 456 "microcparse.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "microcparse.mly"
                             ( (_3,_4) :: _1 )
# 465 "microcparse.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "microcparse.mly"
            ( Int   )
# 471 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "microcparse.mly"
            ( Float )
# 477 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "microcparse.mly"
            ( Void  )
# 483 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "microcparse.mly"
            ( () )
# 489 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    Obj.repr(
# 64 "microcparse.mly"
                      ( () )
# 496 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "microcparse.mly"
              ( () )
# 503 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "microcparse.mly"
                     ( [] )
# 509 "microcparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 69 "microcparse.mly"
                     ( _2 :: _1 )
# 517 "microcparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 72 "microcparse.mly"
                                            ( (_1, _2) )
# 525 "microcparse.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 73 "microcparse.mly"
                                            ( () )
# 534 "microcparse.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    Obj.repr(
# 74 "microcparse.mly"
                                             ( () )
# 542 "microcparse.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 77 "microcparse.mly"
                   ( [] )
# 548 "microcparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 78 "microcparse.mly"
                   ( _2 :: _1 )
# 556 "microcparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 81 "microcparse.mly"
                                            ( Expr _1               )
# 563 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 82 "microcparse.mly"
                                            ( Return _2             )
# 570 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 83 "microcparse.mly"
                                            ( Block(List.rev _2)    )
# 577 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 84 "microcparse.mly"
                                            ( () )
# 584 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'expr_struct) in
    Obj.repr(
# 85 "microcparse.mly"
                                                          ( () )
# 593 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 86 "microcparse.mly"
                          ( Assign(_1, _3)         )
# 601 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 87 "microcparse.mly"
                          ( Assign(_1, _3)         )
# 609 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "microcparse.mly"
                  ( Noexpr )
# 615 "microcparse.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "microcparse.mly"
                  ( _1 )
# 622 "microcparse.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 94 "microcparse.mly"
                     ( Literal(_1)            )
# 629 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "microcparse.mly"
                     ( () )
# 635 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 96 "microcparse.mly"
                     ( Fliteral(_1)           )
# 642 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 97 "microcparse.mly"
                     ( Id(_1)                 )
# 649 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 98 "microcparse.mly"
                     ( Obs(_1)                )
# 656 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "microcparse.mly"
                     ( Binop(_1, Add,   _3)   )
# 664 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "microcparse.mly"
                     ( Binop(_1, Div,   _3)   )
# 672 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "microcparse.mly"
                     ( Binop(_1, Equal, _3)   )
# 680 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "microcparse.mly"
                     ( Binop(_1, Neq,   _3)   )
# 688 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "microcparse.mly"
                     ( Binop(_1, Less,  _3)   )
# 696 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "microcparse.mly"
                     ( Binop(_1, Leq,   _3)   )
# 704 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "microcparse.mly"
                     ( Binop(_1, Greater, _3) )
# 712 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "microcparse.mly"
                     ( Binop(_1, Geq,   _3)   )
# 720 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "microcparse.mly"
                     ( Binop(_1, And,   _3)   )
# 728 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "microcparse.mly"
                     ( Binop(_1, Or,    _3)   )
# 736 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "microcparse.mly"
                         ( Unop(Neg, _2)      )
# 743 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "microcparse.mly"
                     ( Unop(Not, _2)          )
# 750 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 111 "microcparse.mly"
                              ( Call(_1, _3)  )
# 758 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 112 "microcparse.mly"
                       ( _2                   )
# 765 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "microcparse.mly"
                                            ( If(_3, _5, _7) )
# 774 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "microcparse.mly"
                                ( () )
# 781 "microcparse.ml"
               : 'expr_struct))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_struct) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "microcparse.mly"
                                ( () )
# 789 "microcparse.ml"
               : 'expr_struct))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "microcparse.mly"
                  ( [] )
# 795 "microcparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 125 "microcparse.mly"
               ( List.rev _1 )
# 802 "microcparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "microcparse.mly"
                            ( [_1] )
# 809 "microcparse.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "microcparse.mly"
                         ( _3 :: _1 )
# 817 "microcparse.ml"
               : 'args_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
