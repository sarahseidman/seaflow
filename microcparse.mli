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

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
