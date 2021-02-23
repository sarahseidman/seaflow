/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRAKT RBRAKT COMMA PLUS MINUS TIMES DIVIDE ASSIGN ARROW
%token EQ NEQ LT LEQ GT GEQ AND OR DOT
%token RETURN IF ELSE INT FLOAT VOID CHAR STRUCT NULL
%token <int> LITERAL
%token <string> ID FLIT OBS SID
%token <char> CHLIT
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { ([], [])               }
  | decls vdecl { (($2 :: fst $1), snd $1) }
  | decls fdecl { (fst $1, ($2 :: snd $1)) }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = List.rev $4;
	 body = List.rev $7 } }

list:
    /* nothing */   { ([], []) }
  | list vdecl  { () }
  | list stmt   { () }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
    typ ID                   { [($1,$2)]     }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT     { Int   }
  | FLOAT   { Float }
  | VOID    { Void  }
  | CHAR    { () }
  | typ LBRAKT RBRAKT { () }  /* array */
  | STRUCT SID { () }          /* struct */
  | LPAREN typ_list RPAREN ARROW LPAREN typ RPAREN { () } /* for higher order function */

typ_list:
    typ                { () }
  | typ_list COMMA typ { () }

// vdecl_list:
//     /* nothing */    { [] }
//   | vdecl_list vdecl { $2 :: $1 }

vdecl:
    typ ID SEMI                              { ($1, $2) }
  | typ ID ASSIGN expr SEMI                  { () }
  | STRUCT SID LBRACE sdecl_list RBRACE SEMI { () }

sdecl_list:
    typ ID SEMI                 { () }
  | sdecl_list typ ID SEMI            { () }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | ID ASSIGN LBRACE expr_struct RBRACE SEMI { () }
  | ID ASSIGN expr SEMI   { Assign($1, $3)         }
  | OBS ASSIGN expr SEMI  { Assign($1, $3)         }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1)            }
  | NULL             { () }
  | FLIT             { Fliteral($1)           }
  | id_var           { () }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | MINUS expr       { Unop(Neg, $2)      }
  | ID LBRAKT expr RBRAKT { () }
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }
  | LPAREN expr RPAREN { $2                   }
  | IF LPAREN expr RPAREN expr ELSE expr    { If($3, $5, $7) }
  | LPAREN formals_opt RPAREN ARROW LBRACE list RBRACE { () }   /* annoymous function */



expr_struct:
    expr                        { () }
  | expr_struct COMMA expr      { () }


id_var:
    ID              { () }
  | OBS             { () }
  | id_var DOT ID   { () }


args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
