/* Ocamlyacc parser for Seaflow */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRAKT RBRAKT COMMA PLUS MINUS TIMES DIVIDE ASSIGN ARROW DOT
%token EQ NEQ LT LEQ GT GEQ AND OR
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
%left DOT

%%

program:
  glob_body EOF { $1 }

glob_body:
   /* nothing */ { ([], [])               }
  | glob_body vdecl { (($2 :: fst $1), snd $1) }
  | glob_body fdecl { (fst $1, ($2 :: snd $1)) }
  | glob_body odecl { () }
  | glob_body stmt { () }
  | glob_body obs_stmt { () }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE func_body RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = List.rev $4;
	 body = List.rev $7 } }

odecl:
    typ OBS SEMI                              { ($1, $2) }
  | typ OBS ASSIGN expr SEMI                  { () }
  | typ OBS ASSIGN obs_expr SEMI              { () }
  | typ OBS ASSIGN LBRACE args_list RBRACE SEMI { () }
  | typ OBS ASSIGN LBRAKT args_list RBRAKT SEMI { () }


func_body:
    /* nothing */   { ([], []) }
  | func_body vdecl  { () }
  | func_body stmt   { () }

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

vdecl:
    typ ID SEMI                              { ($1, $2) }
  | typ ID ASSIGN expr SEMI                  { () }
  | typ ID ASSIGN LBRACE args_list RBRACE SEMI { () }
  | typ ID ASSIGN LBRAKT args_list RBRAKT SEMI { () }
  | STRUCT SID LBRACE sdecl_list RBRACE SEMI { () }


sdecl_list:
    typ ID SEMI                 { () }
  | sdecl_list typ ID SEMI            { () }


stmt:
    expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }


obs_stmt:
    obs_expr SEMI         { () }
  | OBS ASSIGN expr SEMI  { Assign($1, $3)         }
  | OBS ASSIGN obs_expr SEMI    { () }


expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1)            }
  | NULL             { () }
  | FLIT             { Fliteral($1)           }
  | CHLIT            { () }
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
  | LPAREN formals_opt RPAREN ARROW LBRACE func_body RBRACE { () }   /* anonymous function */

obs_expr:
  | obs_var              { () }
  | obs_expr PLUS   expr { Binop($1, Add,   $3)   }
  | obs_expr DIVIDE expr { Binop($1, Div,   $3)   }
  | obs_expr EQ     expr { Binop($1, Equal, $3)   }
  | obs_expr NEQ    expr { Binop($1, Neq,   $3)   }
  | obs_expr LT     expr { Binop($1, Less,  $3)   }
  | obs_expr LEQ    expr { Binop($1, Leq,   $3)   }
  | obs_expr GT     expr { Binop($1, Greater, $3) }
  | obs_expr GEQ    expr { Binop($1, Geq,   $3)   }
  | obs_expr AND    expr { Binop($1, And,   $3)   }
  | obs_expr OR     expr { Binop($1, Or,    $3)   }

  | expr PLUS   obs_expr { Binop($1, Add,   $3)   }
  | expr DIVIDE obs_expr { Binop($1, Div,   $3)   }
  | expr EQ     obs_expr { Binop($1, Equal, $3)   }
  | expr NEQ    obs_expr { Binop($1, Neq,   $3)   }
  | expr LT     obs_expr { Binop($1, Less,  $3)   }
  | expr LEQ    obs_expr { Binop($1, Leq,   $3)   }
  | expr GT     obs_expr { Binop($1, Greater, $3) }
  | expr GEQ    obs_expr { Binop($1, Geq,   $3)   }
  | expr AND    obs_expr { Binop($1, And,   $3)   }
  | expr OR     obs_expr { Binop($1, Or,    $3)   }

  | obs_expr PLUS   obs_expr { Binop($1, Add,   $3)   }
  | obs_expr DIVIDE obs_expr { Binop($1, Div,   $3)   }
  | obs_expr EQ     obs_expr { Binop($1, Equal, $3)   }
  | obs_expr NEQ    obs_expr { Binop($1, Neq,   $3)   }
  | obs_expr LT     obs_expr { Binop($1, Less,  $3)   }
  | obs_expr LEQ    obs_expr { Binop($1, Leq,   $3)   }
  | obs_expr GT     obs_expr { Binop($1, Greater, $3) }
  | obs_expr GEQ    obs_expr { Binop($1, Geq,   $3)   }
  | obs_expr AND    obs_expr { Binop($1, And,   $3)   }
  | obs_expr OR     obs_expr { Binop($1, Or,    $3)   }

  | MINUS obs_expr       { Unop(Neg, $2)      }
  | OBS LBRAKT expr RBRAKT { () }
  | LPAREN obs_expr RPAREN { $2                   }



id_var:
    ID               { () }
  |  id_var DOT ID   { () }


obs_var:
    OBS             { () }
  | obs_var DOT ID   { () }


args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
