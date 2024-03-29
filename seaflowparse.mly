/* Ocamlyacc parser for Seaflow */

%{
open Ast
%}

%token MAP COMBINE SUBSCRIBE COMPLETE
%token SEMI LPAREN RPAREN LBRACE RBRACE LBRAKT RBRAKT COMMA PLUS MINUS TIMES DIVIDE ASSIGN ARROW DOT
%token EQ NEQ LT LEQ GT GEQ AND OR
%token RETURN IF ELSE INT FLOAT VOID CHAR STRUCT NULL
%token <int> LITERAL
%token <string> ID FLIT OBS SID STRLIT
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
%left LBRAKT
%left DOT

%%
  
program:
  glob_lines EOF { List.rev $1 }

glob_lines:
     { [] }
  | glob_lines glob_line { $2 :: $1 }

glob_line:
  | fdecl    { Fdecl($1) }
  | stmt     { Stmt($1) }
  | obs_stmt { Obs_Stmt($1) }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE func_body RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = List.rev $4;
	 body = List.rev $7 } }

func_body:
    /* nothing */    { []       }
  | func_body stmt   { $2 :: $1 }

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
  | CHAR    { Char }
  | typ LBRAKT RBRAKT { Arr($1) }             /* array */
  | STRUCT SID        { Struct($2) }          /* struct */
  | LPAREN typ_list RPAREN ARROW LPAREN typ RPAREN { Func(List.rev $2, $6) } /* for higher order function */

typ_list:
    typ                { [$1] }
  | typ_list COMMA typ { $3 :: $1 }

sdecl_list:
    typ ID SEMI                 { [($1,$2)] }
  | sdecl_list typ ID SEMI            { ($2,$3) :: $1 }

stmt:
    expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }
  | typ ID ASSIGN expr SEMI                  { Decl($1, $2, $4) }
  | STRUCT SID LBRACE sdecl_list RBRACE SEMI { Str_Def($2, List.rev $4) }
  | typ ID ASSIGN IF LPAREN expr RPAREN expr ELSE expr SEMI   { If($1, $2, $6, $8, $10) }


obs_stmt:
    obs_expr SEMI                             { OExpr $1 }
  | OBS ASSIGN expr SEMI                      { OAssign($1, $3)         }
  | OBS ASSIGN obs_expr SEMI                  { OOAssign($1, $3) }
  | typ OBS SEMI                              { Obs(Observable($1), $2) }
  | typ OBS ASSIGN expr SEMI                  { ODecl(Observable($1), $2, $4) }
  | typ OBS ASSIGN obs_expr SEMI              { OODecl(Observable($1), $2, $4) }

  | SUBSCRIBE LPAREN expr COMMA obs_expr RPAREN SEMI { Subscribe($3, $5) }
  | COMPLETE  LPAREN obs_expr RPAREN SEMI            { Complete($3) }


expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1)            }
  | NULL             { Null                   }
  | FLIT             { Fliteral($1)           }
  | CHLIT            { Chliteral($1)          }
  | STRLIT           { Strliteral($1)         }
  | ID               { Id($1)                 }
  | SID              { Sid($1)                }
  | expr DOT ID      { Ref($1, $3)            }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr MINUS  expr { Binop($1, Sub, $3)     }
  | expr TIMES  expr { Binop($1, Mult, $3)    }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | LBRAKT args_list RBRAKT { Aliteral(List.rev $2)         }

  | MINUS expr       { Unop(Neg, $2)          }
  | LBRACE args_list RBRACE                   { Sliteral(List.rev $2)}
  | expr LBRAKT expr RBRAKT                     { Arr_Ref($1, $3)  }
  | ID LPAREN args_opt RPAREN                 { Call(Id($1), $3) }
  | LPAREN expr RPAREN LPAREN args_opt RPAREN { Call($2, $5)     }
  | LPAREN expr RPAREN { $2                   }
  | LPAREN formals_opt RPAREN ARROW LBRACE func_body RBRACE 
        { FuncExpr(List.rev $2, List.rev $6) }   /* anonymous function */

obs_expr:
  | obs_var              { OId($1) }
  | obs_expr PLUS   expr { OBinop1($1, Add,   $3)   }
  | obs_expr DIVIDE expr { OBinop1($1, Div,   $3)   }
  | obs_expr MINUS  expr { OBinop1($1, Sub, $3)     }
  | obs_expr TIMES  expr { OBinop1($1, Mult, $3)    }
  | obs_expr EQ     expr { OBinop1($1, Equal, $3)   }
  | obs_expr NEQ    expr { OBinop1($1, Neq,   $3)   }
  | obs_expr LT     expr { OBinop1($1, Less,  $3)   }
  | obs_expr LEQ    expr { OBinop1($1, Leq,   $3)   }
  | obs_expr GT     expr { OBinop1($1, Greater, $3) }
  | obs_expr GEQ    expr { OBinop1($1, Geq,   $3)   }
  | obs_expr AND    expr { OBinop1($1, And,   $3)   }
  | obs_expr OR     expr { OBinop1($1, Or,    $3)   }

  | expr PLUS   obs_expr { OBinop2($1, Add,   $3)   }
  | expr DIVIDE obs_expr { OBinop2($1, Div,   $3)   }
  | expr MINUS  obs_expr { OBinop2($1, Sub, $3)     }
  | expr TIMES  obs_expr { OBinop2($1, Mult, $3)    }
  | expr EQ     obs_expr { OBinop2($1, Equal, $3)   }
  | expr NEQ    obs_expr { OBinop2($1, Neq,   $3)   }
  | expr LT     obs_expr { OBinop2($1, Less,  $3)   }
  | expr LEQ    obs_expr { OBinop2($1, Leq,   $3)   }
  | expr GT     obs_expr { OBinop2($1, Greater, $3) }
  | expr GEQ    obs_expr { OBinop2($1, Geq,   $3)   }
  | expr AND    obs_expr { OBinop2($1, And,   $3)   }
  | expr OR     obs_expr { OBinop2($1, Or,    $3)   }

  | obs_expr PLUS   obs_expr { OBinop3($1, Add,   $3)   }
  | obs_expr DIVIDE obs_expr { OBinop3($1, Div,   $3)   }
  | obs_expr MINUS  obs_expr { OBinop3($1, Sub, $3)     }
  | obs_expr TIMES  obs_expr { OBinop3($1, Mult, $3)    }
  | obs_expr EQ     obs_expr { OBinop3($1, Equal, $3)   }
  | obs_expr NEQ    obs_expr { OBinop3($1, Neq,   $3)   }
  | obs_expr LT     obs_expr { OBinop3($1, Less,  $3)   }
  | obs_expr LEQ    obs_expr { OBinop3($1, Leq,   $3)   }
  | obs_expr GT     obs_expr { OBinop3($1, Greater, $3) }
  | obs_expr GEQ    obs_expr { OBinop3($1, Geq,   $3)   }
  | obs_expr AND    obs_expr { OBinop3($1, And,   $3)   }
  | obs_expr OR     obs_expr { OBinop3($1, Or,    $3)   }

  | MAP LPAREN expr COMMA obs_expr RPAREN { Map($3, $5) }
  | COMBINE LPAREN expr COMMA obs_expr COMMA obs_expr RPAREN { Combine($3, $5, $7) }


  | MINUS obs_expr         { OUnop(Neg, $2)       }
  | LPAREN obs_expr RPAREN { $2                   }

obs_var:
    OBS             { $1 }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
