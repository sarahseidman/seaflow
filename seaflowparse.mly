/* Ocamlyacc parser for Seaflow */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRAKT RBRAKT COMMA PLUS MINUS TIMES DIVIDE ASSIGN ARROW DOT
%token EQ NEQ LT LEQ GT GEQ AND OR
%token RETURN IF ELSE INT FLOAT VOID CHAR STRUCT NULL LEN
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

// program:
//   glob_body EOF { List.rev $1 }
  
program:
  glob_lines EOF { List.rev $1 }

// glob_body:
//    /* nothing */ { [] }
//   | glob_body vdecl { $2 :: $1 }
//   | glob_body fdecl { $2 :: $1 }
//   | glob_body odecl { $2 :: $1 }
//   | glob_body stmt { $2 :: $1 }
//   | glob_body obs_stmt { $2 :: $1 }


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

// odecl:
//     typ OBS SEMI                              { Obs($2) }
//   | typ OBS ASSIGN expr SEMI                  { Assign($2, $4) }
//   | typ OBS ASSIGN obs_expr SEMI              { Assign($2, $4) }
//   | typ OBS ASSIGN LBRACE args_list RBRACE SEMI { Str_Assign($2, $5) }
//   | typ OBS ASSIGN LBRAKT args_list RBRAKT SEMI { Arr_Assign($2, $5) }


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

// vdecl:
//     // typ ID SEMI                              { Id($2) }
//     typ ID ASSIGN expr SEMI                  { Assign($1, $2, $4) }
//   | typ ID ASSIGN LBRACE args_list RBRACE SEMI { Str_Assign($1, $2, List.rev $5) }
//   | typ ID ASSIGN LBRAKT args_list RBRAKT SEMI { Arr_Assign($1, $2, List.rev $5) }
//   | STRUCT SID LBRACE sdecl_list RBRACE SEMI { Str_Decl($2, List.rev $4) }


sdecl_list:
    typ ID SEMI                 { [($1,$2)] }
  | sdecl_list typ ID SEMI            { ($2,$3) :: $1 }


stmt:
    expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }
// vdecls
  | typ ID ASSIGN expr SEMI                  { Decl($1, $2, $4) }
  | typ ID ASSIGN LBRACE args_list RBRACE SEMI { Str_Decl($1, $2, List.rev $5) }
  | typ ID ASSIGN LBRAKT args_list RBRAKT SEMI { Arr_Decl($1, $2, List.rev $5) }
  | STRUCT SID LBRACE sdecl_list RBRACE SEMI { Str_Def($2, List.rev $4) }


obs_stmt:
    obs_expr SEMI                             { OExpr $1 }
  | OBS ASSIGN expr SEMI                      { OAssign($1, $3)         }
  | OBS ASSIGN obs_expr SEMI                  { OAssign($1, $3) }
// odecl:
  | typ OBS SEMI                              { Obs($2) }
  | typ OBS ASSIGN expr SEMI                  { ODecl($1, $2, $4) }
  | typ OBS ASSIGN obs_expr SEMI              { ODecl($1, $2, $4) }
  | typ OBS ASSIGN LBRACE args_list RBRACE SEMI { OStr_Decl($1, $2, List.rev $5) }
  | typ OBS ASSIGN LBRAKT args_list RBRAKT SEMI { OArr_Decl($1, $2, List.rev $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1)            }
  | NULL             { Void                    }
  | FLIT             { Fliteral($1)           }
  | CHLIT            { Chliteral($1)          }
  | ID               { Id($1)                 }
  | SID              { Sid($1)                }
  | ID DOT LEN       { Len($1)                }
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
  | ID LBRAKT expr RBRAKT                     { Arr_Ref($1, $3)  }
  | ID LPAREN args_opt RPAREN                 { Call(Id($1), $3) }
  | LPAREN expr RPAREN LPAREN args_opt RPAREN { Call($2, $5)     }
  | LPAREN expr RPAREN { $2                   }
  | IF LPAREN expr RPAREN expr ELSE expr    { If($3, $5, $7) }
  | LPAREN formals_opt RPAREN ARROW LBRACE func_body RBRACE { FuncExpr(List.rev $2, List.rev $6) }   /* anonymous function */

obs_expr:
  | obs_var              { Id($1) }
  | obs_expr PLUS   expr { Binop($1, Add,   $3)   }
  | obs_expr DIVIDE expr { Binop($1, Div,   $3)   }
  | obs_expr MINUS  expr { Binop($1, Sub, $3)     }
  | obs_expr TIMES  expr { Binop($1, Mult, $3)     }
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
  | expr MINUS  obs_expr { Binop($1, Sub, $3)     }
  | expr TIMES  obs_expr { Binop($1, Mult, $3)     }
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
  | obs_expr MINUS  obs_expr { Binop($1, Sub, $3)     }
  | obs_expr TIMES  obs_expr { Binop($1, Mult, $3)     }
  | obs_expr EQ     obs_expr { Binop($1, Equal, $3)   }
  | obs_expr NEQ    obs_expr { Binop($1, Neq,   $3)   }
  | obs_expr LT     obs_expr { Binop($1, Less,  $3)   }
  | obs_expr LEQ    obs_expr { Binop($1, Leq,   $3)   }
  | obs_expr GT     obs_expr { Binop($1, Greater, $3) }
  | obs_expr GEQ    obs_expr { Binop($1, Geq,   $3)   }
  | obs_expr AND    obs_expr { Binop($1, And,   $3)   }
  | obs_expr OR     obs_expr { Binop($1, Or,    $3)   }

  | MINUS obs_expr         { Unop(Neg, $2)      }
  | OBS LBRAKT expr RBRAKT { Arr_Ref($1, $3) }
  | LPAREN obs_expr RPAREN { $2                   }



// id_var:
//     ID              { $1 }
//   // | id_var DOT ID   { Ref($1, $3) }


obs_var:
    OBS             { $1 }
  // | obs_var DOT ID   { Ref($1, $3) }


args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
