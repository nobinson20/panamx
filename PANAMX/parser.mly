/* Ocamlyacc parser for PANAMX */

%{
open Ast
%}


%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA LBRACKET RBRACKET
%token PLUS MINUS TIMES DIVIDE MODULO ASSIGN INCREMENT DECREMENT
/* Should we have the matrix built-in function tokens on their own line  */
%token NOT EQ NEQ LT LEQ GT GEQ AND OR DOT STRUCT
%token HEIGHT WIDTH 
%token RETURN IF ELSE FOR WHILE INT BOOL STRING FLOAT VOID MATRIX
%token <int> LITERAL
%token <bool> BLIT
%token <string> STRLIT ID FLIT
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
%left TIMES DIVIDE MODULO
%left INCREMENT DECREMENT
%right NOT

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { ([], [], [])                          }
 | decls vdecl { let (a, b, c) = $1 in (($2 :: a), b, c) }
 | decls fdecl { let (a, b, c) = $1 in (a, ($2 :: b), c) }
 | decls sdecl { let (a, b, c) = $1 in (a, b, ($2 :: c)) }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = List.rev $4;
	 locals = List.rev $7;
	 body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
    typ ID                   { [($1,$2)]     }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT       { Int   }
  | BOOL      { Bool  }
  | FLOAT     { Float }
  | STRING    { String }
  | VOID      { Void }
  | MATRIX    { Matrix }
  | STRUCT ID { Struct($2) }

sdecl:
    STRUCT ID LBRACE sbody_list RBRACE SEMI { {
      sname = $2; 
      svar = $4 } }

sbody_list:
    vdecl            { [$1] }
  | vdecl sbody_list { $1 :: $2 }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9)   }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1)            }
  | FLIT	           { Fliteral($1)           }
  | BLIT             { BoolLit($1)            }
  | STRLIT           { StrLit($1)             }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr MODULO expr { Binop($1, Mod,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | MINUS expr %prec NOT { Unop(Neg, $2)      }
  | NOT expr         { Unop(Not, $2)          }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | expr INCREMENT   { Unop(Inc, $1)          }
  | expr DECREMENT   { Unop(Dec, $1)          }
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }
  | LPAREN expr RPAREN { $2                   }
  | LBRACKET matrixlit RBRACKET  { MatLit($2) }
  | LT expr COMMA expr GT { MatLitEmpty($2, $4) }
  | ID LBRACKET expr RBRACKET LBRACKET expr RBRACKET { MatIndex($1, $3, $6) }
  | ID LBRACKET expr RBRACKET LBRACKET expr RBRACKET ASSIGN expr { MatAssign($1, $3, $6, $9) }
  | ID HEIGHT        { Call("matrixHeight", [Id($1)]) }
  | ID WIDTH         { Call("matrixWidth",  [Id($1)]) }
  | LT STRUCT ID GT  { StructLit($3)          }
  | ID DOT ID        { Member($1, $3)         }
  | ID DOT ID ASSIGN expr { MemAssign($1, $3, $5) }

matrixlit:
    arraylit                { [$1] }
  | arraylit SEMI matrixlit { $1 :: $3 }

arraylit:
    expr                { [$1] }
  | expr COMMA arraylit { $1 :: $3 }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
