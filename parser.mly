
%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC
%token IN
%token NIL
%token CONS
%token ISNIL
%token HEAD
%token TAIL
%token CONCAT
%token BOOL
%token NAT
%token STRING
%token QUIT


%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET 
%token LCORCHETE
%token RCORCHETE
%token COMMA
%token DOT
%token EQ
%token COLON
%token ARROW
%token EOF
%token LTRIANG
%token RTRIANG
%token CASE
%token AS
%token OF
%token VBAR
%token VARROW

%token <int> INTV
%token <string> IDV
%token <string> IDTY
%token <string> STRINGV

%start s
%type <Lambda.command> s

%%

s :
    | IDV EQ term EOF { Bind ($1,$3)}
    | IDTY EQ ty EOF { BindTy($1, $3)} 
    | term EOF { Eval $1 }
    | QUIT EOF { Quit }

term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA IDV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET IDV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC IDV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }
  | LTRIANG IDV EQ term RTRIANG AS ty {TmTag ($2,$4,$7)}
  | CASE term OF cases
        { TmCase ($2, $4) }

appTerm :
    projTerm
      { $1 }
  | SUCC projTerm
      { TmSucc $2 }
  | PRED projTerm
      { TmPred $2 }
  | ISZERO projTerm
      { TmIsZero $2 }
  | CONCAT projTerm projTerm
      { TmConcat ($2, $3) }
  | appTerm projTerm
      { TmApp ($1, $2) }
  

 projTerm :
   | projTerm DOT INTV
      { TmProj ($1,(string_of_int $3))}
      
   | projTerm DOT IDV
      { TmProj ($1,$3)}

   | atomicTerm
      { $1 }      


atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | IDV EQ term
      { $3 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | IDV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
  | STRINGV 
      { TmString $1 }   

  | LBRACKET tuplesTM RBRACKET
     { TmTuple $2 }
  | LBRACKET recordTM RBRACKET
     {TmRecord $2}
  | NIL LCORCHETE ty RCORCHETE
      { TmNil $3 }
  | CONS LCORCHETE ty RCORCHETE atomicTerm atomicTerm
      { TmCons ($3, $5, $6) }
  | ISNIL LCORCHETE ty RCORCHETE atomicTerm
      { TmIsNil ($3, $5) }
  | HEAD LCORCHETE ty RCORCHETE atomicTerm
      { TmHead ($3, $5) }
  | TAIL LCORCHETE ty RCORCHETE atomicTerm
      { TmTail ($3, $5) }
    


recordTM:
    | {[]}
    | nonEmptyRecordTM {$1}

nonEmptyRecordTM:
    | IDV EQ term {[$1,$3]}
    | IDV EQ term COMMA nonEmptyRecordTM {($1,$3)::$5}

tuplesTM:
   | term { [$1] }
   | term COMMA tuplesTM { $1::$3 }
   
ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }


atomicTy :
    LPAREN ty RPAREN
      { $2 }
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRING
      { TyString }
  | LBRACKET tuplesTY RBRACKET
      { TyTuple $2 }
  | LBRACKET recordTY RBRACKET
      { TyRecord $2 }
  | IDTY
      { TyVarTy $1 }
  | LCORCHETE ty RCORCHETE
      { TyList $2 } 
  | LTRIANG noemptyrecordTY RTRIANG
      { TyVariant $2 }

recordTY:
  |        { [] }
  | noemptyrecordTY { $1 }

noemptyrecordTY:
  | IDV COLON ty {[$1,$3]}
  | IDV COLON ty COMMA noemptyrecordTY {($1,$3)::$5}
  
tuplesTY:
  | ty { [$1] }
  | ty COMMA tuplesTY { $1::$3 }   

cases:
    | case
        { [$1] }
    | case VBAR cases
        { $1::$3 }
    
case:
    | LTRIANG IDV EQ IDV RTRIANG VARROW appterm
        { ($2,$4, $7) }

