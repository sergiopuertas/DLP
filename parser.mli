type token =
  | LAMBDA
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | SUCC
  | PRED
  | ISZERO
  | LET
  | LETREC
  | IN
  | NIL
  | CONS
  | ISNIL
  | HEAD
  | TAIL
  | CONCAT
  | BOOL
  | NAT
  | STRING
  | QUIT
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LCORCHETE
  | RCORCHETE
  | COMMA
  | DOT
  | EQ
  | COLON
  | ARROW
  | EOF
  | INTV of (int)
  | IDV of (string)
  | IDTY of (string)
  | STRINGV of (string)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.command
