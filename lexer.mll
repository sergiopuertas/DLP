{
  open Parser;;
  exception Lexical_error;;
}

rule token = parse
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "as"        {AS}
  | "of"        {OF}
  | "case"      {CASE}
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "let"       { LET }
  | "letrec"    { LETREC }
  | "in"        { IN }
  | "nil"       { NIL }
  | "cons"      { CONS }
  | "isnil"     { ISNIL }
  | "head"      { HEAD }
  | "tail"      { TAIL }
  | "concat"    { CONCAT }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "String"    { STRING }
  | "chao"      { QUIT } 
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '{'         {LBRACKET}
  | '}'         {RBRACKET}
  | '['         {LCORCHETE}
  | ']'         {RCORCHETE}
  | "<"         {LTRIANG}
  | ">"         {RTRIANG}
  | '|'         {VBAR}
  | "=>"        {VARROW}
  | ','         {COMMA}
  | '.'         { DOT }
  | '='         { EQ }
  | ':'         { COLON }
  | "->"        { ARROW }
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['A'-'Z']['a'-'z' '_' '0'-'9']* { IDTY (Lexing.lexeme lexbuf) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']* { IDV (Lexing.lexeme lexbuf) }
  | '"'[^'"' ';' '\n']*'"' {let s = Lexing.lexeme lexbuf in
                            STRINGV (String.sub s 1 (String.length s - 2))}              
  | eof         { EOF }
  | _           { raise Lexical_error }