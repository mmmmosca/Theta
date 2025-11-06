type tokenType =
  | ID of string
  | LPAREN
  | RPAREN
  | ARROW
  | LCURLY
  | RCURLY
  | DOT
  | IMP
  | LET
  | EQUALS
  | STRING of string


let lex code = let _ = code in Utils.todo ()