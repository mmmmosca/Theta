type tokenType =
  | ID of string
  | LPAREN
  | RPAREN
  | ARROW
  | LCURLY
  | RCURLY
  | DOT
  | IMP
  | STRING of string


let lex code = let _ = code in Utils.todo ()