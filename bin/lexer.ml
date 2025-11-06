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


let lex code =
  let rec loop code =
    if String.starts_with ~prefix: "let" code then
      [LET] :: loop (String.sub code 3 0)
    else
      []
  in
  Ok(loop code)
  