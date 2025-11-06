type expression = Imp of string | Call of string * expression list

let parse = function
  | [] -> []
  | [ Lexer.IMP; Lexer.ID name ] :: rest -> [ Imp name ]
