type expression = Imp of string | Call of string * expression list

let rec parse = function
  | [] -> []
  | [ Lexer.IMP; Lexer.STRING name ] :: rest -> [ Imp name ] :: parse rest
  | [] -> []
