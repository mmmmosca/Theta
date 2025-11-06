type expression =
  | Imp of string
  | Let of string * expression
  | Block of expression list
  | Access of expression * string
  | Call of expression * expression list
  | Id of string
  | Str of string
  | FuncDef of string * string list * expression

let rec parse = function
  | [] -> []
  | [ Lexer.IMP; Lexer.STRING name ] :: rest -> [ Imp name ] :: parse rest
  | [] -> []
