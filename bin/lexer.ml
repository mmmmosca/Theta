(*
  THE NEXT THING TO ADD AS TOKENS ARE THE 'when' AND 'finally' KEYWORDS.
  THOSE ARE USED FOR CONTROL FLOW AND PRESENT THIS SYNTAX:
  fact(x) -> x * fact(x-1) when x > 1 finally x
*)

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
  | WHEN
  | FINALLY
  | EQUALS
  | STRING of string

let append what = function
  | Ok rest -> (
      match what with
      | Ok value -> Ok (value :: rest)
      | Error error -> Error [ error ])
  | Error rest -> (
      match what with Ok _ -> Error rest | Error error -> Error (error :: rest))

let rec lex code =
  if String.starts_with ~prefix:"(" code then
    append (Ok LPAREN) (lex (String.sub code 1 (String.length code - 1)))
  else if String.starts_with ~prefix:")" code then
    append (Ok RPAREN) (lex (String.sub code 1 (String.length code - 1)))
  else if String.starts_with ~prefix:"->" code then
    append (Ok ARROW) (lex (String.sub code 2 (String.length code - 2)))
  else if String.starts_with ~prefix:"{" code then
    append (Ok LCURLY) (lex (String.sub code 1 (String.length code - 1)))
  else if String.starts_with ~prefix:"}" code then
    append (Ok RCURLY) (lex (String.sub code 1 (String.length code - 1)))
  else if String.starts_with ~prefix:"." code then
    append (Ok DOT) (lex (String.sub code 1 (String.length code - 1)))
  else if String.starts_with ~prefix:"=" code then
    append (Ok EQUALS) (lex (String.sub code 1 (String.length code - 1)))
  else if String.starts_with ~prefix:"\"" code then
    Ok [] (*let rec lex_string code = () in
    lex_string code*)
  else if String.length code > 0 && Char.code (String.get code 0) < 33 then
    lex (String.sub code 1 (String.length code - 1))
  else Ok []

let print_tokens tokens =
  let rec loop tokens =
    match tokens with
    | LET :: rest ->
        print_endline "let";
        loop rest
    | WHEN :: rest ->
        print_endline "when";
        loop rest
    | FINALLY :: rest ->
        print_endline "finally";
        loop rest
    | ID id :: rest ->
        print_endline ("id: " ^ id);
        loop rest
    | LPAREN :: rest ->
        print_endline "(";
        loop rest
    | RPAREN :: rest ->
        print_endline ")";
        loop rest
    | ARROW :: rest ->
        print_endline "->";
        loop rest
    | LCURLY :: rest ->
        print_endline "{";
        loop rest
    | RCURLY :: rest ->
        print_endline "}";
        loop rest
    | DOT :: rest ->
        print_endline ".";
        loop rest
    | EQUALS :: rest ->
        print_endline "=";
        loop rest
    | STRING string :: rest ->
        print_endline ("string: " ^ string);
        loop rest
    | IMP :: rest ->
        print_endline "imp";
        loop rest
    | [] -> ()
  in
  loop tokens;
  tokens
