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

let lex code =
  let rec loop code =
    if String.starts_with ~prefix:"(" code then
      LPAREN :: loop (String.sub code 1 (String.length code - 1))
    else if String.starts_with ~prefix:")" code then
      RPAREN :: loop (String.sub code 1 (String.length code - 1))
    else if String.starts_with ~prefix:"->" code then
      ARROW :: loop (String.sub code 2 (String.length code - 2))
    else if String.starts_with ~prefix:"{" code then
      LCURLY :: loop (String.sub code 1 (String.length code - 1))
    else if String.starts_with ~prefix:"}" code then
      RCURLY :: loop (String.sub code 1 (String.length code - 1))
    else if String.starts_with ~prefix:"." code then
      DOT :: loop (String.sub code 1 (String.length code - 1))
    else if String.starts_with ~prefix:"=" code then
      EQUALS :: loop (String.sub code 1 (String.length code - 1))
    else if String.contains " \n\t\r" (String.get code 0) then
      loop (String.sub code 1 (String.length code - 1))
    else []
  in
  Ok (loop code)

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
