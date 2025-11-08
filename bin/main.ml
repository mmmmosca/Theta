let () =
  match
    Lexer.lex "( ) \"Hello world\\\" io\n\nmain() -> {io.out(\"Hello world!\")}"
  with
  | Error errors -> Errors.print_errors errors
  | Ok tokens -> (
      match Parser.parse (Lexer.print_tokens tokens) with
      | Error errors -> Errors.print_errors errors
      | Ok ast -> (
          match Types.check_infer ast with
          | Error errors -> Errors.print_errors errors
          | Ok _ -> ()))
