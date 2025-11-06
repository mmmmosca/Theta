type error = RunawayString

let print_errors errors =
  List.iter
    (fun error ->
      match error with RunawayString -> print_endline "Runaway String")
    errors
