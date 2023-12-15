open Aiken

let () =
  let lexbuf = Lexing.from_channel stdin in
  Parser.quiz Lexer.tokenize lexbuf |>
  List.iter (fun (q, l) ->
    Printf.printf "%s\n" q;
    l |> List.iteri (fun i (correct, c) ->
      Printf.printf "%c. %s%s\n"
        Char.(chr (code 'a' + i))
        c
        (if correct then " (*)" else "")
    )
  )
