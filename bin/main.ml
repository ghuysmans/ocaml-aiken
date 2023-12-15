open Aiken

let () =
  let lexbuf = Lexing.from_channel stdin in
  Parser.quiz Lexer.tokenize lexbuf |>
  Format.printf "%a@." Printer.pp
