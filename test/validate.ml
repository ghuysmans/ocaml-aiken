open Aiken

let expect_failure exp f =
  try
    Format.printf "%a@." Printer.pp (f ());
    exit 1
  with e ->
    if e <> exp then exit 1

let const k _ = k
let parse s () = Parser.quiz Lexer.tokenize (Lexing.from_string s)


let inv = {|q
A. x
B. y
ANSWER: C
|}

let nc = {|q
A. x
C. z
ANSWER: A
|}

let () =
  expect_failure Printer.Invalid_question (const ["", []]);
  expect_failure Printer.Invalid_question (const ["a. z", []]);
  expect_failure Answers.Invalid (const ["q", []]);
  expect_failure Answers.Invalid (const ["q", [false, "x"]]);
  expect_failure Answers.Invalid (parse inv);
  expect_failure Answers.Non_consecutive (parse nc);
  Format.printf "@.%a@." Printer.pp ["q", [true, "x"; false, "y"]]
