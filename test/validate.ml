open Aiken

let expect_failure exp f =
  try
    f ();
    exit 1
  with e ->
    if e <> exp then exit 1

let validate l _ =
  List.iter (fun (q, a) ->
    ignore (Printer.question q);
    ignore (Answers.correct a)
  ) l

let parse s () =
  ignore (Parser.quiz Lexer.tokenize (Lexing.from_string s))


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
  expect_failure Printer.Invalid_question (validate ["", []]);
  expect_failure Printer.Invalid_question (validate ["a. z", []]);
  expect_failure Answers.Invalid (validate ["q", []]);
  expect_failure Answers.Invalid (validate ["q", [false, "x"]]);
  expect_failure Answers.Invalid (parse inv);
  expect_failure Answers.Non_consecutive (parse nc);
  validate ["q", [true, "x"; false, "y"]] ()
