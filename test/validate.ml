open Aiken

let expect_failure exp t =
  try
    Format.printf "%a@." Printer.pp t;
    exit 1
  with e ->
    if e <> exp then exit 1


let () =
  expect_failure Printer.Invalid_question ["", []];
  expect_failure Printer.Invalid_question ["a. z", []];
  expect_failure Answers.Invalid ["q", []];
  expect_failure Answers.Invalid ["q", [false, "x"]];
  Format.printf "@.%a@." Printer.pp ["q", [true, "x"; false, "y"]]
