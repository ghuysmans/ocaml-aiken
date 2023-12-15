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
  expect_failure Printer.No_correct_answer ["q", []];
  expect_failure Printer.No_correct_answer ["q", [false, "x"]];
  Format.printf "@.%a@." Printer.pp ["q", [true, "x"; false, "y"]]
