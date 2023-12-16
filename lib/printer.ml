open Format

let letter i = Char.(chr (code 'A' + i))

exception Invalid_question

let question q =
  try
    Parser.title Lexer.tokenize (Lexing.from_string q);
    q
  with Parser.Error ->
    raise Invalid_question

let pp_question ppf (q, (l : Answers.t)) =
  let l = (l :> (bool * string) list) in
  try
    fprintf ppf "@[<v>%s" (question q);
    List.iteri (fun i (_, c) -> fprintf ppf "@;%c. %s" (letter i) c) l;
    fprintf ppf "@;ANSWER: %c@]" (letter (Answers.correct l))
  with Parser.Error ->
    raise Invalid_question

let pp ppf =
  let pp_sep ppf () = fprintf ppf "@;@;" in
  fprintf ppf "@[<v>%a@]" (pp_print_list ~pp_sep pp_question)
