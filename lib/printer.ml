open Format

exception No_correct_answer
exception Invalid_question

let letter i = Char.(chr (code 'a' + i))

let pp_question ppf (q, l) =
  try
    Parser.title Lexer.tokenize (Lexing.from_string q);
    fprintf ppf "@[<v>%s" q;
    begin match
      List.fold_left (fun (i, acc) (correct, c) ->
        fprintf ppf "@;%c. %s" (letter i) c;
        i + 1, if correct then Some i else acc
      ) (0, None) l
    with
    | _, None -> raise No_correct_answer
    | _, Some correct -> fprintf ppf "@;ANSWER: %c@]" (letter correct)
    end
  with Parser.Error ->
    raise Invalid_question

let pp ppf =
  let pp_sep ppf () = fprintf ppf "@;@;" in
  fprintf ppf "@[<v>%a@]" (pp_print_list ~pp_sep pp_question)
