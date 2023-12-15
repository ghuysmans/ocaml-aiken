%token <string> TEXT
%token <char * string> CHOICE
%token <char> ANSWER
%token NL
%token EOF
%start <(string * (bool * string) list) list> quiz
%start <unit> title
%{
exception Non_consecutive_answer
exception Invalid_answer
let same_choice c c' = Char.(lowercase_ascii c = lowercase_ascii c')
%}
%%
choice: c=CHOICE NL { c }
question: q=TEXT NL l=nonempty_list(choice) a=ANSWER NL {
  let _, ex, answers =
    List.fold_left (fun (prev, ex, acc) (id', c) ->
      begin match prev with
        | None -> ()
        | Some id ->
          let exp = Char.(chr (code id + 1)) in
          if not (same_choice id' exp) then
            raise Non_consecutive_answer
      end;
      let correct = same_choice id' a in
      Some id', ex || correct, (correct, c) :: acc
    ) (None, false, []) l
  in
  if ex then
    q, List.rev answers
  else
    raise Invalid_answer
}
quiz: l=separated_list(NL, question) EOF { l }
title: TEXT EOF { () }
