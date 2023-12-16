type t = (bool * string) list

exception Non_consecutive
exception Invalid

let parse l a =
  let _, ex, answers =
    List.fold_left (fun (id, ex, acc) (id', c) ->
      let exp = Char.(chr (code id + 1)) in
      if id' <> exp then raise Non_consecutive;
      let correct = id' = a in
      id', ex || correct, (correct, c) :: acc
    ) (Char.(chr (code 'A' - 1)), false, []) l
  in
  if ex then
    List.rev answers
  else
    raise Invalid

let correct l =
  match
    List.fold_left (fun (i, acc) (correct, _) ->
      i + 1, if correct then Some i else acc
    ) (0, None) l
  with
  | _, None -> raise Invalid
  | _, Some x -> x
