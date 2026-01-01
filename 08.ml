(* Eliminate Duplicates *)

let rec compress l =
    match l with
    | [] -> l
    | [_] -> l
    | (first :: second :: rest) ->
            if first = second then compress (second :: rest)
            else first :: (compress (second :: rest))

