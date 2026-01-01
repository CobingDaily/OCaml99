(* N'th element of a list *)

let at index l =
    let rec _at accum l =
        match l with
        | [] -> None
        | (hd :: rest) ->
                if accum < index then _at (accum + 1) (rest)
                else Some hd
    in
    _at 0 l;;
