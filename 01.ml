(* Tail of a list *)

let rec last l =
    match l with
    | [] -> None
    | [e] -> Some e
    | (hd :: tl) -> last(tl)
