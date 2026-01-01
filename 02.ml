(* Last two elements of a list *)

let rec last_two l =
    match l with
    | [] -> None
    | [e] -> None
    | [e1; e2] -> Some (e1, e2)
    | (hd1 :: hd2 :: tl) -> last_two(hd2 :: tl)
