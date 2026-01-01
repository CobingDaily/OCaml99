(* Length of a list *)

let rec length l =
    match l with
    | [] -> 0
    | (hd :: rest) -> 1 + length rest;;


(* Bonus tail recursion *)

let length_tailrec l =
    let rec len accum l =
        match l with
        | [] -> accum
        | (hd :: rest) -> len (accum+1) (rest)
    in
    len 0 l;;
