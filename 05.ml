(* Reverse a list *)

let rec rev l =
    let rec rev_aux l result = 
        match l with
        | [] -> result
        | (hd :: rest) -> rev_aux (rest) (hd :: result)
    in
    rev_aux l [];;
