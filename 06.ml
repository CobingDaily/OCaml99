(* Palindrome *)

let rec is_palindrome l =
    let rec rev_aux l result = 
        match l with
        | [] -> result
        | (hd :: rest) -> rev_aux (rest) (hd :: result)
    in
    l = (rev_aux l []);; (* Or more simply: l = List.rev l *)
