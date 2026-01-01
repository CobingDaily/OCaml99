(* Replicate the elements of a List a Given Number of Times *)

let replicate list k =
    let rec prepend list value n =
        if n > 0 then prepend (value :: list) (value) (n-1)
        else list in
    let rec aux list acc =
        match list with
        | [] -> acc
        | first :: rest -> aux rest (prepend acc first k) in
    aux list [] |> List.rev;;

