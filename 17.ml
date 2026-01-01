(* Split list Into Two Parts; The Length of the First Part is Given *)

let split list n =
    let rec aux list acc k =
        match list with
        | [] -> (List.rev acc, list)
        | first :: rest ->
                if k = n then (List.rev acc, list)
                else aux rest (first :: acc) (k+1)
    in aux list [] 0
