(* Rotate a List N Place to the Left *)

let rotate list n =
    let rec aux list acc i =
        match list with
        | [] -> List.append list (List.rev acc)
        | first :: rest ->
                if (i>0) then aux rest (first :: acc) (i-1)
                else List.append list (List.rev acc)
    in aux list [] n

