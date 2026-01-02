(* Remove the K'th Element From a List *)

let rec remove_at k list =
    match list with
    | [] -> list
    | first :: rest ->
            if k = 0 then rest
            else first :: (remove_at (k-1) rest)
