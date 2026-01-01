(* Drop every N'th Element From a List *)

let drop list k =
    let rec aux list acc m =
        match list with
        | [] -> acc
        | first :: rest ->
                if m = 2 then aux rest acc ((m + 1) mod 3)
                else aux rest (first :: acc) ((m + 1) mod 3)
    in aux list [] 0
        |> List.rev;;
