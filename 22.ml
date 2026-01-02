(* Create a List Containing All Integers Within a Given Range *)

let range m n =
    let rec aux i acc =
        if i <= n then aux (i+1) (i :: acc)
        else acc
    in aux m [] |> List.rev
