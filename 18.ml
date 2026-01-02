(* Extract a Slice From a List *)

let slice list i k =
    let rec aux list j acc =
        match list with
        | [] -> acc
        | hd :: rest -> 
                if (i<=j && j<=k) then aux rest (j+1) (hd :: acc)
                else aux rest (j+1) acc
    in aux list 0 []
        |> List.rev
