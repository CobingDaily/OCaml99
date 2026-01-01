(* Pack Consecutive Duplicates *)

let pack list =
    let rec aux list current acc =
        match list with
            | [] -> []
            | [x] -> (x :: current) :: acc
            | (a :: (b :: _ as t)) ->
                    if a = b then aux (t) (a :: current) acc
                    else aux t [] ((a :: current) :: acc)
    in
    List.rev (aux list [] [])
        

