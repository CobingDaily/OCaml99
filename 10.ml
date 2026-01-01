(* Run-Length Encoding *)

let encode list =
    let rec aux list count acc =
        match list with
        | [] -> []
        | [x] -> (count+1, x) :: acc
        | (a :: (b :: _ as t)) ->
                if a = b then aux (t) (count+1) (acc)
                else aux (t) (0) ((count+1, a) :: acc)
    in aux list 0 [] |> List.rev
