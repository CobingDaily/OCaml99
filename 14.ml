(* Duplicate the Elements of a List *)

let rec duplicate list =
    match list with
    | [] -> []
    | first :: rest -> first :: first :: (duplicate rest);;


let dup_tailrec list =
    let rec aux list acc =
        match list with
        | [] -> acc
        | first :: rest -> aux rest (first :: first :: acc)
    in aux list []
        |> List.rev;;
