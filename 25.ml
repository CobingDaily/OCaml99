(* Generate a Random Permutation of the Elements of a List *)

let rec permutation list =
    let index = list |> List.length |> Random.int in
    let (rest, element) = get_and_remove list index in
    match element with
    | None -> []
    | Some x -> 
        match rest with
        | [] -> [x]
        | rest -> x :: permutation rest;;

let get_and_remove list i =
    let rec aux list j acc =
        match list with
        | [] -> (list, None)
        | first :: rest ->
                if i = j then (List.rev acc @ rest, Some first)
                else aux rest (j+1) (first :: acc)
    in aux list 0 [];;

