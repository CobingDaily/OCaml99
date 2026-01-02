(* Extract a Given Number of Randomly Selected Elements From a List *)

let rec rand_select list n =
    let len = List.length list in
    let index = Random.int len in
    let (list, element) = get_and_remove list index in
    match element with
    | None -> raise Not_found
    | Some x ->
        match list with
        | [] -> [x]
        | list -> 
            if n > 0 then x :: rand_select list (n-1)
            else [];;

let get_and_remove list i =
    let rec aux list j acc =
        match list with
        | [] -> (list, None)
        | first :: rest ->
                if i = j then (List.rev acc @ rest, Some first)
                else aux rest (j+1) (first :: acc)
    in aux list 0 [];;


