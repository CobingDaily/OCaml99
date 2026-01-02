(* Lotto: Draw N Different Random Numbers From the Set 1..M *)

let lotto_select n m =
    let ms = up_to m in
    let rec aux n nums acc =
        match nums with
        | [] -> acc
        | nums ->
                let index = nums |> List.length |> Random.int in
                let (rest, element) = get_and_remove nums index in
                match element with
                | None -> raise Not_found
                | Some x ->
                        if n > 0 then aux (n-1) rest (x :: acc)
                        else acc
    in aux n ms [];;

let get_and_remove list i =
    let rec aux list j acc =
        match list with
        | [] -> (list, None)
        | first :: rest ->
                if i = j then (List.rev acc @ rest, Some first)
                else aux rest (j+1) (first :: acc)
    in aux list 0 [];;

let up_to n =
    let rec aux i acc =
        if (1 <= i && i <= n) then aux (i + 1) (i :: acc)
        else acc
    in aux 1 [];;

