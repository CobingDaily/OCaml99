(* Flatten a List *)

type 'a node =
  | One of 'a 
  | Many of 'a node list

let flatten nested =
    let rec aux nested acc =
        match nested with
        | [] -> acc
        | One x :: rest -> aux rest (x :: acc)
        | Many xs :: rest -> aux rest (aux xs acc)
    in aux nested [] |> List.rev
