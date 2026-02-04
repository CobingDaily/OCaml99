(* Rotate a List N Place to the Left *)

let rotate list n =
    let modulo x y =
      let result = x mod y in
      if result >= 0 then result
      else result + y
    in
    let n =
        let len = List.length list in
        (modulo n len)
    in
    let rec aux list acc i =
        match list with
        | [] -> List.append list (List.rev acc)
        | first :: rest ->
                if (i>0) then aux rest (first :: acc) (i-1)
                else List.append list (List.rev acc)
    in aux list [] n

