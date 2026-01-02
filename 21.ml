(* Insert an Element at a Given Position Into a List *)

let rec insert_at el n list =
    match list with
    | [] -> el :: list
    | first :: rest ->
            if n = 0 then el :: list
            else first :: (insert_at el (n-1) rest)
