(* natural numbers: 0, 1, 2, 3, n *)
type t = int list
let rec is_zero (x: t) =
    match x with
    [] -> true
    | 0 :: tl -> is_zero tl
    | _ -> false
let rec succ (x: t): t =
    match x with
        [] | [0] -> [1]
        | hd :: tl ->
            if hd < Int.max_int then
                hd + 1 :: tl
            else
                0 :: succ tl
let rec pred (x: t): t =
    match x with
    [] | [0] -> raise (Invalid_argument "0 has no predecessor")
    | 1 :: [] -> []
    | 0 :: m -> Int.max_int :: pred m
    | n :: m -> n - 1 :: m
let of_int i: t =
    [i]
let rec compare x y =
    match (is_zero x, is_zero y) with
        true, true -> 0
        | true, false -> -1
        | false, true -> 1
        | false, false -> compare (pred x) (pred y)
let rec add x y =
    if is_zero y then
        x
    else
        add (succ x) (pred y)
let rec sub x y =
    if is_zero y then
        x
    else
        sub (pred x) (pred y)