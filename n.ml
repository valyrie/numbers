(* natural numbers: 0, 1, 2, 3, n *)
type t = int list
let rec is_zero (n: t) =
    match n with
    [] -> true
    | 0 :: tl -> is_zero tl
    | _ -> false
let rec succ (n: t): t =
    match n with
        [] | [0] -> [1]
        | hd :: tl ->
            if hd < Int.max_int then
                hd + 1 :: tl
            else
                0 :: succ tl
let rec pred (n: t): t =
    match n with
    [] | [0] -> raise (Invalid_argument "0 has no predecessor")
    | 1 :: [] -> []
    | 0 :: tl -> Int.max_int :: pred tl
    | hd :: tl -> hd - 1 :: tl
let of_int i: t =
    [i]
let to_int n =
    match n with
        [] -> 0
        | [i] -> i
        | _ -> raise (Invalid_argument "natural is too large to be converted to an int")
let rec compare x y =
    match (is_zero x, is_zero y) with
        true, true -> 0
        | true, false -> -1
        | false, true -> 1
        | false, false -> compare (pred x) (pred y)
let rec add x y =
    match x, y with
        _, [0] -> x
        | n :: ntl, m :: mtl when Int.max_int - m > n -> add (n + m :: ntl) (0 :: mtl)
        | _, _ -> add (succ x) (pred y)
let rec sub x y =
    match x, y with
        _, [0] -> x
        | n :: ntl, m :: mtl when m < n -> sub (n - m :: ntl) (0 :: mtl)
        | _, _ -> sub (pred x) (pred y)