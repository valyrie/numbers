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
    match x, y with
        [0], _ -> -1
        | _, [0] -> 1
        | [n], [m] -> Int.compare n m 
        | _, _ -> match (is_zero x, is_zero y) with
            true, true -> 0
            | true, false -> -1
            | false, true -> 1
            | false, false -> compare (pred x) (pred y)
let rec add x y =
    match x, y with
        _, [0] -> x
        | n :: ntl, m :: mtl when n != 0 && m != 0 -> let d = min (Int.max_int - n) m in 
            add (n + d :: ntl) (m - d :: mtl)
        | _, _ -> add (succ x) (pred y)
let rec sub x y =
    match x, y with
        _, [0] -> x
        | n :: ntl, m :: mtl when n != 0 && m != 0 -> let d = min n m in
            sub (n - d :: ntl) (m - d :: mtl)
        | _, _ -> sub (pred x) (pred y)