(* integers: -n, -3, -2, -1, 0, 1, 2, 3, n *)

type t =
    Zero
    | Positive of N.t
    | Negative of N.t
let is_zero z =
    z = Zero
let succ z =
    match z with
        Zero -> Positive (N.of_int 1)
        | Positive n -> Positive (N.succ n)
        | Negative n when n = N.of_int 1 -> Zero
        | Negative n -> Negative (N.pred n)
let pred z =
    match z with
        Zero -> Negative (N.of_int 1)
        | Negative n -> Negative (N.pred n)
        | Positive n when n = N.of_int 1 -> Zero
        | Positive n -> Positive (N.pred n)
let of_int i =
    if i > 0 then
        Positive (N.of_int i)
    else if i < 0 then
        Negative (N.of_int i)
    else
        Zero
let neg z =
    match z with
        Zero -> Zero
        | Positive n -> Negative n
        | Negative n -> Positive n