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
let to_int z =
    match z with
        Zero -> 0
        | Positive n -> N.to_int n
        | Negative n -> -(N.to_int n)
let neg z =
    match z with
        Zero -> Zero
        | Positive n -> Negative n
        | Negative n -> Positive n
let rec compare x y =
    match x, y with
        Zero, Zero -> 0
        | Negative _, Zero
        | Zero, Positive _ -> -1
        | Positive _, Zero
        | Zero, Negative _ -> 1
        | Positive _, _ -> compare (pred x) (pred y)
        | Negative _, _ -> compare (succ x) (succ y)
let rec add x y =
    match (x, y) with
        _, Zero -> x
        | Zero, _ -> y
        | _, Positive _ -> add (succ x) (pred y)
        | _, Negative _ -> add (pred x) (succ y)
let sub x y =
    add x @@ neg y