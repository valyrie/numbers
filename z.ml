(* integers: -n, -3, -2, -1, 0, 1, 2, 3, n *)

type t =
    Positive of K.t
    | Negative of N.t
exception Underflow = K.Underflow
exception Division_by_zero = K.Division_by_zero
let zero = Positive K.zero
let one = Positive K.one
let minus_one = Negative N.one
let trim z =
    match z with
        Positive k -> Positive (K.trim k)
        | Negative n -> Negative (N.trim n)
let to_string z =
    match z with
        Positive k -> Printf.sprintf " %s" @@ K.to_string k
        | Negative n -> Printf.sprintf "-%s" @@ N.to_string n
let compare a b =
    match (a, b) with
        Positive _, Negative _ -> 1
        | Negative _, Positive _ -> -1
        | Positive x, Positive y -> K.compare x y
        | Negative x, Negative y -> N.compare y x
let neg z =
    match z with
        Positive k when K.is_zero k -> z
        | Positive k -> Negative (N.of_k k)
        | Negative n -> Positive (N.to_k n)
let add a b =
    match (a, b) with
        Positive x, Positive y -> Positive (K.add x y)
        | Negative x, Negative y -> Negative (N.add x y)
        | Positive p, Negative n
        | Negative n, Positive p ->
            if K.compare p (N.to_k n) >= 0 then
                Positive (K.sub p @@ N.to_k n)
            else
                Negative (N.of_k @@ K.sub (N.to_k n) p)
let sub a b =
    add a @@ neg b
let succ z =
    add z one
let pred z =
    sub z one
let double z =
    match z with
        Positive k -> Positive (K.double k)
        | Negative n -> Negative (N.of_k @@ K.double @@ N.to_k n)
let half z =
    match z with
        Positive k -> Positive (K.half k)
        | Negative n when N.is_one n -> Positive K.zero
        | Negative n -> Negative (N.of_k @@ K.half @@ N.to_k n)
let mul a b =
    match (a, b) with
        Positive x, Positive y -> Positive (K.mul x y)
        | Negative x, Negative y -> Positive (K.mul (N.to_k x) (N.to_k y))
        | Positive p, Negative n
        | Negative n, Positive p ->
            if K.compare p K.zero = 0 then
                Positive K.zero
            else
                Negative (N.of_k @@ K.mul p @@ N.to_k n)