(* integers: -n, -3, -2, -1, 0, 1, 2, 3, n *)

type t =
    Zero
    | Positive of N.t
    | Negative of N.t
let zero = Zero
let is_zero z =
    match z with
        Zero -> true
        | _ -> false
let trim z =
    match z with
        Zero -> Zero
        | Positive n -> Positive (N.trim n)
        | Negative n -> Negative (N.trim n)
let to_string z =
    match z with
        Zero -> "0"
        | Positive n -> Printf.sprintf " %s" @@ N.to_string n
        | Negative n -> Printf.sprintf "-%s" @@ N.to_string n
let neg z =
    match z with
        Zero -> Zero
        | Positive n -> Negative n
        | Negative n -> Positive n
let add a b =
    match a, b with
        _, Zero -> a
        | Zero, _ -> b
        | Positive n, Positive m -> Positive (N.add n m)
        | Negative n, Negative m -> Negative (N.add n m)
        | Positive p, Negative n
        | Negative n, Positive p ->
            if p > n then
                Positive (N.sub p n)
            else if p < n then
                Negative (N.sub n p)
            else
                Zero
let sub a b =
    add a @@ neg b
let compare a b =
    match sub a b with
        Zero -> 0
        | Positive _ -> 1
        | Negative _ -> -1
let mul a b =
    match a b with
        _, Zero
        | Zero, _ -> Zero
        | Positive n, Positive m
        | Negative n, Negative m -> Positive (N.mul n m)
        | Positive n, Negative m
        | Negative n, Positive m -> Negative (N.mul n m)
let is_counting z =
    match z with
        Positive _ -> true
        | _ -> false
let is_natural z =
    match z with
        Zero
        | Positive _ -> true
        | _ -> false
let addi32 z i =
    if Int32.compare i 0l > 0 then
        add z @@ Positive (N.of_u32 i)
    else if Int32.compare i 0l < 0 then
        sub z @@ Positive (N.of_u32 @@ Int32.neg i)
    else
        z
let addi64 z i =
    if Int64.compare i 0L > 0 then
        add z @@ Positive (N.of_u64 i)
    else if Int64.compare i 0L < 0 then
        sub z @@ Positive (N.of_u64 @@ Int64.neg i)
    else
        z
let of_i32 i =
    addi32 zero i
let of_i64 i =
    addi64 zero i