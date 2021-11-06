(* integers: -n, -3, -2, -1, 0, 1, 2, 3, n *)

type t =
    Zero
    | Positive of K.t
    | Negative of K.t
let zero = Zero
let is_zero z =
    match z with
        Zero -> true
        | _ -> false
let trim z =
    match z with
        Zero -> Zero
        | Positive k -> Positive (K.trim k)
        | Negative k -> Negative (K.trim k)
let to_string z =
    match z with
        Zero -> "0"
        | Positive k -> Printf.sprintf " %s" @@ K.to_string k
        | Negative k -> Printf.sprintf "-%s" @@ K.to_string k
let neg z =
    match z with
        Zero -> Zero
        | Positive k -> Negative k
        | Negative k -> Positive k
let add a b =
    match a, b with
        _, Zero -> a
        | Zero, _ -> b
        | Positive k, Positive l -> Positive (K.add k l)
        | Negative k, Negative l -> Negative (K.add k l)
        | Positive p, Negative n
        | Negative n, Positive p ->
            if p > n then
                Positive (K.sub p n)
            else if p < n then
                Negative (K.sub n p)
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
        | Negative n, Negative m -> Positive (K.mul n m)
        | Positive n, Negative m
        | Negative n, Positive m -> Negative (K.mul n m)
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
        add z @@ Positive (K.of_u32 i)
    else if Int32.compare i 0l < 0 then
        sub z @@ Positive (K.of_u32 @@ Int32.neg i)
    else
        z
let addi64 z i =
    if Int64.compare i 0L > 0 then
        add z @@ Positive (K.of_u64 i)
    else if Int64.compare i 0L < 0 then
        sub z @@ Positive (K.of_u64 @@ Int64.neg i)
    else
        z
let of_i32 i =
    addi32 zero i
let of_i64 i =
    addi64 zero i