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
let is_zero z =
    compare z zero = 0
let is_negative z =
    compare z zero < 0
let is_positive z =
    compare z zero > 0
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
            if K.compare p K.zero > 0 then
                Negative (N.of_k @@ K.mul p @@ N.to_k n)
            else
                Positive K.zero
let divrem a b =
    match (a, b) with
        Positive x, Positive y ->
            let (q, r) = K.divrem x y in
            Positive q, Positive r
        | Negative x, Negative y ->
            let (q, r) = K.divrem (N.to_k x) (N.to_k y) in
            Positive q, 
                if K.compare r K.zero > 0 then
                    Negative (N.of_k r)
                else
                    Positive r
        | Positive x, Negative y ->
            let (q, r) = K.divrem x (N.to_k y) in
            (if K.compare q K.zero > 0 then
                Negative (N.of_k q)
            else
                Positive q),
                Positive r
        | Negative x, Positive y ->
            let (q, r) = K.divrem (N.to_k x) y in
            (if K.compare q K.zero > 0 then
                Negative (N.of_k q)
            else
                Positive q),
                if K.compare r K.zero > 0 then
                    Negative (N.of_k r)
                else
                    Positive r
let div a b =
    let (q, _) = divrem a b in
    q
let rem a b =
    let (_, r) = divrem a b in
    r
let of_i32 i =
    if Int32.compare i 0l >= 0 then
        Positive (K.of_u32 i)
    else
        Negative (N.of_u32 @@ Int32.neg i)
let of_i64 i =
    if Int64.compare i 0L >= 0 then
        Positive (K.of_u64 i)
    else
        Negative (N.of_u64 @@ Int64.neg i)
let of_int i =
    of_i64 @@ Int64.of_int i
let of_n n =
    Positive (N.to_k n)
let of_k k =
    Positive k
let to_i32 z =
    if compare z (of_i32 Int32.max_int) <= 0 then
        if compare z (of_i32 Int32.min_int) >= 0 then
            match z with
                Positive k -> K.to_u32 k
                | Negative n -> Int32.neg @@ N.to_u32 n
        else
            raise @@ Invalid_argument (
                Printf.sprintf "Cannot convert Z %s to int32; Z is too small"
                (to_string z))
    else
        raise @@ Invalid_argument (
            Printf.sprintf "Cannot convert Z %s to int32; Z is too large"
                (to_string z))
let to_i64 z =
    if compare z (of_i64 Int64.max_int) <= 0 then
        if compare z (of_i64 Int64.min_int) >= 0 then
            match z with
                Positive k -> K.to_u64 k
                | Negative n -> Int64.neg @@ N.to_u64 n
        else
            raise @@ Invalid_argument (
                Printf.sprintf "Cannot convert Z %s to int64; Z is too small"
                (to_string z))
    else
        raise @@ Invalid_argument (
            Printf.sprintf "Cannot convert Z %s to int64; Z is too large"
                (to_string z))
let to_int z =
    Int64.to_int @@ to_i64 z
let to_n z =
    match z with
        Positive k -> N.of_k k
        | _ -> raise Underflow
let to_k z =
    match z with
        Positive k -> k
        | _ -> raise Underflow