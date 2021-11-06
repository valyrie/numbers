(* counting numbers: 1, 2, 3, n *)

type t = N.t
let one = N.of_u32 1l
let trim k =
    N.trim k
let to_string k =
    N.to_string k
let add a b =
    N.add a b
let compare a b =
    N.compare a b
let sub a b =
    if compare a b > 0 then
        N.sub a b
    else
        raise @@ Invalid_argument (Printf.sprintf "Cannot subtract K %s from K %s" (to_string b) (to_string a))
let mul a b =
    N.mul a b
let addu32 k i =
    N.addu32 k i
let addu64 k i =
    N.addu64 k i
let of_u32 i =
    if Int32.compare i 0l > 0 then
        N.of_u32 i
    else
        raise @@ Invalid_argument (Printf.sprintf "cannot create K from %d" @@ Int32.to_int i)
let of_u64 i =
    if Int64.compare i 0L > 0 then
        N.of_u64 i
    else
        raise @@ Invalid_argument (Printf.sprintf "cannot create K from %d" @@ Int64.to_int i)