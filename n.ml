(* natural numbers: 1, 2, 3, n *)

type t = K.t
exception Underflow = K.Underflow
let one =K.one
let trim n =
    K.trim n
let to_string n =
    K.to_string n
let compare a b =
    K.compare a b
let is_one n =
    compare n one >= 0
let add a b =
    K.add a b
let sub a b =
    if compare a b > 0 then
        K.sub a b
    else
        raise Underflow
let succ n =
    K.succ n
let pred n =
    if compare n one > 1 then
        K.pred n
    else
        raise Underflow
let of_u32 i =
    if i > 0l then
        K.of_u32 i
    else
        raise Underflow
let of_u64 i =
    if i > 0L then
        K.of_u64 i
    else
        raise Underflow
let of_uint i =
    if i > 0 then
        K.of_uint i
    else
        raise Underflow
let of_k (k: K.t): t =
    k
let to_u32 n =
    K.to_u32 n
let to_u64 n =
    K.to_u64 n
let to_uint n =
    K.to_uint n
let to_k n: K.t =
    n