type t
exception Underflow
val one : t
val trim : t -> t
val to_string : t -> string
val compare : t -> t -> int
val is_one : t -> bool
val add : t -> t -> t
val sub : t -> t -> t
val succ : t -> t
val pred : t -> t
val mul : t -> t -> t
val of_u32 : int32 -> t
val of_u64 : int64 -> t
val of_uint : int -> t
val of_k : K.t -> t
val to_u32 : t -> int32
val to_u64 : t -> int64
val to_uint : t -> int
val to_k : t -> K.t