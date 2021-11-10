type t
exception Underflow
exception Division_by_zero
val make : int -> int -> t
val to_list : t -> int list
val to_list_bits : t -> bool list
val fold_left : ('a -> int -> 'a) -> 'a -> t -> 'a
val fold_right : (int -> 'a -> 'a) -> t -> 'a -> 'a
val fold_left_bits : ('a -> bool -> 'a) -> 'a -> t -> 'a
val fold_right_bits : (bool -> 'a -> 'a) -> t -> 'a -> 'a
val zero : t
val one : t
val length : t -> int
val significant_digits : t -> int
val trim : t -> t
val to_string : t -> string
val pad : int -> t -> t
val pad2 : int -> t -> t -> t * t
val fold_left2 : ('a -> int -> int -> 'a) -> 'a -> int -> t -> t -> 'a
val fold_right2 : (int -> int -> 'a -> 'a) -> int -> t -> t -> 'a -> 'a
val cat : t -> t -> t
val trunc : t -> int -> t
val compare : t -> t -> int
val is_zero : t -> bool
val add : t -> t -> t
val sub : t -> t -> t
val succ : t -> t
val pred : t -> t
val double : t -> t
val half : t -> t
val mul : t -> t -> t
val divrem : t -> t -> t * t
val div : t -> t -> t
val rem : t -> t -> t
val of_u32 : int32 -> t
val of_u64 : int64 -> t
val of_uint : int -> t
val to_u32 : t -> int32
val to_u64 : t -> int64
val to_uint : t -> int
val logand : t -> t -> t
val logor : t -> t -> t
val logxor : t -> t -> t
val left_shift : t -> t -> t
val right_shift : t -> t -> t