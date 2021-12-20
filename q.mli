type t = { numerator : Z.t; denominator : N.t; }
val zero : t
val one : t
val minus_one : t
val trim : t -> t
val to_string : t -> string
val commonate : t -> t -> Z.t * Z.t * N.t
val compare : t -> t -> int
val is_zero : t -> bool
val is_positive : t -> bool
val is_negative : t -> bool
val is_integer : t -> bool
val neg : t -> t
val normalize : t -> t
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val reciprocate : t -> t
val div : t -> t -> t
val of_z : Z.t -> t
val of_i64 : int64 -> t
val of_i32 : int32 -> t
val of_int : int -> t
val of_n : N.t -> t
val of_k : K.t -> t
val to_i32 : t -> int32
val to_i64 : t -> int64
val to_int : t -> int
val to_z : t -> Z.t
val to_n : t -> N.t
val to_k : t -> K.t