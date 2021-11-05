(* rational numbers -- all numbers that can be represented by Q/N when N <> 0*)

type t =
    Integer of Z.t
    | Fraction of Z.t * N.t