(* rational numbers: ratios of integers *)

type t =
    {numerator: Z.t; denominator: N.t}
let zero = {numerator = Z.zero; denominator = N.one}
let one = {numerator = Z.one; denominator = N.one}
let minus_one = {numerator = Z.minus_one; denominator = N.one}
let trim q =
    let rec k_gcd a b =
        if K.is_zero b then
            None
        else if K.compare a b = 0 then
            Some a
        else if K.compare a b > 0 then
            let rem = K.rem a b in
            if K.compare rem K.zero > 0 then
                k_gcd b rem
            else
                Some b
        else
            k_gcd b a in
    let gcd_opt = k_gcd (Z.to_k q.numerator) @@ N.to_k q.denominator in
    match gcd_opt with
        None ->
            {numerator = Z.trim q.numerator; denominator = N.trim q.denominator}
        | Some gcd ->
            {
                numerator = Z.trim @@ Z.div q.numerator (Z.of_k gcd);
                denominator =
                    N.trim @@ N.of_k @@ K.div (N.to_k q.denominator) gcd}
let to_string q =
    let q = trim q in
    String.concat "/" [
        Z.to_string q.numerator;
        N.to_string q.denominator]
let commonate a b =
    let a_de = N.to_k a.denominator in
    let b_de = N.to_k b.denominator in
    Z.mul a.numerator @@ Z.of_k b_de,
        Z.mul b.numerator @@ Z.of_k a_de,
        N.of_k @@ K.mul a_de b_de
let compare a b =
    let an, bn, _ = commonate a b in
    Z.compare an bn
let is_zero q =
    Z.is_zero q.numerator
let is_positive q =
    Z.is_positive q.numerator
let is_negative q =
    Z.is_positive q.numerator
let is_integer q =
    Z.is_zero @@ Z.rem q.numerator (Z.of_n q.denominator)
let neg q =
    {q with numerator = Z.neg q.numerator}
let normalize q =
    if compare q one > 0 then
        one
    else if compare q minus_one < 0 then
        minus_one
    else
        q
let add a b =
    let a_nu, b_nu, de = commonate a b in
    trim {
        numerator = Z.add a_nu b_nu;
        denominator = de}
let sub a b =
    let a_nu, b_nu, de = commonate a b in
    trim {
        numerator = Z.sub a_nu b_nu;
        denominator = de}
let mul a b =
    trim {
        numerator = Z.mul a.numerator b.numerator;
        denominator = N.mul a.denominator b.denominator}
let reciprocate q =
    if Z.compare q.numerator Z.zero = 0 then
        raise @@ Invalid_argument "cannot reciprocate zero"
    else
        {
            numerator = Z.mul (Z.of_n q.denominator) @@ Z.normalize q.numerator;
            denominator = Z.to_n @@ Z.abs q.numerator}
let div a b =
    mul a @@ reciprocate b
let of_z z =
    {numerator = z; denominator = N.one}
let of_i64 i =
    of_z @@ Z.of_i64 i
let of_i32 i =
    of_z @@ Z.of_i32 i
let of_int i =
    of_z @@ Z.of_int i
let of_n n =
    of_z @@ Z.of_n n
let of_k k =
    of_z @@ Z.of_k k
let to_i32 q =
    if is_integer q then
        Z.to_i32 @@ Z.div (q.numerator) @@ Z.of_n q.denominator
    else
        raise @@ Invalid_argument (
            Printf.sprintf "Cannot convert Q %s to int32; Q is not an integer"
            (to_string q))
let to_i64 q =
    if is_integer q then
        Z.to_i64 @@ Z.div (q.numerator) @@ Z.of_n q.denominator
    else
        raise @@ Invalid_argument (
            Printf.sprintf "Cannot convert Q %s to int64; Q is not an integer"
            (to_string q))
let to_int q =
    if is_integer q then
        Z.to_int @@ Z.div (q.numerator) @@ Z.of_n q.denominator
    else
        raise @@ Invalid_argument (
            Printf.sprintf "Cannot convert Q %s to int; Q is not an integer"
            (to_string q))
let to_z q =
    if is_integer q then
        Z.div (q.numerator) @@ Z.of_n q.denominator
    else
        raise @@ Invalid_argument (
            Printf.sprintf "Cannot convert Q %s to Z; Q is not an integer"
            (to_string q))
let to_n q =
    if is_integer q && compare q zero > 0 then
        Z.to_n @@ Z.div (q.numerator) @@ Z.of_n q.denominator
    else
        raise @@ Invalid_argument (
            Printf.sprintf "Cannot convert Q %s to N; Q is not a natural"
            (to_string q))
let to_k q =
    if is_integer q && compare q minus_one > 0 then
        Z.to_k @@ Z.div (q.numerator) @@ Z.of_n q.denominator
    else
        raise @@ Invalid_argument (
            Printf.sprintf "Cannot convert Q %s to K; Q is not a positive integer"
            (to_string q))