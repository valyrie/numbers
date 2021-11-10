(* unsigned integers: 0, 1, 2, 3, n *)

(*                          UNSIGNED INTEGERS, AKA k                          *)
(*    Unsigned integer literals (aka k) are stored as a sequence of bytes in  *)
(* little-endian form, which is to say the least significant byte is stored   *)
(* first. unsigned integers are the non-negative integers, including 0. This  *)
(* somewhat simplifies operations on them, due to having a zero-element, vs   *)
(* the naturals.                                                              *)
(*    Operations on unsigned integers tend to be digit-by-digit, and require  *)
(* more iterations as the operands acquire more digits.                       *)
(*                              WHAT IS A DIGIT?                              *)
(*    Being stored as a little-endian sequence of bytes, it makes sense for   *)
(* operations on unsigned integers to iterate these bytes. This makes the     *)
(* smallest divisible component of an unsigned integer literal the byte or,   *)
(* alternatively, the digit. The words digit, char, and byte can be used      *)
(* interchangably when discussing these most primitive units of a number.     *)

(* K.t -- unsigned integer; ex: 0, 1, 2, 3, n *)
type t = bytes
exception Underflow
exception Division_by_zero
let make i k =
    Bytes.make i @@ Char.chr k
let u8 i = Int.logand i 0xFF
let u8carry i = Int.shift_right_logical i 8
let to_list k =
    List.map Char.code @@ List.of_seq @@ Bytes.to_seq k
let to_list_bits k =
    List.concat @@ List.map
        (fun i -> [
            Int.shift_right_logical i 0 |> Int.logand 1;
            Int.shift_right_logical i 1 |> Int.logand 1;
            Int.shift_right_logical i 2 |> Int.logand 1;
            Int.shift_right_logical i 3 |> Int.logand 1;
            Int.shift_right_logical i 4 |> Int.logand 1;
            Int.shift_right_logical i 5 |> Int.logand 1;
            Int.shift_right_logical i 6 |> Int.logand 1;
            Int.shift_right_logical i 7 |> Int.logand 1])
        @@ List.map Char.code @@ List.of_seq @@ Bytes.to_seq k
let fold_left f i k =
    List.fold_left f i @@ to_list k
let fold_right f k i =
    List.fold_right f (to_list k) i
let fold_left_bits f i k =
    List.fold_left f i @@ to_list_bits k
let fold_right_bits f k i =
    List.fold_right f (to_list_bits k) i
let make_zero i = make i 0
let zero = make_zero 1
let length k =
    Bytes.length k
let ctz_digits k =
    fold_left (fun s i -> if i != 0 then 0 else s + 1) 0 k
let clz_digits k =
    fold_right (fun s i -> if i != 0 then 0 else s + 1) k 0
let significant_digits k =
    max 1 @@ length k - ctz_digits k
let trim k =
    Bytes.sub k 0 @@ significant_digits k
let to_string k =
    let to_string_inner k =
        fold_left (fun s i -> Printf.sprintf "%02x%s" i s) "" @@ trim k in
    Printf.sprintf "0x%s" @@ to_string_inner k
let get k i =
    if i < length k then
        Bytes.get k i
    else
        Char.chr 0
let pad l k =
    let l = max l @@ length k in
    Bytes.init l (get k)
let pad2 l a b =
    let l = max l @@ max (length a) (length b) in
    pad l a, pad l b
let fold_left2 f i l a b =
    let (a, b) = pad2 l a b in
    List.fold_left2 f i
        (to_list a)
        (to_list b)
let fold_right2 f l a b i =
    let (a, b) = pad2 l a b in
    List.fold_right2 f
        (to_list a)
        (to_list b) i
let cat a b =
    Bytes.cat a b
let trunc a i =
    Bytes.sub (pad i a) 0 i
let lstrip a i =
    Bytes.sub (pad i a) 0 i
let rstrip a i =
    Bytes.sub (pad i a) i (Bytes.length (pad i a) - i)
let left_shift_digits n i =
    cat (make_zero i) n
let right_shift_digits n i =
    rstrip n i
let compare a b =
    fold_left2
        (fun c a b -> if Int.compare a b = 0 then c else Int.compare a b)
        0 0 a b
let is_zero n =
    compare n zero = 0
let add a b =
    let l = 1 + max (length a) (length b) in
    let (c, _, _) = fold_left2
        (fun (c, i, carry) a b ->
            let cn = a + b + carry in
            Bytes.set_uint8 c i @@ u8 cn;
            c, i + 1, u8carry cn)
        (make l 0, 0, 0) l a b in
    c
let sub a b =
    if compare a b > 0 then
        let l = max (length a) (length b) in
        let (c, _, _) = fold_left2
            (fun (c, i, carry) a b ->
                let cn, cb =
                    (if a - b - carry >= 0 then
                        a - b - carry, 0
                    else
                        (a + 256) - b - carry, 1) in
                Bytes.set_uint8 c i @@ cn;
                c, i + 1, cb)
            (make l 0, 0, 0) l a b in
        c
    else if compare a b = 0 then
        zero
    else
        raise @@ Underflow
let succ k =
    add k @@ make 1 1
let pred k =
    sub k @@ make 1 1
let double k =
    let l = length k + 1 in
    let (x, _, _) = fold_left
        (fun (x, i, carry) k ->
            let c = Int.shift_right_logical (Int.logand k 0b1000_0000) 7 in
            let n = carry + Int.shift_left k 1 in
            Bytes.set_uint8 x i @@ u8 n;
            x, i + 1, c)
        (make l 0, 0, 0) @@ pad l k in
    x
let half k =
    let l = length k in
    let (x, _, _) = fold_right
        (fun k (x, i, carry) ->
            let c = Int.logand 1 k in
            let n = (Int.shift_left carry 7) + Int.shift_right_logical k 1 in
            Bytes.set_uint8 x i @@ u8 n;
            x, i - 1, c)
        (pad l k) (make l 0, (l - 1), 0) in
    x
let mul a b =
    let l = (length a) + (length b) in
    let (c, _) = fold_left
        (fun (c, i) b ->
            let (c, _, _, _) = fold_left
                (fun (c, j, carry, b) a ->
                    add
                        (left_shift_digits (make 1 (u8 @@ a * b + carry)) j)
                        c, j + 1, u8carry @@ a * b + carry, b)
                (c, i, 0, b) @@ pad l a in
            c, i + 1)
        (make l 0, 0) @@ pad l b in
    c
let divmod a b =
    if not @@ is_zero b then
        ()
    else
        raise Division_by_zero
let addu32 n i =
    let b = make_zero 4 in
    let _ = Bytes.set_int32_le b 0 i in
    trim @@ add n b
let addu64 n i =
    let b = make_zero 8 in
    let _ = Bytes.set_int64_le b 0 i in
    trim @@ add n b
let of_u32 i =
    addu32 zero i
let of_u64 i =
    addu64 zero i
let of_uint i =
    addu64 zero (Int64.of_int i)
let to_u32 n =
    if compare n (of_u32 Int32.max_int) <= 0 then
        Bytes.get_int32_le (pad 4 n) 0
    else
        raise @@ Invalid_argument (
            Printf.sprintf "Cannot convert N %s to int32; N is too large"
                (to_string n))
let to_u64 n =
    if compare n (of_u64 Int64.max_int) <= 0 then
        Bytes.get_int64_le (pad 8 n) 0
    else
        raise @@ Invalid_argument (
            Printf.sprintf "Cannot convert N %s to int64; N is too large"
                (to_string n))
let to_uint n =
    Int64.to_int @@ to_u64 n
let logand a b =
    let l = max (length a) (length b) in
    let (c, _) = fold_left2
        (fun (c, i) a b ->
            Bytes.set_uint8 c i (Int.logand a b);
            c, i + 1)
        (make l 0, 0) l a b in
    c
let logor a b =
    let l = max (length a) (length b) in
    let (c, _) = fold_left2
        (fun (c, i) a b ->
            Bytes.set_uint8 c i (Int.logor a b);
            c, i + 1)
        (make l 0, 0) l a b in
    c
let logxor a b =
    let l = max (length a) (length b) in
    let (c, _) = fold_left2
        (fun (c, i) a b ->
            Bytes.set_uint8 c i (Int.logxor a b);
            c, i + 1)
        (make l 0, 0) l a b in
    c