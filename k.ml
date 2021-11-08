(* naturals: 0, 1, 2, 3, n *)

type t = bytes
let make i n =
    Bytes.make i @@ Char.chr n
let u8 i = Int.logand i 0xFF
let u8carry i = Int.shift_right_logical i 8
let to_list n =
    List.map Char.code @@ List.of_seq @@ Bytes.to_seq n
let fold_left f i n =
    List.fold_left f i @@ to_list n
let fold_right f n i =
    List.fold_right f (to_list n) i
let make_zero i = make i 0
let zero = make_zero 1
let length n =
    Bytes.length n
let ctz_digits n =
    fold_left (fun s i -> if i != 0 then 0 else s + 1) 0 n
let clz_digits n =
    fold_right (fun s i -> if i != 0 then 0 else s + 1) n 0
let significant_digits n =
    max 1 @@ length n - ctz_digits n
let trim n =
    Bytes.sub n 0 @@ significant_digits n
let to_string n =
    let to_string_inner n =
        fold_left (fun s i -> Printf.sprintf "%02x%s" i s) "" @@ trim n in
    Printf.sprintf "0x%s" @@ to_string_inner n
let get n i =
    if i < length n then
        Bytes.get n i
    else
        Char.chr 0
let pad l n =
    let l = max l @@ length n in
    Bytes.init l (get n)
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
        (fun c a b -> if Int.compare a b = 0 then c else Int.compare a b) 0 0 a b
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
        raise @@ Invalid_argument (Printf.sprintf "Cannot subtract N %s from N %s" (to_string b) (to_string a))
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
        raise @@ Invalid_argument (Printf.sprintf "Cannot convert N %s to int32; N is too large" (to_string n))
let to_u64 n =
    if compare n (of_u64 Int64.max_int) <= 0 then
        Bytes.get_int64_le (pad 8 n) 0
    else
        raise @@ Invalid_argument (Printf.sprintf "Cannot convert N %s to int64; N is too large" (to_string n))
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