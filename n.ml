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
let fold_right f i n =
    List.fold_right f (to_list n) i
let make_zero i = make i 0
let zero = make_zero 1
let length n =
    Bytes.length n
let ctz_digits n =
    fold_left (fun s i -> if i != 0 then 0 else s + 1) 0 n
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
let pad_to l a b =
    let l = max l @@ max (length a) (length b) in
    Bytes.init l (get a), Bytes.init l (get b)
let fold_left2 f i l a b =
    let (a, b) = pad_to l a b in
    List.fold_left2 f i
        (to_list a)
        (to_list b)
let fold_right2 f a b i l=
    let (a, b) = pad_to l a b in
    List.fold_right2 f
        (to_list a)
        (to_list b) i
let add a b =
    let l = 1 + max (length a) (length b) in
    let (c, _, _) = fold_left2
        (fun (c, i, carry) a b ->
            let cn = a + b + carry in
            Bytes.set_uint8 c i @@ u8 cn;
            c, i + 1, u8carry cn)
        (make l 0, 0, 0) l a b in
    c
let compare a b =
    fold_left2
        (fun c a b -> if Int.compare a b = 0 then c else Int.compare a b) 0 0 a b
let sub a b =
    if compare a b > 0 then
        let l = max (length a) (length b) in
        let (c, _, _) = fold_left2
            (fun (c, i, carry) a b ->
                let cn = a - b - carry in
                Bytes.set_uint8 c i @@ u8 cn;
                c, i + 1, u8carry cn)
            (make l 0, 0, 0) l a b in
        c
    else if compare a b = 0 then
        zero
    else
        raise @@ Invalid_argument (Printf.sprintf "Cannot subtract N %s from N %s" (to_string b) (to_string a))
let mul a b =
    let l = (length a) + (length b) in
    let (c, _, _) = fold_left2
        (fun (c, i, carry) a b ->
            let cn = a * b + carry in
            Bytes.set_uint8 c i @@ u8 cn;
            c, i + 1, u8carry cn)
        (make l 0, 0, 0) l a b in
    c
let is_couting n =
    compare n zero > 0
let cat a b =
    Bytes.cat a b
let addu32 n i =
    let b = make_zero 4 in
    let _ = Bytes.set_int32_ne b 0 i in
    add n b
let addu64 n i =
    let b = make_zero 8 in
    let _ = Bytes.set_int64_ne b 0 i in
    add n b
let of_u32 i =
    addu32 zero i
let of_u64 i =
    addu64 zero i