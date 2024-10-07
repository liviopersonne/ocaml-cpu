open Circuit
open Logique

let vers_bus b =
  Array.init nb_bits (fun i -> if i=0 then b else zero)

(* Renvoie le couple (h,l) avec hl = a+b *)
let half_adder a b =
  let _ = a,b in (et a b, xor a b)
  
(* Renvoie le trouple (h,l) avec hl = a+b+c *)
let full_adder a b c =
  let _ = a,b,c in let x = xor a b in (ou (et x c) (et a b), xor x c)

let somme a b =
  let _ = a,b in failwith "Not implemented"

let increment a =
  let _ = a in failwith "Not implemented"

let decrement a =
  let _ = a in failwith "Not implemented"

let difference a b =
  let _ = a,b in failwith "Not implemented"

let est_nul a =
  let _ = a in failwith "Not implemented"

let est_negatif a =
  let _ = a in failwith "Not implemented"

let est_positif a =
  let _ = a in failwith "Not implemented"

  
let rec bit_liste_vers_nb = (* Suppose le petit-boutisme*)
  function 
  | [] -> 0
  | a::q -> a+2*(bit_liste_vers_nb q)


let nb_vers_bits n = (* Suppose le petit-boutisme *)
  let n = if n < 0 then n + 65536 else n in
  let rec foo n b =
    if b = 0
    then []
    else (n mod 2)::(foo (n/2) (b-1))
  in
  foo n nb_bits
  
let nb_to_array i = ((i+65536) mod 65536) |> nb_vers_bits |> Array.of_list

