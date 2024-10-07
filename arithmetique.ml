open Circuit
open Logique

let vers_bus b =
  Array.init nb_bits (fun i -> if i=0 then b else zero)

(* Renvoie le couple (h,l) avec hl = a+b *)
let half_adder a b =
  let _ = a,b in (et a b, xor a b)
  
(* Renvoie le couple (h,l) avec hl = a+b+c *)
let full_adder a b c =
  let _ = a,b,c in let x = xor a b in (ou (et x c) (et a b), xor x c)

let somme a b =
  let _ = a,b in let n = Array.length a in
  let rep = Array.make n zero in
  let carry = ref zero in
  for i = 0 to (n-1) do
    let h,l = full_adder a.(i) b.(i) !carry in
    rep.(i) <- l;
    carry := h 
  done;
  rep

let increment a =
  let _ = a in
  let n = Array.length a in
  let one = Array.init n (fun i -> if i = 0 then un else zero) in
  somme a one

let decrement a =
  let _ = a in
  let n = Array.length a in
  let neg_one = Array.make n un in
  somme a neg_one

let difference a b =
  let _ = a,b in
  let compl_b = increment (inverse b) in
  somme a compl_b

let est_nul a =
  let _ = a in Array.fold_left (fun curr x -> curr && x = zero) (true) a

let est_negatif a =
  let _ = a in a.(0) = un

let est_positif a =
  let _ = a in a.(0) = zero

  
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

