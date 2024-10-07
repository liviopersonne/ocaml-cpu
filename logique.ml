open Circuit ;;

let neg a = nand a a
let ou a b = let _ = a,b in nand (neg a) (neg b)
let et a b = let _ = a,b in neg (nand a b)
let xor a b = let _ = a,b in let n = nand a b in nand (nand a n) (nand b n)
let mux flag a b = let _ = a,b,flag in if (flag = zero) then a else b
(* mux renvoie une tension qui vaut celle de a quand flag=0 et celle de b sinon *)


(* Les fonctions suivantes prennent en entrée des tableau *)
let inverse a = let _ = a in Array.init (Array.length a) (fun i -> neg a.(i))

let selecteur flag a b = let _ = a,b,flag in Array.init (Array.length a) (fun i -> mux flag a.(i) b.(i))

let et_logique a b = let _ = a,b in Array.init (Array.length a) (fun i -> et a.(i) b.(i))
  
let ou_logique a b = let _ = a,b in Array.init (Array.length a) (fun i -> ou a.(i) b.(i))

let xor_logique a b = let _ = a,b in  Array.init (Array.length a) (fun i -> xor a.(i) b.(i))
