open Circuit ;;

let neg a = nand a a
let ou a b = nand (neg a) (neg b)
let et a b = neg (nand a b)
let xor a b = let n = nand a b in nand (nand a n) (nand b n)
let mux flag a b = ou (et (neg flag) a) (et flag b)
(* mux renvoie une tension qui vaut celle de a quand flag=0 et celle de b sinon *)


(* Les fonctions suivantes prennent en entrée des tableau *)
let inverse a = Array.init (Array.length a) (fun i -> neg a.(i))

let selecteur flag a b = Array.init (Array.length a) (fun i -> mux flag a.(i) b.(i))

let et_logique a b = Array.init (Array.length a) (fun i -> et a.(i) b.(i))
  
let ou_logique a b = Array.init (Array.length a) (fun i -> ou a.(i) b.(i))

let xor_logique a b = Array.init (Array.length a) (fun i -> xor a.(i) b.(i))
