(* open Logique *)
open Circuit
open Arithmetique
open Memory
(* open Alu *)


let print_array (a) =
  print_string "[|";
  Array.iter (Printf.printf "%d, ") a;
  print_string "\b\b|]\n"


(* 
  On arrive enfin au cpu entier !
  Arguments:
    program: Tableau d'instructions à exécuter (au plus 256 puisque c'est stocké dans la rom)
  Sortie:
    pc: Program Counter
    opcode: Operation code
    r2: Registre n°2 (lu)
    r3: Registre n°3 (lu)
*)
let cpu (program: tension array array): tension array * tension array * tension array * tension array =
  let _ = program in
  let set_pc = nouvelle_tension() in
  let zero_word = Array.init (nb_bits) (fun _ -> nouvelle_tension ()) in
  let pc = word_registre set_pc zero_word in
  
  let zero_array = nb_to_array 0 in
  let rep = compile zero_word pc zero_array in
  print_array rep;
  ([||], [||], [||], [||])



let _ = cpu ([||])
  
      
