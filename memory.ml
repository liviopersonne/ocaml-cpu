(* open Circuit *)
(* open Logique *)

let bit_registre  doit_ecrire valeur_ecrite =
  let _ = (doit_ecrire,valeur_ecrite) in failwith "Bit registre not implemented!"
  
let word_registre doit_ecrire valeur_ecrite  =
  let _ = (doit_ecrire,valeur_ecrite) in failwith "Word registre not implemented!"

 
let rec memoire taille_addr set l1 l2 e v =
  let _ = if false then let _ = memoire taille_addr set l1 l2 e v in () ; () in failwith "Memoire not implemented!"

let rom l1 l2 valeurs =          
  let _ = (l1,l2,valeurs) in failwith "ROM not implemented!"
  

let ram_rom taille_addr set l1 l2 e v contenu_rom =
  let _ = (taille_addr,set,l1,l2,e,v,contenu_rom) in failwith "RAM/ROM not implemented!"
       
    
  
