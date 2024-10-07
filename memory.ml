open Circuit
open Logique

let bit_registre doit_ecrire valeur_ecrite =
  let _ = (doit_ecrire,valeur_ecrite) in
  let valeur_stockee = nouvelle_tension() in
  let nouvelle_valeur = delai (mux doit_ecrire valeur_ecrite valeur_stockee) in
  relie valeur_stockee nouvelle_valeur;
  valeur_stockee
  
let word_registre doit_ecrire valeur_ecrite  =
  let _ = (doit_ecrire,valeur_ecrite) in
  let valeur_stockee = Array.init 16 (fun i -> nouvelle_tension()) in
  let nouvelle_valeur = delai (selecteur doit_ecrire valeur_ecrite valeur_stockee) in
  relie valeur_stockee nouvelle_valeur;
  valeur_stockee
 
(* 
  taill_addr: Taille des adresses (8 bits)
  set: Ecrire ?
  l1: 1e adresse de lecture
  l2: 2e adresse de lecture
  e: adresse à écrire
  v: valeur à écrire
  sortie: les 2 nombres lus
*)
let rec memoire (taille_addr: int) (set: tension) (l1: tension list) (l2: tension list)
  (e: tension list) (v: tension array): tension array * tension array =
  let _ = if false then let _ = memoire taille_addr set l1 l2 e v in () ; () in failwith "Memoire not implemented!"

let rom l1 l2 valeurs =          
  let _ = (l1,l2,valeurs) in failwith "ROM not implemented!"
  

let ram_rom taille_addr set l1 l2 e v contenu_rom =
  let _ = (taille_addr,set,l1,l2,e,v,contenu_rom) in failwith "RAM/ROM not implemented!"
       
    
  
