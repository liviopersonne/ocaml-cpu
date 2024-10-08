open Circuit
open Logique

let bit_registre doit_ecrire valeur_ecrite: tension =
  let valeur_stockee = nouvelle_tension() in
  let nouvelle_valeur = mux doit_ecrire valeur_stockee valeur_ecrite in
  relie valeur_stockee (delai nouvelle_valeur);
  valeur_stockee
  
let word_registre doit_ecrire valeur_ecrite: tension array =
  Array.init nb_bits (fun i -> (bit_registre doit_ecrire valeur_ecrite.(i)))
 
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
  let _ = if false then let _ = memoire taille_addr set l1 l2 e v in () ; () in
  if taille_addr = 0 then begin
    let case = word_registre set v in
    (case, case)
  end
  else match (l1, l2, e) with
  | (h1::q1, h2::q2, he::qe) -> begin
      let vg1, vg2 = memoire (taille_addr-1) (mux set set he) q1 q2 qe v in
      let vd1, vd2 = memoire (taille_addr-1) (mux set set he) q1 q2 qe v in
      (selecteur h1 vg1 vg2, selecteur h2 vd1 vd2)
  end
  | _ -> failwith "Invalid adresses"

let rom l1 l2 valeurs =          
  let _ = (l1,l2,valeurs) in failwith "ROM not implemented!"
  

let ram_rom taille_addr set l1 l2 e v contenu_rom =
  let _ = (taille_addr,set,l1,l2,e,v,contenu_rom) in failwith "RAM/ROM not implemented!"
       
    
  
