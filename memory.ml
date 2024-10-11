open Circuit
open Logique
open Arithmetique

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
  if taille_addr = 0 then begin
    let case = word_registre set v in
    (case, case)
  end
  else match (l1, l2, e) with
  | (h1::q1, h2::q2, he::qe) -> begin
      let vg1, vg2 = memoire (taille_addr-1) (mux set set (neg he)) q1 q2 qe v in
      let vd1, vd2 = memoire (taille_addr-1) (mux set set he) q1 q2 qe v in
      (selecteur h1 vg1 vd1, selecteur h2 vg2 vd2)
  end
  | _ -> failwith "Invalid adresses"


let rom (l1: tension list) (l2: tension list) 
(valeurs: tension array array): tension array * tension array =
  let nb_valeurs = Array.length valeurs in
  let log2 = log(Float.of_int nb_valeurs) /. log(2.) in
  if not (Float.is_integer log2) then
    failwith "Le nombre de valeurs n'est pas une puissance de 2";
  let taille_addr_rom = Int.of_float log2 in
  let rec construit_rom (taille_addr: int) (rl1: tension list) (rl2: tension list) (addr: int list): tension array * tension array =
    if taille_addr = 0 then let v = valeurs.(bit_liste_vers_nb (List.rev addr)) in (v, v)
    else match (rl1, rl2) with
    | (h1::q1, h2::q2) -> begin
      let vg1, vg2 = construit_rom (taille_addr-1) q1 q2 (0::addr) in
      let vd1, vd2 = construit_rom (taille_addr-1) q1 q2 (1::addr) in
      (selecteur h1 vg1 vd1, selecteur h2 vg2 vd2)
    end
    | _ -> failwith "Invalid adresses"
  in construit_rom taille_addr_rom l1 l2 []
  


(* let ram_rom (set: tension) (l1: tension list) (l2: tension list)
(e: tension list) (v: tension array) (contenu_rom: tension array array): tension array * tension array =
  let taille_rom = Array.length contenu_rom in
  if not (taille_rom = 256) then
    failwith "La rom ne fait pas la bonne taille";

  let rec construit_ram_rom (taille_addr: int) (set: tension) (rl1: tension list) (rl2: tension list) (addr: int list) (e: tension list)
  (v: tension array): tension array * tension array =
    if taille_addr = 0 then begin
      let adresse = bit_liste_vers_nb (List.rev addr) in
      (* let case = rom_word_registre set v v_rom in
      (case, case) *)
      ([||], [||])
    end else match (rl1, rl2, e) with
    | (h1::q1, h2::q2, he::qe) -> begin
      let vg1, vg2 = construit_ram_rom (taille_addr-1) (mux set set (neg he)) q1 q2 (0::addr) qe v in
      let vd1, vd2 = construit_ram_rom (taille_addr-1) (mux set set he) q1 q2 (1::addr) qe v in
      (selecteur h1 vg1 vd1, selecteur h2 vg2 vd2)
    end
    | _ -> failwith "Invalid adresses"
  in construit_ram_rom 8 set l1 l2 [] e v *)

let ram_rom (set: tension) (l1: tension list) (l2: tension list)
(e: tension list) (v: tension array) (contenu_rom: tension array array): tension array * tension array =
  match (l1, l2) with
  | h1::q1, h2::q2 -> begin
    let ramg, ramd = memoire 7 set q1 q2 e v in
    let romg, romd = rom q1 q2 contenu_rom in
    (selecteur h1 romg ramg, selecteur h2 romd ramd)
  end
  | _ -> failwith "Format des adresses incorrect"
       
    
  
