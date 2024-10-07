let nb_bits = 16 
type tension = ((unit -> unit) * int )
let meme_tension = ref []
let portes_nand = ref []
let portes_delai = ref []
let peripheriques = ref []
let nb_tensions = ref 0

let nouvelle_tension : unit -> tension =
  fun () -> (incr nb_tensions ; ( (fun ()->()), !nb_tensions-1))
  
let zero = nouvelle_tension ()
let un = nouvelle_tension ()

(* let get_ram () = *)
(*   let adr_lue_1 = Array.init nb_bits (fun () -> nouvelle_tension()) in *)
(*   let adr_lue_2 = Array.init nb_bits (fun () -> nouvelle_tension()) in *)
(*   let adr_ecrite = Array.init nb_bits (fun () -> nouvelle_tension()) in *)
(*   let v_ecrite = Array.init nb_bits (fun () -> nouvelle_tension()) in *)
(*   let v_lue_1 = Array.init nb_bits (fun () -> nouvelle_tension()) in *)
(*   let v_lue_2 = Array.init nb_bits (fun () -> nouvelle_tension()) in *)
(*   let ecrit = nouvelle_tension () in *)
(*   (adr_lue_1,adr_lue_2,adr_ecrite,v_ecrite,ecrit,v_lue_1,v_lue_2) *)
            
let vide_circuit () =
  nb_tensions := 2 ;  (* 3+6*nb_bits ; *)
  assert(snd zero=0) ;
  assert(snd un=1) ;
  (* let _,_,_,_,e,_,_,_ = get_ram() in *)
  (* assert(e+1=!nb_tensions) ; *)
  portes_nand := [] ;
  portes_delai := [] ;
  meme_tension := [] ;
  peripheriques := [] 
  

let nand (a:tension) (b:tension) : tension =
  let c = nouvelle_tension () in
  portes_nand := (snd a,snd b,snd c)::!portes_nand ;
  c

let delai (a:tension) : tension =
  let c = nouvelle_tension () in
  portes_delai := (snd a,snd c)::!portes_delai ;
  c
  
let relie (a:tension) (b:tension) =
  meme_tension := (snd a,snd b)::!meme_tension 

let nouveau_peripherique (entrees : tension list) (sorties : tension list) calcule =
  peripheriques := (List.map snd entrees,
                    List.map snd sorties,
                    snd (nouvelle_tension ()),
                    calcule) :: !peripheriques


let rec nouveau_peripherique_complexe ioclist = match ioclist with
  | [] -> ()
  | [i,o,c] -> nouveau_peripherique i o c
  | (i,o,c)::(i2,o2,c2)::r ->
     let t = nouvelle_tension () in 
     let _ = nouveau_peripherique i (t::o) (fun l -> 0::c l) in
     nouveau_peripherique_complexe ((t::i2,o2, (fun l -> c2 (List.tl l)))::r)
    
  
type porte =
  ZERO | UN | NAND | DELAI | ENTREE | UNDEFINED | PERIPHERIQUE

let compacte_cc entrees sorties =

  (* On fait un algo de union-find sur les points de tensions pour
  trouver les points tensions vraiment différents *)

  let repr = Array.make (!nb_tensions) (-1) in
  let rec find x =
    if repr.(x) < 0
    then x
    else (repr.(x) <- find repr.(x) ; repr.(x))
  in
  let unite (a,b) =
    let a = find a and b = find b in
    let a,b = if repr.(a) < repr.(b) then (a,b) else (b,a) in
    repr.(a) <- repr.(a)+repr.(b) ;
    repr.(b) <- a 
  in
  List.iter unite (!meme_tension) ;

  (* Ensuite on fait une correspondance entre un point de tension et
  un numéro de composante connexe *)
  let nb_cc = ref 0 in

  let cc_de = Array.make (!nb_tensions) (-1) in
  for i = 0 to !nb_tensions-1 do
    if repr.(i) < 0
    then
      begin
        cc_de.(i) <- !nb_cc ;
        incr nb_cc ;
      end
  done ;
  for i = 0 to !nb_tensions-1 do
    cc_de.(i) <- cc_de.(find i) ;  
  done ;
  
  let portes_nand = List.rev_map (fun (a,b,c) -> cc_de.(a),cc_de.(b),cc_de.(c)) (!portes_nand) in
  let portes_delai = List.rev_map (fun (a,b) -> cc_de.(a),cc_de.(b)) (!portes_delai) in
  let entrees = Array.map (fun a -> cc_de.(a)) entrees in
  let sorties = Array.map (fun a -> cc_de.(a)) sorties in
  let do_periph (entrees,sorties, tension,calcule) =
    let f = List.map (fun e -> cc_de.(e)) in
    (f entrees, f sorties, cc_de.(tension), calcule)
  in
  let peripheriques = List.map do_periph (!peripheriques) in
  (!nb_cc,zero,un,portes_nand,portes_delai,entrees,sorties, peripheriques)

let print_circuit () =
  List.iter (fun (a,b,c) -> print_int c ; print_string " = NAND(" ; print_int a ; print_string "," ; print_int b; print_string ")\n") (!portes_nand) ;
  List.iter (fun (a,b) -> print_int b ; print_string " = DELAI(" ; print_int a; print_string ")\n") (!portes_delai) ;
  List.iter (fun (a,b) -> print_int a ; print_string " = " ; print_int b ; print_string "\n") (!meme_tension) ;
  List.iter (fun (i,o,m,_) ->
      List.iter (fun e -> print_int e ; print_string " ") o ;
      print_string " = PERIPHERIQUE(" ; print_int m ; print_string ")" ;
      List.iter (fun e -> print_int e ; print_string " ") i ;
      print_string "\n") (!peripheriques)

  
  
let type_points entrees nb_points portes_nand portes_delai peripheriques =
  (* On vérifie que chaque point de tension correspond à exactement un de ces critères :
     - c'est zero ou un 
     - c'est la sortie d'une unique porte NAND 
     - c'est la sortie d'un délai
     - c'est une entrée
   *)
  let type_cc = Array.make nb_points UNDEFINED in
  let type_str = function
    | UNDEFINED -> "UNDEFINED"
    | NAND -> "NAND"
    | ZERO -> "0"
    | PERIPHERIQUE -> "PERIPHERIQUE"
    | UN -> "1"
    | DELAI -> "DELAI"
    | ENTREE -> "ENTREE"
  in
  let assigne i t =
    match type_cc.(i) with
    | UNDEFINED ->  type_cc.(i) <- t
    | t' ->
       print_circuit () ;
       failwith ("La tension "^(string_of_int i)^" est définie deux fois, comme "^(type_str t)^" et comme "^(type_str t'))
  in
  assigne (snd zero) ZERO ;
  assigne (snd un) UN ;
  List.iter (fun (_,_,c) -> assigne c NAND) portes_nand ;
  List.iter (fun (_,b) -> assigne b DELAI) portes_delai ;
  List.iter (fun (_,_,t,_) -> assigne t PERIPHERIQUE) peripheriques ;
  Array.iter (fun a -> assigne a ENTREE) entrees ;
  type_cc

let tri_topologique vient_apres =
  let rang = Array.map (fun _ -> -2) vient_apres in
  let index = Array.map (fun _ -> -2) vient_apres in
  let nb_places = ref (Array.length vient_apres) in
  
  let rec libere noeud =
    if rang.(noeud) = -1
    then failwith "Il n'existe pas d'ordre valable pour le circuit !" ;

    if rang.(noeud) < 0
    then
      begin
        rang.(noeud) <- -1 ;
        List.iter libere vient_apres.(noeud) ;
        decr nb_places ;
        rang.(noeud) <- !nb_places ;
        index.(!nb_places) <- noeud ;
      end
  in
  for i = 0 to Array.length vient_apres - 1 do
    libere i
  done ;
  (rang,index)

let compile_raw (entrees : tension array) (sorties : tension array) =
  let entrees = Array.map snd entrees in
  let sorties = Array.map snd sorties in
  let nb_points, _zero, _un, portes_nand, portes_delai, entrees, sorties, peripheriques = compacte_cc entrees sorties in
  let types = type_points entrees nb_points portes_nand portes_delai peripheriques in
  (types,portes_nand,portes_delai,entrees,sorties, peripheriques)
  
let compile (entrees : tension array) (sorties : tension array) =
  let entrees = Array.map snd entrees in
  let sorties = Array.map snd sorties in
  let nb_points, zero, un, portes_nand, portes_delai, entrees, sorties, peripheriques = compacte_cc entrees sorties in
  
  let types = type_points entrees nb_points portes_nand portes_delai peripheriques in

  (* Ensuite on vérifie qu'il existe un ordre sur les portes nand (si
    c=NAND(a,b) alors la CC de c doit être placée après celle de a et
    celle de b dans l'ordre que l'on créé. *)
  let vient_apres = Array.make nb_points [] in
  List.iter (fun (a,b,c) ->
      vient_apres.(a) <- c::vient_apres.(a) ;
      vient_apres.(b) <- c::vient_apres.(b)) portes_nand ;

  (* Pour chaque peripherique les entrées arrivent avant les sorties.
     On en profite pour stocker la manière de calculer chaque périphérique
     dans une table de hash *)
  let peripheriques_de = Hashtbl.create 17 in
  List.iter (fun (entrees,sorties,tension,calc) ->
    List.iter (fun ent -> vient_apres.(ent) <- tension::vient_apres.(ent)) entrees ;
    List.iter (fun sor -> vient_apres.(tension) <- sor::vient_apres.(tension)) sorties ;
    Hashtbl.add peripheriques_de tension (entrees,sorties,calc)
    ) peripheriques ;
  
  (* On affecte le tri *)
  let rang,_ = tri_topologique vient_apres in
  let entrees_portes_nand = Array.make nb_points (-1,-1) in
  let _ = List.iter (fun (a,b,c) -> entrees_portes_nand.(c)<- (a,b)) portes_nand in
  let portes_ordonnees = List.sort (fun a b -> rang.(a)-rang.(b)) (List.init nb_points (fun i->i) ) in 
  
  (*Enfin on peut simuler le circuit *)
  let valeurs_precedentes = Array.make nb_points 0 in
  let maj valeurs_entrees =
    let valeurs = Array.make nb_points 2 in
    valeurs.(snd zero) <- 0 ;
    valeurs.(snd un) <- 1 ;
    (* Gère les portes delai *)
    List.iter (fun (a,b) -> valeurs.(b) <- valeurs_precedentes.(a)) portes_delai ;
    (* Gère les entrées *)
    for i = 0 to Array.length entrees - 1 do
      valeurs.(entrees.(i)) <- valeurs_entrees.(i) ;
    done ;
    (* Gère les portes dans l'ordre topo *)
    List.iter (fun c ->
        match types.(c) with
        | NAND ->
           let a,b = entrees_portes_nand.(c) in
           valeurs.(c) <- (1-(valeurs.(a)*valeurs.(b)))
        | PERIPHERIQUE ->
           let entrees,sorties,calc = Hashtbl.find peripheriques_de c in
           let v_entrees = List.map (fun p -> valeurs.(p)) entrees in
           let v_sorties = calc v_entrees in
           List.iter2 (fun p v -> valeurs.(p) <- v) sorties v_sorties 
        | _ -> ()
      ) portes_ordonnees ;
    (* Gère les portes DELAI *)
    
    Array.iteri (fun i v -> valeurs_precedentes.(i) <- v) valeurs ;
    ((Array.map (fun i->valeurs.(i)) sorties))
    
  in
  vide_circuit () ;
  maj


let execute entrees valeurs sorties =
  let maj = compile entrees (Array.of_list sorties) in
  Array.to_list valeurs |> List.map maj |> List.map Array.to_list
  

let affiche_sorties res =
  List.iter (fun t ->
      List.iter (fun i->print_int i ; print_string " " ) t ;
      print_newline()) res
