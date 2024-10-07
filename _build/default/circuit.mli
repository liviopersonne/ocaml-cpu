val nb_bits : int
type tension
val nouvelle_tension : unit -> tension
val zero : tension
val un : tension
val vide_circuit : unit -> unit
val nand : tension -> tension -> tension
val delai : tension -> tension
val relie : tension -> tension -> unit
val nouveau_peripherique :
  tension list -> tension list -> (int list -> int list) -> unit
val nouveau_peripherique_complexe :
  (tension list * tension list * (int list -> int list)) list -> unit
val compile : tension array -> tension array -> int array -> int array
val execute :
  tension array -> int array array -> tension list -> int list list
val affiche_sorties : int list list -> unit


(* For use in libdraw *)
type porte =
  ZERO | UN | NAND | DELAI | ENTREE | UNDEFINED | PERIPHERIQUE
val compile_raw :
  tension array ->
  tension array ->
  porte array * (int * int * int) list * (int * int) list * int array *
  int array * (int list * int list * int * (int list -> int list)) list

