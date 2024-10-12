open Logique
open Circuit
open Arithmetique
open Memory
open Alu

let print_array (a) =
  print_string "[|";
  Array.iter (Printf.printf "%d, ") a;
  print_string "\b\b|]\n"

let relie_liste (l1: tension list) (l2: tension list) = List.iter2 (fun x y -> relie x y) l1 l2
let relie_array (l1: tension array) (l2: tension array): unit = Array.iter2 (fun x y -> relie x y) l1 l2


(* La mémoire doit être modifiée que si opcode = 3 *)
let memory_set opcode: tension = 
  let trois = [|un; un; zero; zero|] in
  est_nul (difference trois opcode)

let adress_to_register (adress: tension list) (regs: tension array array): tension array = 
  let rec aux adress (cmpt: int list): tension array = match adress with
    | [] -> regs.(bit_liste_vers_nb (List.rev cmpt))
    | h::q -> selecteur h (aux q (0::cmpt)) (aux q (1::cmpt))
  in aux adress []

let alu_entries (r2: tension list) (r3: tension list) (regs: tension array array): tension array * tension array = 
  (adress_to_register r2 regs, adress_to_register r3 regs)

let memory_read_adresses (r2: tension list) (regs: tension array array): tension list * tension list = 
  let r2_value = adress_to_register r2 regs in (* Array of size 16 *)
  let zero_word = List.init nb_bits (fun _ -> zero) in
  (Array.to_list(Array.sub r2_value 0 8), zero_word)

let memory_write_adress (r1: tension array) (r2: tension list) (regs: tension array array): tension list = 
  Array.to_list (somme (adress_to_register r2 regs) r1)

let memory_write_value (r3: tension list) (regs: tension array array): tension array =
  (* On suppose que opcode = 3 *)
  adress_to_register r3 regs

(* Un registre est modifié ssi opcode >= 4 et i = R1 *)
let register_set (i: int) (opcode: tension array) (r1: tension array): tension = 
  let quatre = [|zero; zero; un; zero|] in
  let correspond (n: int) (t: tension): tension =
    if n = 0 then neg t else t in
  mux (est_positif(difference opcode quatre)) (
    (* opcode >= 4 *)
    match nb_to_array i with
    | [|a;b;c;d|] -> et (et (correspond a r1.(0)) (correspond b r1.(1))) (et (correspond c r1.(2)) (correspond d r1.(3)))
    | _ -> failwith "Unmatchable case"
  ) (
    (* opcode < 4 *)
    zero
  )

let register_value (mem1: tension array) (pc: tension array) (opcode: tension array) (r1: tension array) 
(r2: tension array) (r3: tension array) (alu_x: tension array) (alu_y: tension array): tension array =
  let zero_word = [|zero; zero; zero; zero; zero; zero; zero; zero|] in
  match opcode with
  | [|a;b;c;d|] -> begin
    selecteur d (
      selecteur c (
        (* Opcode < 4: instruction pc ou ram *)
        zero_word
      ) (
        selecteur b (
          selecteur a (
            (* Opcode = 4 *)
            somme pc (Array.append r1 r2)
          ) (
            (* Opcode = 5 *)
            mem1
          )
        ) (
          (* Opcode = 6 ou 7 *)
          Array.append r2 r3
        )
      )
    ) (
      (* Opcode >= 8: instruction ALU *)
      alu [|a;b;c|] alu_x alu_y
    )
  end
  | _ -> failwith "Unmatchable case"

(* Le registre pc est modifié ssi opcode <= 2 *)
let pc_set (opcode: tension array) (r1: tension array) (r2: tension list) (regs: tension array array): tension = 
  let deux = [|zero; un; zero; zero|] in
  let r2_value = adress_to_register r2 regs in
  match opcode with
  | [|a;b;_;_|] -> begin
    mux (est_positif (difference deux opcode)) (
      (* Opcode opcode > 2 *)
      zero
    ) (
      mux b (
        mux a (
          (* opcode = 0 *)
          mux r1.(1) (
            (* r1[1] = 0 *)
            ou (est_negatif r2_value) (est_nul r2_value)
          ) (
            (* r1[1] = 1 *)
            et (est_positif r2_value) (neg (est_nul r2_value))
          )
        ) (
          (* opcode = 1 *)
          mux r1.(1) (
            (* r1[1] = 0 *)
            est_nul r2_value
          ) (
            (* r1[1] = 1 *)
            neg (est_nul r2_value)
          )
        )
      ) (
          (* opcode = 2 *)
          un
      )
    )
    end
  | _ -> failwith "Unmatchable case"
  (* TODO ajouter l'incrémentation de pc *)

let pc_value (pc: tension array) (opcode: tension array) (r1: tension array) (r2: tension array) (r3: tension array) (regs: tension array array): tension array =
  match opcode with
  | [|_;b;_;_|] -> begin
    (* On suppose que opcode < 3 *)
    selecteur b (
        (* opcode = 2 *)
        somme pc (Array.concat [Array.sub r1 3 2;r3])
    ) (
      (* opcode = 0 ou 1 *)
      selecteur r1.(0) (
        (* r1[0] = 0 *)
        somme pc (Array.concat [Array.sub r1 3 2;r2;r3])
      ) (
        (* r1[0] = 1 *)
        adress_to_register (Array.to_list r3) regs
      )
    )
  end
  | _ -> failwith "Unmatchable case"





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
  assert(Array.length program <= 256);  (* Check that the program fits in the rom *)
  
  (* Inputs *)
  let input = Array.init 16 (fun _ -> nouvelle_tension()) in
  let opcode = Array.sub input 0 4 in
  let r1_array = Array.sub input 4 4 in
  let r2_array = Array.sub input 8 4 in
  let r3_array = Array.sub input 12 4 in
  let r1_list = Array.to_list (r1_array) in
  let r2_list = Array.to_list (r2_array) in
  let r3_list = Array.to_list (r3_array) in
  
  (* Registers *)
  let pc_init = Array.init nb_bits (fun _ -> nouvelle_tension()) in
  let alu_x_init = Array.init nb_bits (fun _ -> nouvelle_tension()) in
  let alu_y_init = Array.init nb_bits (fun _ -> nouvelle_tension()) in
  let mem1_init = Array.init nb_bits (fun _ -> nouvelle_tension()) in
  let regs_set = Array.init 16 (fun i -> register_set i opcode r1_array) in
  let regs_value = Array.init 16 (fun _ -> register_value mem1_init pc_init opcode r1_array r2_array r3_array alu_x_init alu_y_init) in
  let regs = Array.init 16 (fun i -> word_registre regs_set.(i) regs_value.(i)) in
  let pc_set = pc_set opcode r1_array r2_list regs in
  let pc_value = pc_value pc_init opcode r1_array r2_array r3_array regs in
  let pc = word_registre pc_set pc_value in
  relie_array pc pc_init;

  (* ALU entries *)
  let alu_x, alu_y = alu_entries r2_list r3_list regs in
  relie_array alu_x alu_x_init;
  relie_array alu_y alu_y_init;


  (* Memory entries *)
  let mem_set = memory_set opcode in
  let mem_l1, mem_l2 = memory_read_adresses r2_list regs in
  let mem_e = memory_write_adress r1_array r2_list regs in
  let mem_v = memory_write_value r3_list regs in
  
  (* Memory and ALU *)
  let mem1, mem2 = ram_rom mem_set mem_l1 mem_l2 mem_e mem_v program in
  relie_array mem1 mem1_init;

  (* Unused variables *)
  let _ = r1_list, mem2 in


  (* TODO: Initialize pc to 0 *)
  (* TODO: Excute code *)

  let entree = [|0;0;0;0;0;0;0;0|] in
  let rep = compile pc_value pc entree in
  print_array rep;
  (pc, opcode, adress_to_register r2_list regs, adress_to_register r3_list regs)


let _ = cpu [||]
      
