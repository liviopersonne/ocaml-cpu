open Logique
open Circuit
open Arithmetique
open Memory
open Alu

let _ = vers_bus (bit_registre (nouvelle_tension()) (nouvelle_tension()))


let print_array (a) =
  print_string "[|";
  Array.iter (Printf.printf "%d, ") a;
  print_string "\b\b|]\n"

let register_init (taille: int): tension * tension array * tension array =
  let set = nouvelle_tension() in
  let input = Array.init taille (fun _ -> nouvelle_tension ()) in
  let register = word_registre set input in
  (set, input, register)

let rec relie_liste (l1: tension list) (l2: tension list) =
  match (l1, l2) with
  | h1::q1, h2::q2 -> relie h1 h2; relie_liste q1 q2
  | _ -> failwith "Listes de longueusr différentes"


(* La mémoire doit être modifiée que si opcode = 3 *)
let memory_set opcode: tension = match opcode with
  | [|a;b;c;d|] -> et (et (a) (b)) (et (neg c) (neg d))  (* Teste si opcode = 3 *)
  | _ -> failwith "Unmatchable case"

let alu_instruction opcode: tension array = match opcode with
  | [|a;b;c;_|] -> [|a;b;c|]
  | _ -> failwith "Unmatchable case"

let adress_to_register (adress: tension list) (regs: tension array array): tension array = 
  let rec aux adress (cmpt: int list): tension array = match adress with
    | [] -> regs.(bit_liste_vers_nb (List.rev cmpt))
    | h::q -> selecteur h (aux q (0::cmpt)) (aux q (1::cmpt))
  in aux adress []

let alu_entries (r2: tension list) (r3: tension list) (regs: tension array array): tension array * tension array = 
  (adress_to_register r2 regs, adress_to_register r3 regs)

let memory_read_adresses (r2: tension list) (r3: tension list): tension list * tension list = 
  (r2, r3)

let memory_write_adress (r1: tension array) (r2: tension list) (regs: tension array array): tension list = 
  Array.to_list (somme (adress_to_register r2 regs) r1)

let memory_write_value (r3: tension list) (regs: tension array array): tension array =
  adress_to_register r3 regs


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
  let zero_list = List.init 8 (fun _ -> zero) in
  let zero_array = Array.init 8 (fun _ -> zero) in
  
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
  let pc_set, pc_input, pc_out = register_init(8) in
  let regs_set = Array.init 16 (fun _ -> nouvelle_tension()) in
  let regs_value = Array.init 16 (fun _ -> Array.init 8 (fun _ -> nouvelle_tension())) in
  let regs = Array.init 16 (fun i -> word_registre regs_set.(i) regs_value.(i)) in


  (* Memory entries *)
  let mem_set = memory_set opcode in
  let mem_l1, mem_l2 = memory_read_adresses r2_list r3_list in
  let mem_e = memory_write_adress r1_array r2_list regs in
  let mem_v = memory_write_value r3_list regs in
  
  (* ALU entries *)
  let alu_instruction = alu_instruction opcode in
  let alu_x, alu_y = alu_entries r2_list r3_list regs in
  
  (* Memory and ALU *)
  let mem1, mem2 = ram_rom mem_set mem_l1 mem_l2 mem_e mem_v program in
  let alu_out = alu alu_instruction alu_x alu_y in





  let entree = [|0;0;0;0;0;0;0;0|] in
  let rep = compile pc_input pc_out entree in
  print_array rep;
  ([||], [||], [||], [||])


let _ = cpu [||]
      
