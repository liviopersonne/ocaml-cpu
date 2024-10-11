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
  assert(Array.length program <= 256);
  let zero_list = List.init 8 (fun _ -> zero) in
  let zero_array = Array.init 8 (fun _ -> zero) in
  let pc_set, pc_input, pc_out = register_init(8) in
  let opcode_set, opcode_input, opcode_out = register_init(4) in
  let r1_set, r1_input, r1_out = register_init(4) in
  let r2_set, r2_input, r2_out = register_init(4) in
  let r3_set, r3_input, r3_out = register_init(4) in
  
  let input = Array.init 16 (fun _ -> nouvelle_tension()) in
  let opcode = Array.sub input 0 4 in
  let r1 = Array.to_list (Array.sub input 4 4) in
  let r2 = Array.to_list (Array.sub input 8 4) in
  let r3 = Array.to_list (Array.sub input 12 4) in

  let regs_e = Array.init 16 (fun _ -> nouvelle_tension()) in
  let regs_v = Array.init 16 (fun _ -> Array.init 8 (fun _ -> nouvelle_tension())) in
  let regs = Array.init 16 (fun i -> word_registre regs_e.(i) regs_v.(i)) in

  let mem_set = nouvelle_tension() in
  let mem_l1, mem_l2 = List.init 8 (fun _ -> nouvelle_tension()), List.init 8 (fun _ -> nouvelle_tension()) in
  let mem_e, mem_v = List.init 8 (fun _ -> nouvelle_tension()), Array.init 8 (fun _ -> nouvelle_tension()) in
  let mem1, mem2 = ram_rom mem_set mem_l1 mem_l2 mem_e mem_v program in

  let alu_instruction = Array.init 3 (fun _ -> nouvelle_tension()) in
  let alu_x, alu_y = Array.init 16 (fun _ -> nouvelle_tension()), Array.init 16 (fun _ -> nouvelle_tension()) in
  let alu_out = alu alu_instruction alu_x alu_y in

  (* relie_liste mem_l1 (Array.to_list(regs.(bit_liste_vers_nb r2))); *)


  let a = selecteur opcode_out.(3) (
    (* Instruction Registres *)
    selecteur opcode_out.(2) (
      selecteur opcode_out.(1) (
        selecteur opcode_out.(0) (
          (* Opcode = 0 *)
          [||]
        ) (
          (* Opcode = 1 *)
          [||]
        )
      ) (
        selecteur opcode_out.(0) (
          (* Opcode = 2 *)
          [||]
        ) (
          (* Opcode = 3 *)
          [||]
        )
      )
    ) (
      selecteur opcode_out.(1) (
        selecteur opcode_out.(0) (
          (* Opcode = 4 *)
          [||]
        ) (
          (* Opcode = 5 *)
          [||]
        )
      ) (
        selecteur opcode_out.(0) (
          (* Opcode = 6 *)
          [||]
        ) (
          (* Opcode = 7 *)
          [||]
        )
      )
    )
  ) (
    (* Instruction ALU *)
    (* let l1, l2 = ram_rom zero r2 r3 zero_list zero_array program in
    alu (Array.sub opcode_out 0 3) l1 l2 *)

    [||]
  ) in



  let entree = [|0;0;0;0;0;0;0;0|] in
  let rep = compile pc_input pc_out entree in
  print_array rep;
  ([||], [||], [||], [||])


let _ = cpu [||]
      
