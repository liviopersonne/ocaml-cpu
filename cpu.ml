open Logique
open Circuit
open Arithmetique
open Memory
open Alu


let print_array (a) =
  print_string "[|";
  Array.iter (Printf.printf "%d, ") a;
  print_string "\b\b|]\n"

let incr_array (a: int array): int array = nb_to_array ((bit_liste_vers_nb (Array.to_list a)) + 1)

(* Mot de taille n constitué que de zeros *)
let zero_array (n: int): tension array = Array.init n (fun _ -> zero)
let zero_list (n: int): tension list = List.init n (fun _ -> zero)

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

let memory_read_adresses (r2: tension list) (pc: tension array) (regs: tension array array): tension list * tension list = 
  let r2_value = adress_to_register r2 regs in (* Array of size 16 *)
  (Array.to_list(Array.sub r2_value 0 9), Array.to_list(Array.sub pc 0 9))

let memory_write_adress (r1: tension array) (r2: tension list) (regs: tension array array): tension list = 
  Array.to_list (somme (adress_to_register r2 regs) (Array.concat [r1; zero_array 12]))

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
    match Array.sub (nb_to_array i) 0 4 with
    | [|a;b;c;d|] -> et (et (correspond a r1.(0)) (correspond b r1.(1))) (et (correspond c r1.(2)) (correspond d r1.(3)))
    | _ -> failwith "Unmatchable case in register_set"
  ) (
    (* opcode < 4 *)
    zero
  )
  
(* Tailles:
    - r1, r2, r3, opcode: 4
    - pc, mem1, alu_x, alu_y: 16
    - sortie: 16
*)
let register_value (mem1: tension array) (pc: tension array) (opcode: tension array) (r1: tension array) 
(r2: tension array) (r3: tension array) (alu_x: tension array) (alu_y: tension array): tension array =
  let _ = r1 in (* TODO: remove *)
  let zero_word = zero_array 16 in
  let zero_half = zero_array 8 in
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
            somme pc (Array.concat [r3; r2; zero_half])
          ) (
            (* Opcode = 5 *)
            mem1
          )
        ) (
          selecteur a (
            (* Opcode = 6 *)
            (* TODO: Corriger ici pour ne pas modifier les autres bits (de zero_half) *)
            Array.concat [r3; r2; zero_half]
          ) (
            (* Opcode = 7 *)
            (* TODO: Corriger ici pour ne pas modifier les autres bits (de zero_half) *)
            Array.concat [zero_half; r3; r2]
          )
        )
      )
    ) (
      (* Opcode >= 8: instruction ALU *)
      alu [|a;b;c|] alu_x alu_y
    )
  end
  | _ -> failwith "Unmatchable case in register_value"

(* Le registre pc est modifié ssi opcode <= 2 *)
let pc_jump (opcode: tension array) (r1: tension array) (r2: tension list) (regs: tension array array): tension = 
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
          mux r1.(2) (
            (* r1[1] = 0 *)
            ou (est_negatif r2_value) (est_nul r2_value)
          ) (
            (* r1[1] = 1 *)
            et (est_positif r2_value) (neg (est_nul r2_value))
          )
        ) (
          (* opcode = 1 *)
          mux r1.(2) (
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
  | _ -> failwith "Unmatchable case in pc_set"
  (* TODO ajouter l'incrémentation de pc *)

let pc_value (pc: tension array) (jump: tension) (opcode: tension array) (r1: tension array) (r2: tension array) (r3: tension array) (regs: tension array array): tension array =
  match opcode with
  | [|_;b;_;_|] -> begin
    selecteur (jump) (
      (* Il n'y a pas de saut *)
      increment pc
    ) (
      (* Il y a un saut (opcode < 3) *)
      selecteur b (
        (* opcode = 0 ou 1 *)
        selecteur r1.(3) (
          (* r1[0] = 0 *)
          (* TODO: Ca peut être sub r1 2 2 selon le sens de r1 *)
          (* TODO: Vérifier ordre *)
          somme pc (Array.concat [r3; Array.sub r1 0 2; zero_array 10])
        ) (
          (* r1[0] = 1 *)
          adress_to_register (Array.to_list r3) regs
        )
      ) (
        (* opcode = 2 *)
        (* TODO: Ca peut être sub r1 2 2 selon le sens de r1 *)
        somme pc (Array.concat [r3; r2; Array.sub r1 0 2; zero_array 6])
      )
    )
    
  end
  | _ -> failwith "Unmatchable case in pc_value"





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
let cpu (program: tension array array): int array * int array * int array * int array =
  let program_length = Array.length program in
  assert(program_length <= 256);  (* Check that the program fits in the rom *)
  let program_256 = Array.init 256 (fun i -> if i < program_length then program.(i) else zero_array 16) in
  

  (* Inputs *)
  let pc_init = Array.init 16 (fun _ -> nouvelle_tension()) in
  let regs_init = Array.init 16 (fun _ -> Array.init 16 (fun _ -> nouvelle_tension())) in
  let input_init = Array.init 16 (fun _ -> nouvelle_tension()) in
  let opcode = Array.sub input_init 0 4 in
  let r1_array = Array.sub input_init 4 4 in
  let r2_array = Array.sub input_init 8 4 in
  let r3_array = Array.sub input_init 12 4 in
  let r1_list = Array.to_list (r1_array) in
  let r2_list = Array.to_list (r2_array) in
  let r3_list = Array.to_list (r3_array) in



  (* Registers *)
  let alu_x_init = Array.init nb_bits (fun _ -> nouvelle_tension()) in
  let alu_y_init = Array.init nb_bits (fun _ -> nouvelle_tension()) in
  let mem_init = Array.init nb_bits (fun _ -> nouvelle_tension()) in
  let regs_set = Array.init 16 (fun i -> register_set i opcode r1_array) in
  let regs_value = Array.init 16 (fun _ -> register_value mem_init pc_init opcode r1_array r2_array r3_array alu_x_init alu_y_init) in
  let regs = Array.init 16 (fun i -> word_registre regs_set.(i) regs_value.(i)) in
  let pc_jump = pc_jump opcode r1_array r2_list regs in
  let pc_out = pc_value pc_init pc_jump opcode r1_array r2_array r3_array regs in


  (* ALU entries *)
  let alu_x, alu_y = alu_entries r2_list r3_list regs in

  (* Memory entries *)
  let mem_set = memory_set opcode in
  let mem_l1, mem_l2 = memory_read_adresses r2_list pc_init regs in
  let mem_e = memory_write_adress r1_array r2_list regs in
  let mem_v = memory_write_value r3_list regs in
  
  (* Memory and ALU *)
  (* On fait une lecture en mémoire à gauche et une lecture d'instruction à droite *)
  let mem, input = ram_rom mem_set mem_l1 mem_l2 mem_e mem_v program_256 in

  (* On relie les fils *)
  (* relie_array pc pc_init; *)
  Array.iter2 (fun x y -> relie_array x y) regs regs_init;
  relie_array alu_x alu_x_init;
  relie_array alu_y alu_y_init;
  relie_array mem mem_init;
  relie_array input input_init;

  (* Unused variables *)
  let _ = r1_list, regs, regs_init, alu_x, alu_y, alu_x_init, alu_y_init, mem, mem_init, input, input_init in
  
  
  (* Compile entrées sorties *)
  let pc_input = ref (Array.make 16 0) in
  let pc_output = ref (Array.make 16 0) in
  let rep = ref (Array.make 28 0) in
  let opcode_output = ref (Array.make 4 0) in
  let r2_output = ref (Array.make 16 0) in
  let r3_output = ref (Array.make 16 0) in

  while (bit_liste_vers_nb (Array.to_list !pc_input) < 256) do
    (* Bit array to number *)
    let atn (a: int array): int = bit_liste_vers_nb (Array.to_list a) in
    Printf.printf "pc: %d\n" (atn !pc_input);
    rep := compile pc_init (Array.concat [pc_out; opcode; adress_to_register r2_list regs; adress_to_register r3_list regs]) !pc_input;
    pc_output := Array.sub !rep 0 16;
    opcode_output := Array.sub !rep 16 4;
    r2_output := Array.sub !rep 20 16;
    r3_output := Array.sub !rep 36 16;
    Printf.printf "pc: %d, opcode: %d, r2: %d, r3: %d\n" (atn !pc_output) (atn !opcode_output) (atn !r2_output) (atn !r3_output);
    if (Array.for_all2 (=) !pc_input !pc_output) then  (* Not a jump instruction *)
      pc_input := (incr_array !pc_output)
    else  (* Jump instruction *)
      pc_input := !pc_output
  done;

  (!pc_output, !opcode_output, !r2_output, !r3_output)
      
