open Circuit
open Arithmetique
open Memory 
open Cpu
open Run_cpu

let _ = vers_bus (bit_registre (nouvelle_tension()) (nouvelle_tension()))

let cpu_instructions_to_program (instructions: int array): tension array array =
  let int_to_tension_array (i: int) =
    let a = nb_to_array i in
    Array.map (fun b -> if b == 1 then un else zero) a
  in Array.map int_to_tension_array instructions


let main() =
  let instructions = [|
    ins_add 1 1 1;
  |] in

  let pc, opcode, r2, r3 = cpu (cpu_instructions_to_program instructions) in
  let _ = pc, opcode, r2, r3 in
  print_array pc;
  0

let _ = main()