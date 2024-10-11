open Circuit
open Arithmetique
open Memory 
open Cpu

let _ = vers_bus (bit_registre (nouvelle_tension()) (nouvelle_tension()))

let main() =
  let pc, opcode, r2, r3 = cpu [||] in
  let _ = pc, opcode, r2, r3 in
  0

let _ = main()