open Circuit
open Libdraw
   
let _ =
  let i = Array.init 16 (fun _ -> nouvelle_tension ()) in
  let i2 = nouvelle_tension () in
  let a = Memory.word_registre i2 i in
  draw_pdf "/tmp/circuit.pdf" (Array.concat [i ; [|i2|]]) a

     
(* let _ = *)
(*   let a = nouvelle_tension () in  *)
(*   let b = nouvelle_tension () in  *)
(*   let c = nand a b in *)
(*   draw_pdf "/tmp/circuit.pdf" [|a;b|] [|c|]  *)
