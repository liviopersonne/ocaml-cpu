open Circuit
open Logique
open Arithmetique

(* instruction sur 3 bits et x y de 16 bits chacun *)
let alu (instruction: tension array) (x: tension array) (y: tension array): tension array =
  selecteur (instruction.(2)) (
    selecteur (instruction.(1)) (
      selecteur (instruction.(0)) (
        (* instruction = 0 *)
        somme x y
      ) (
        (* instruction = 1 *)
        inverse x
      )
    ) (
      selecteur (instruction.(0)) (
        (* instruction = 2 *)
        et_logique x y
      ) (
        (* instruction = 3 *)
        increment x
      )
    )
  ) (
    selecteur (instruction.(1)) (
      selecteur (instruction.(0)) (
        (* instruction = 4 *)
        difference x y
      ) (
        (* instruction = 5 *)
        xor_logique x y
      )
    ) (
      selecteur (instruction.(0)) (
        (* instruction = 6 *)
        ou_logique x y
      ) (
        (* instruction = 7 *)
        decrement x
      )
    )
  )



