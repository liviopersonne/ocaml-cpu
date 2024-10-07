open Logique
open Arithmetique

let alu (instruction: Circuit.tension array) (x: Circuit.tension array) (y: Circuit.tension array): Circuit.tension array = (* instruction sur 3 bits et x y de 16 bits chacun *)
  selecteur (instruction.(2)) (
    selecteur (instruction.(1)) (
      selecteur (instruction.(0)) (
        somme x y
      ) (
        inverse x
      )
    ) (
      selecteur (instruction.(0)) (
        et_logique x y
      ) (
        increment x
      )
    )
  ) (
    selecteur (instruction.(1)) (
      selecteur (instruction.(0)) (
        difference x y
      ) (
        xor_logique x y
      )
    ) (
      selecteur (instruction.(0)) (
        ou_logique x y
      ) (
        decrement x
      )
    )
  )



