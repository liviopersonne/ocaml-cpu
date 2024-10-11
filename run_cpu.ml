(* open Cpu
open Circuit
open Arithmetique *)

   
let ins_add x y dst =  8+16*(dst+16*(x+16*y))
let ins_moins x y dst =  12+16*(dst+16*(x+16*y))
let ins_et x y dst =  10+16*(dst+16*(x+16*y))
let ins_ou x y dst =  14+16*(dst+16*(x+16*y))
let ins_inv x dst =  9+16*(dst+16*(x+16*0))
let ins_xor x y dst =  13+16*(dst+16*(x+16*y))
let ins_inc x dst =  11+16*(dst+16*(x+16*0))
let ins_dec x dst =  15+16*(dst+16*(x+16*0))
let ins_cst_hi valeur dst = 7+16*(dst+16*(valeur))
let ins_cst_low valeur dst = 6+16*(dst+16*(valeur))
let ins_load reg_adr delta dst =5+16*(dst+16*(reg_adr+16*(if delta < 0 then delta+16 else delta)))
let ins_get_pc dst delta = 4 + 16*(dst+16*(if delta < 0 then delta+256 else delta))
let ins_store reg_adr delta reg_val =3+16*((if delta < 0 then delta+16 else delta)+16*(reg_adr+16*reg_val))
let ins_jump_short delta = 2+64*(delta)
let ins_jump_short_relatif delta = 2+16+64*(if delta < 0 then delta+2048 else delta)
let ins_jump_negatif_relatif reg_cond delta =
  let delta = if delta < 0 then delta+128 else delta in
  1+16+32*(delta mod 8 + 8*(reg_cond+16*(delta/8)))
let ins_jump_est_zero_relatif reg_cond delta =
  let delta = if delta < 0 then delta+64 else delta in
  0+16+64*(delta mod 4 + 4*(reg_cond+16*(delta/4)))
let ins_jump_nonzero_relatif reg_cond delta =
  let delta = if delta < 0 then delta+64 else delta in
  0+16+32+64*(delta mod 4 + 4*(reg_cond+16*(delta/4)))
