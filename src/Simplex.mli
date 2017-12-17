
(** Solving Linear systems of rational equations. *)


module type VAR = Simplex_intf.VAR
module type VAR_PP = Simplex_intf.VAR_PP
module type VAR_GEN = Simplex_intf.VAR_GEN

module type S = Simplex_intf.S
module type S_PP = Simplex_intf.S_PP
module type S_FULL = Simplex_intf.S_FULL

module Make(Q : Rat.S)(V : VAR) : S with module Q = Q and type var = V.t

module Make_pp(Q : Rat.S)(V : VAR_PP) : S_PP with module Q = Q and type var = V.t

module Make_full(Q : Rat.S)(V : VAR_GEN)
  : S_FULL
    with type var = V.t
     and module Q = Q


