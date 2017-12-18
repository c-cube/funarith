
(** Solving Linear systems of rational equations. *)


module type VAR = Simplex_intf.VAR
module type VAR_GEN = Simplex_intf.VAR_GEN

module type S = Simplex_intf.S
module type S_FULL = Simplex_intf.S_FULL

(** Low level simplex interface *)
module Make(Q : Rat.S)(V : VAR) : S with module Q = Q and type var = V.t

(** Full version of the simplex, with a high level API *)
module Make_full(Q : Rat.S)(V : VAR_GEN)
  : S_FULL
    with type var = V.t
     and module Q = Q


