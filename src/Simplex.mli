
(** Solving Linear systems of rational equations. *)


module type VAR = Simplex_intf.VAR
module type VAR_GEN = Simplex_intf.VAR_GEN

module type S = Simplex_intf.S
module type CONSTRAINT = Simplex_intf.CONSTRAINT

module Make(Q : Rat.S)(V : VAR) : S with module Q = Q and type var = V.t

module Make_constr(Q : Rat.S)(V : VAR_GEN)
  : CONSTRAINT
    with type var = V.t
     and module Q = Q


