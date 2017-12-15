
(** Solving Linear systems of rational equations. *)

module type S = Simplex_intf.S
module type Var = Simplex_intf.Var

module Make(Q : Rat.S)(V : Var) : S with module Q = Q and type var = V.t
