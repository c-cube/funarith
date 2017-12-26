
(** Solving Linear systems of rational equations. *)

module type VAR = Linear_expr_intf.VAR
module type FRESH = Linear_expr_intf.FRESH
module type VAR_GEN = Linear_expr_intf.VAR_GEN

module type S = Simplex_intf.S
module type S_FULL = Simplex_intf.S_FULL

(** Low level simplex interface *)
module Make(Q : Rat.S)(V : VAR) :
  S with module Q = Q
     and type var = V.t
     and module Var_map = CCMap.Make(V)

(** High-level simplex interface *)
module Make_full(Q : Rat.S)(V : VAR_GEN)
    (L : Linear_expr.S with type Var.t = V.t and type C.t = Q.t)
  : S_FULL with module Q = Q
            and type var = V.t
            and module L = L
            and module Var_map = L.Var_map
