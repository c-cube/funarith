
(** Solving Linear systems of rational equations. *)

module type VAR = Expr_intf.VAR
module type FRESH = Expr_intf.FRESH

module type S = Simplex_intf.S
module type S_FULL = Simplex_intf.S_FULL

(** Low level simplex interface *)
module Make(Q : Rat.S)(V : VAR) :
  S with module Q = Q and type var = V.t

(** High-level simplex interface *)
module Make_full(Q : Rat.S)(F : FRESH)
    (L: Expr.Linear.S with type C.t = Q.t and type Var.t = F.var) :
  S_FULL with module Q = Q and type var = F.var and module L = L
