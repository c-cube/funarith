
open Funarith

module Make(V : Simplex.VAR)
  : Simplex.S with type Q.t = Q.t and type var = V.t

module Make_full(F : Simplex.FRESH)
    (L: Expr.Linear.S with type C.t = Q.t and type Var.t = F.var)
  : Simplex.S_FULL with type Q.t = Q.t and type var = F.var and module L = L
