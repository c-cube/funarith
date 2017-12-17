
open Funarith

module Make(V : Simplex.VAR) : Simplex.S with type Q.t = Q.t and type var = V.t
module Make_pp(V : Simplex.VAR_PP) : Simplex.S_PP with type Q.t = Q.t and type var = V.t
module Make_full(V : Simplex.VAR_GEN) : Simplex.S_FULL with type Q.t = Q.t and type var = V.t
