
open Funarith

module Make(V : Simplex.VAR) : Simplex.S with type Q.t = Q.t and type var = V.t

module Make_constr(V : Simplex.VAR_GEN) : Simplex.CONSTRAINT with type Q.t = Q.t and type var = V.t
