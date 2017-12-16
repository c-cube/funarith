
open Funarith

module Make(V : Simplex.Var) : Simplex.S with type Q.t = Q.t and type var = V.t
