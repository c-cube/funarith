
open Funarith

module Make(V : Simplex.Var) = Simplex.Make(Rat_zarith)(V)
