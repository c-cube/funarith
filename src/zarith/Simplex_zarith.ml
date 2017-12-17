
open Funarith

module Make = Simplex.Make(Rat_zarith)

module Make_constr = Simplex.Make_constr(Rat_zarith)
