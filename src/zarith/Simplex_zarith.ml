
open Funarith

module Make = Simplex.Make(Rat_zarith)
module Make_full = Simplex.Make_full(Rat_zarith)
module Make_full_for_expr = Simplex.Make_full_for_expr(Rat_zarith)
