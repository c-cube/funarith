
(** Solving Diophantine equations, and systems of Diophantine equations.

    We follow "Linear Diophantine Equations", S. Contejean.
*)

module type INT = Diophantine_intf.INT

module type S = Diophantine_intf.S

module Make(Int : INT) : S with module Int = Int
