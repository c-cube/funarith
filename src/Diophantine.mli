
(** Solving Diophantine equations, and systems of Diophantine equations.

    We follow "Linear Diophantine Equations", S. Contejean.
*)

module type S = Diophantine_intf.S

module Make(Z : Int.DERIVED) : S with module Z = Z
