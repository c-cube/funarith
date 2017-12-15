
(** {1 Interface for Rationals} *)

(** This abstracts over a type and operation for rationals.

    A possible implementation for arbitrary precision numbers
    is Zarith, with module {!Q}
*)

module type S = Rat_intf.S

