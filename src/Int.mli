
(** {1 Interface for Integers} *)

(** This abstracts over a type and operation for integers.

    A possible implementation for arbitrary precision numbers
    is Zarith, with module {!Z}
*)

module type S = Int_intf.S

module Default : S with type t = int
(** Fixed precision *)
