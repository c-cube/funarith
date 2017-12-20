
(** {1 Interface for Integers} *)

(** This abstracts over a type and operation for integers.

    A possible implementation for arbitrary precision numbers
    is Zarith, with module {!Z}
*)

module type FULL = Int_intf.FULL
module type BASE = Int_intf.BASE
module type DERIVED = Int_intf.DERIVED

module Default : FULL with type t = int
(** Fixed precision *)

module Derive(T : BASE) : DERIVED with type t = T.t 
