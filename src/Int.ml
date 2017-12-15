
(** {1 Interface for Integers} *)

(** This abstracts over a type and operation for integers.

    A possible implementation is Zarith, with module {!Z} *)

module type S = Int_intf.S

module Default : S with type t = int = struct
  include Pervasives
  include CCInt
  let minus_one = -1
  let one = 1
  let zero = 0
  let pp_print = Format.pp_print_int
end
