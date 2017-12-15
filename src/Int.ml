
(** {1 Interface for Integers} *)

(** This abstracts over a type and operation for integers.

    A possible implementation is Zarith, with module {!Z} *)

module type S = Int_intf.S

let sqrt_naive i =
  if i<0 then invalid_arg "sqrt"
  else if i=0 then 0
  else (
    let r = ref 0 in
    while !r < i && !r * !r <= i do incr r; done;
    pred !r
  )

module Default : S with type t = int = struct
  include Pervasives
  include CCInt
  let sqrt = sqrt_naive
  let[@inline] divexact x y = x/y
  let[@inline] of_int x = x
  let[@inline] rem x y = x mod y
  let probab_prime _ _ = 1 (* TODO: better try *)
  let minus_one = -1
  let one = 1
  let zero = 0
  let pp_print = Format.pp_print_int
end
