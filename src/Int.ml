
(** {1 Interface for Integers} *)

(** This abstracts over a type and operation for integers.

    A possible implementation is Zarith, with module {!Z} *)

module type FULL = Int_intf.FULL
module type BASE = Int_intf.BASE
module type DERIVED = Int_intf.DERIVED

let sqrt_naive i =
  if i<0 then invalid_arg "sqrt"
  else if i=0 then 0
  else (
    let r = ref 0 in
    while !r < i && !r * !r <= i do incr r; done;
    pred !r
  )

module Default : FULL with type t = int = struct
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
end

module Derive(T : BASE) : DERIVED with type t = T.t = struct
  include T
  let zero = of_int 0
  let minus_one = of_int ~-1
  let one = of_int 1
  let[@inline] equal a b = T.compare a b = 0
  let[@inline] pred x = x + minus_one
  let[@inline] succ x = x + one
  let[@inline] sign x = T.compare x zero
end
