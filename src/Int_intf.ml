
module type S = sig
  type t
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val pred : t -> t
  val succ : t -> t
  val zero : t
  val one : t
  val minus_one : t
  val sign : t -> int
  val compare : t -> t -> int
  val equal : t -> t -> bool

  val pp_print : t CCFormat.printer
end
