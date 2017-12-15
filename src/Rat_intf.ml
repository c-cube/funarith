
module type S = sig

  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int

  val neg : t -> t
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t

  val zero : t
  val one : t

  val inf : t
  val minus_inf : t

  val print : t CCFormat.printer

end
