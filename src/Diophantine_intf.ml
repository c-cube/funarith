
(** {2 Abstract representation of Integers} *)

(** A possible implementation is Zarith, with module {!Z} *)
module type INT = sig
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

module type S = sig
  module Int : INT

  exception Bad_shape

  module Solution : sig
    type t = Int.t array
    (** Immutable! do not modify *)

    val pp : t CCFormat.printer
  end

  (** {2 Single equation without constant} *)
  module Homogeneous_eqn : sig
    type t

    val make : Int.t array -> t
    val len : t -> int
    val get_coeff : int -> t -> Int.t

    val find_a_solution : t -> Solution.t option
    (** Solve the equation by returning a solution, if there is any *)

    val compute : t -> Int.t array -> Int.t
    (** Compute the value of the equation with the given values for
        the variables.
        @raise Bad_shape if the sizes don't coincide *)

    val pp : t CCFormat.printer
  end

  (** {2 Single equation} *)
  module Eqn : sig
    type t

    val coeffs : t -> Int.t Sequence.t
    val len : t -> int
    val get_coeff : int -> t -> Int.t
    val offset : t -> Int.t

    val make : Int.t array -> Int.t -> t
      (** Make an equation *)

    val compute : t -> Int.t array -> Int.t
    (** Compute the value of the equation with the given values for
        the variables.
        @raise Bad_shape if the sizes don't coincide *)

    val pp : t CCFormat.printer
  end

end
