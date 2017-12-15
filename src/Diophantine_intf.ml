
(** {2 Abstract representation of Integers} *)

module type S = sig
  module Int : Int.S

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
