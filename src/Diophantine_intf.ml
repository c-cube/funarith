
(** {2 Abstract representation of Integers} *)

module type S = sig
  module Z : Int.DERIVED

  exception Bad_shape

  module Solution : sig
    type t = Z.t array
    (** Immutable! do not modify *)

    val pp : t CCFormat.printer
  end

  (** {2 Single equation without constant} *)
  module Homogeneous_eqn : sig
    type t

    val make : Z.t array -> t
    val len : t -> int
    val get_coeff : int -> t -> Z.t

    val find_a_solution : t -> Solution.t option
    (** Solve the equation by returning a solution, if there is any *)

    val compute : t -> Z.t array -> Z.t
    (** Compute the value of the equation with the given values for
        the variables.
        @raise Bad_shape if the sizes don't coincide *)

    val pp : t CCFormat.printer
  end

  (** {2 Single equation} *)
  module Eqn : sig
    type t

    val coeffs : t -> Z.t Sequence.t
    val len : t -> int
    val get_coeff : int -> t -> Z.t
    val offset : t -> Z.t

    val make : Z.t array -> Z.t -> t
    (** Make an equation *)

    val compute : t -> Z.t array -> Z.t
    (** Compute the value of the equation with the given values for
        the variables.
        @raise Bad_shape if the sizes don't coincide *)

    val pp : t CCFormat.printer
  end

  module Homogeneous_system : sig
    type t

    exception Empty
    exception Inconsistent_lengths

    val make : Z.t array array -> t
    (** [make eqns] builds the system [eqns = 0], where each [eqn : Z.t array]
        in [eqns] is an array [ [| a1, …, an |] ] representing
        the equation [a1 x1 + a2 x2 + … + an xn = 0].
        @raise Empty if the array is empty
        @raise Inconsistent_lengths if all equations don't have the same length
    *)

    val len : t -> int
    (** Number of equations *)

    val n_vars : t -> int
    (** Number of variables (i.e. length of each equation) *)

    val eqns : t -> Z.t array array
    (** Get underlying equations.
        {b NOTE}: do not modify! *)

    type solution = Z.t array
    (** Vector of positive coefficients for the variables *)

    val solve : ?cut:(solution->bool) -> t -> solution Sequence.t
    (** Return an iterator on minimum solutions.
        Any solution to the initial problem is a linear combination of these
        minimal solutions.
        @param cut called on every intermediate tuple traversed by
          the algorithm (in an increasing order). If it returns [true],
          the tuple (and all solutions below it) is dropped.
    *)

    val solve_l : ?cut:(solution->bool) -> t -> solution list
    (** Eager version of {!solve}, returns the (reverse) list of solutions *)

    val pp : t CCFormat.printer
    val pp_sol : solution CCFormat.printer

    (**/**)
    val log_enabled : bool ref
    (**/**)
  end

  (* TODO: solve heterogeneous systems, using homogeneous system + first variable
     with coefficient [-rhs_i], being blocked between [0,1] using [cut] *)
end
