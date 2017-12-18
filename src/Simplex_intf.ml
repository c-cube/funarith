(*
  copyright (c) 2014-2018, Guillaume Bury, Simon Cruanes
  *)

(** {1 Modular and incremental implementation of the general simplex}. *)

(** The simplex is used as a decision procedure for linear rational arithmetic
    problems.

    More information can be found on the particular flavor of this
    implementation at https://gbury.eu/public/papers/stage-m2.pdf
*)

(** The types of the variables used by the equations to solve *)
module type VAR = sig
  type t
  val compare : t -> t -> int
end

module type VAR_PP = sig
  include VAR
  val pp : t CCFormat.printer
end

module type S = sig
  (** Rational number implementation *)
  module Q : Rat.S

  (** The given type of the variables *)
  type var

  (** A map on variables *)
  module Var_map : CCMap.S with type key = var

  (** The type of a (possibly not solved) linear system *)
  type t

  (** An unsatisfiability explanation is a couple [(x, expr)]. If [expr] is the
      empty list, then there is a contradiction between two given bounds of [x].
      Else, the explanation is an equality [x = expr] that is valid
      (it can be derived from the original equations of the system) from which a
      bound can be deduced which contradicts an already given bound of the
      system. *)
  type cert = {
    cert_var: var;
    cert_expr: (Q.t * var) list;
  }

  (** Generic type returned when solving the simplex. A solution is a list of
      bindings that satisfies all the constraints inside the system. If the
      system is unsatisfiable, an explanation of type ['cert] is returned. *)
  type res =
    | Solution of Q.t Var_map.t
    | Unsatisfiable of cert

  (** {3 Simplex construction} *)

  (** The empty system *)
  val create : unit -> t

  (** Returns a copy of the given system *)
  val copy : t -> t

  (** [add_eq s (x, eq)] adds the equation [x=eq] to [s] *)
  val add_eq : t -> var * (Q.t * var) list -> unit

  (** [add_bounds (x, lower, upper)] adds to [s]
      the bounds [lower] and [upper] for the given variable [x].
      If the bound is loose on one side
      (no upper bounds for instance), the values [Q.inf] and
      [Q.minus_inf] can be used. By default, in a system, all variables
      have no bounds, i.e have lower bound [Q.minus_inf] and upper bound
      [Q.inf].
      Optional parameters allow to make the the bounds strict. Defaults to false,
      so that bounds are large by default. *)
  val add_bounds : t -> ?strict_lower:bool -> ?strict_upper:bool -> var * Q.t * Q.t -> unit

  val add_lower_bound : t -> ?strict:bool -> var -> Q.t -> unit

  val add_upper_bound : t -> ?strict:bool -> var -> Q.t -> unit

  (** {3 Simplex solving} *)

  (** [ksolve s] solves the system [s] and returns a solution, if one exists.
      This function may change the internal representation of the system to that
      of an equivalent one
      (permutation of basic and non basic variables and pivot operation on the tableaux).
      @param debug An optional debug option can be given, and will be applied to
      all systems encountered while solving the system, including the initial and
      final states of the system. Can be used for printing intermediate states of
      the system. *)
  val solve : t -> res

  (* TODO: push/pop *)

  (** {3 Access functions} *)
  (* TODO: add new access functions ? *)

  (** [get_tab s] returns the current table of [s] as a triple [(l, l', tab)]
      where [l] is the list of the non-basic variables, [l'] the list of basic
      variables and [tab] the list of the rows of the tableaux in the same order
      as [l] and [l']. *)
  val get_tab : t -> var list * var list * Q.t list list

  (** [get_assign s] returns the current (partial) assignment of the variables in
      [s] as a list of bindings.  Only non-basic variables (as given by [get_tab])
      should appear in this assignent. As such, and according to simplex
      invariants, all variables in the assignment returned should satisfy their
      bounds. *)
  val get_assign : t -> (var * Q.t) list

  val get_assign_map : t -> Q.t Var_map.t
  (** Same as {!get_assign} but with a map *)

  (* [get_full_assign s] returns the current values of all the variables present
     in the system.  Notice that it doesn't mean the assignment returned
     satisfies all bounds.*)
  val get_full_assign : t -> Q.t Var_map.t

  val get_full_assign_l : t -> (var * Q.t) list

  (** [get_bounds s x] returns the pair [(low, upp)] of the current bounds for
      the variable [x].
      Notice that it is possible that [low] is strictly greater than [upp]. *)
  val get_bounds : t -> var -> Q.t * Q.t

  (** [get_all_bounds s] returns the list of all the explicit bounds of [s]. Any
      variable not present in the return value is assumed to have no bounds
      (i.e lower bound [Q.minus_inf] and upper bound [Q.inf]). *)
  val get_all_bounds : t -> (var * (Q.t * Q.t)) list

  (* TODO: proof checker for unsat certificates *)

  (**/**)
  val check_invariants : t -> bool (* check that all invariants hold *)
  (**/**)
end

module type S_PP = sig
  include S

  val pp_full_state : t CCFormat.printer
end

module type VAR_GEN = sig
  include VAR_PP

  (** Generate fresh variables on demand *)
  module Fresh : sig
    type var = t

    type t
    val create : unit -> t
    val fresh : t -> var
  end
end

module type S_FULL = sig
  include S_PP

  type subst = Q.t Var_map.t

  module Expr : sig
    type t = Q.t Var_map.t
    val is_empty : t -> bool
    val empty : t
    val singleton : Q.t -> var -> t
    val singleton1 : var -> t
    val add : Q.t -> var -> t -> t
    module Infix : sig
      val (+) : t -> t -> t
      val (-) : t -> t -> t
      val ( * ) : Q.t -> t -> t
    end
    include module type of Infix
    val of_list : (Q.t * var) list -> t
    val to_list : t -> (Q.t * var) list
    val pp : t CCFormat.printer
    val eval : subst -> t -> Q.t
  end

  module Constr : sig
    type op = Leq | Geq | Lt | Gt | Eq

    type t = {
      op: op;
      expr: Expr.t;
      const: Q.t;
    }

    val make : op -> Expr.t -> Q.t -> t
    val op : t -> op
    val expr : t -> Expr.t
    val const : t -> Q.t
    val pp : t CCFormat.printer

    val eval : subst -> t -> bool
  end

  module Problem : sig
    type t = Constr.t list
    module Infix : sig
      val ( && ) : t -> t -> t
    end
    include module type of Infix
    val eval : subst -> t -> bool
    val pp : t CCFormat.printer
  end

  val add_constr : t -> Constr.t -> unit
  (** Add a constraint *)

  val add_problem : t -> Problem.t -> unit
end

