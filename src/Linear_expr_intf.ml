
(** {2 Coeficient interface} *)

(** Coeficients

    Coefficients are used in expressions. They usually
    are either rationals, or integers.
*)
module type COEF = sig
  type t

  val equal : t -> t -> bool
  (** Equality on coefficients. *)

  val compare : t -> t -> int
  (** Comparison on coefficients. *)

  val pp : t CCFormat.printer
  (** Pinter for coefficients. *)

  val zero : t
  (** The zero coefficient. *)

  val one : t
  (** The one coefficient (to rule them all, :p). *)

  val neg : t -> t
  (** Unary negation *)

  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  (** Standard operations on coefficients. *)
end

(** {2 Variable interfaces} *)

(** Variable interface.

    Standard interface for variables that are meant to be used
    in expressions.
*)
module type VAR = sig
  type t
  (** Variable type. *)
  val compare : t -> t -> int
  (** Standard comparison function on variables. *)
  val pp : t CCFormat.printer
  (** Printer for variables. *)
end

(** Fresh variables

    Standard interface for variables with an infinite number
    of 'fresh' variables. A 'fresh' variable should be distinct
    from any other.
*)
module type FRESH = sig
  type var
  (** The type of variables. *)

  type t
  (** A type of state for creating fresh variables. *)

  val create : unit -> t
  (** Create a fresh variable. *)

  val fresh : t -> var
  (** Create a fresh variable using an existing variable as base.
      TODO: need some explaining, about the difference with {!create}. *)
end

(** Generative Variable interface.

    Standard interface for variables that are meant to be used
    in expressions. Furthermore, fresh variables can be generated
    (which is useful to refactor and/or put problems in specific
    formats used by algorithms).
*)
module type VAR_GEN = sig
  include VAR

  (** Generate fresh variables on demand *)
  module Fresh : FRESH with type var := t
end


(** {2 Linear expressions & formulas} *)

(** Linear expressions & formulas.

    This modules defines linear expressions (which are linear
    combinations of variables), and linear constraints, where
    the value of a linear expressions is constrained.
*)
module type S = sig
  module C : COEF
  (** Coeficients used. Can be integers as well as rationals. *)

  module Var : VAR
  (** Variables used in expressions. *)

  type var = Var.t
  (** The type of variables appearing in expressions. *)

  module Var_map : CCMap.S with type key = var
  (** Maps from variables, used for expressions as well as substitutions. *)

  type subst = C.t Var_map.t
  (** Type for substitutions. *)

  (** Combinations.

      This module defines linear combnations as mapping from variables
      to coefficients. This allows for very fast computations.
  *)
  module Comb : sig
    type t = private C.t Var_map.t
    (** The type of linear combinations. *)

    val compare : t -> t -> int
    (** Comparisons on linear combinations. *)

    val pp : t CCFormat.printer
    (** Printer for linear combinations. *)

    val is_empty : t -> bool
    (** Is the given expression empty ?*)

    (** {5 Creation} *)

    val empty : t
    (** The empty linear combination. *)

    val monomial : C.t -> var -> t
    (** [monome n v] creates the linear combination [n * v] *)

    val monomial1 : var -> t
    (** [monome1 v] creates the linear combination [1 * v] *)

    val add : C.t -> var -> t -> t
    (** [add n v t] adds the monome [n * v] to the combination [t]. *)


    (** Infix operations on combinations

        This module defines usual operations on linear combinations,
        as infix operators to ease reading of complex computations. *)
    module Infix : sig
      val (+) : t -> t -> t
      (** Addition between combinations. *)
      val (-) : t -> t -> t
      (** Substraction between combinations. *)
      val ( * ) : C.t -> t -> t
      (** Multiplication by a constant. *)
    end
    include module type of Infix
    (** Include the previous module. *)

    val of_list : (C.t * var) list -> t

    val to_list : t -> (C.t * var) list
    (** Converters to and from lists of monomes. *)

    val of_map : C.t Var_map.t -> t

    val to_map : t -> C.t Var_map.t

    (** {5 Semantics} *)

    val eval : subst -> t -> C.t
    (** Evaluate a linear combination given a substitution for its variables.
        TODO: document potential exceptions raised ?*)
  end

  (** {2 Linear expressions.} *)

  (** Linear expressions represent linear arithmetic expressions as
      a linear combination and a constant.  *)
  module Expr : sig
    type t
    (** The type of linear expressions. *)

    val comb : t -> Comb.t
    val const : t -> C.t

    val is_zero : t -> bool

    val compare : t -> t -> int
    (** Standard comparison function on expressions. *)

    val pp : t CCFormat.printer
    (** Standard printing function on expressions. *)

    val zero : t
    (** The expression [0]. *)

    val of_const : C.t -> t
    (** The constant expression. *)

    val of_comb : Comb.t -> t
    (** Combination without constant *)

    val of_list : (C.t * Var.t) list -> t

    val make : Comb.t -> C.t -> t
    (** [make c n] makes the linear expression [c + n]. *)

    (** Infix operations on expressions

        This module defines usual operations on linear expressions,
        as infix operators to ease reading of complex computations. *)
    module Infix : sig
      val (+) : t -> t -> t
      (** Addition between expressions. *)
      val (-) : t -> t -> t
      (** Substraction between expressions. *)
      val ( * ) : C.t -> t -> t
      (** Multiplication by a constant. *)
    end
    include module type of Infix
    (** Include the previous module. *)

    (** {5 Semantics} *)

    val eval : subst -> t -> C.t
    (** Evaluate a linear expression given a substitution for its variables.
        TODO: document potential exceptions raised ?*)
  end

  (** {2 Linear constraints.} *)

  (** Linear constraints represent constraints on expressions. *)
  module Constr : sig
    type op = [ `Leq | `Geq | `Lt | `Gt | `Eq | `Neq ]
    (** Arithmetic comparison operators. *)

    type +'a t = {
      expr: Expr.t;
      op: 'a;
    } constraint 'a = [< op ]
    (** Linear constraints. Expressions are implictly comapred to zero. *)

    val compare : 'a t -> 'a t -> int
    (** Standard comparison function. *)

    val pp : _ t CCFormat.printer
    (** Standard printing function. *)

    val of_expr : Expr.t -> 'a -> 'a t
    val make : Comb.t -> 'a -> C.t -> 'a t
    (** Create a constraint from a linear expression/combination and a constant. *)

    val op : 'a t -> 'a
    val expr : _ t -> Expr.t
    (** Extract the given part from a constraint. *)

    val split : 'a t -> Comb.t * 'a * C.t
    (** Split the linear combinations from the constant *)

    val eval : subst -> _ t -> bool
    (** Evaluate the given constraint under a substitution.
        TODO: document exceptions raised. *)
  end
end

