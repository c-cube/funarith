
(** Core operations *)
module type BASE = sig
  type t
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val compare : t -> t -> int
  val of_int : int -> t
  val pp : t CCFormat.printer
end

(** Operations that can be derived from {!BASE} *)
module type DERIVED = sig
  include BASE

  val pred : t -> t
  val succ : t -> t
  val zero : t
  val sign : t -> int
  val equal : t -> t -> bool
  val one : t
  val minus_one : t
end

(** All basic operations, along with more sophisticated ones *)
module type FULL = sig
  include DERIVED
  val hash : t -> int
  val divexact : t -> t -> t (* TODO: specify *)
  val rem : t -> t -> t
  val sqrt : t -> t

  val probab_prime : t -> int -> int
  (** [probab_prime n strength] is a sound, fast, but incomplete
      primality checker.
      It must return:

      - [0] if [n] is not a prime
      - [1] if [n] might be a prime
      - [2] if [n] is a prime

      If the answer is [0] or [1] then it must be valid. *)
end
