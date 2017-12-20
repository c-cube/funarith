module type S = sig
  module Z : Int.FULL

  type divisor = {
    prime : Z.t;
    power : int;
  }

  module Cache : sig
    type t

    val create : ?init_size:int -> unit -> t

    val clear : t -> unit
  end

  val is_prime : ?cache:Cache.t -> Z.t -> bool
  (** Is the integer prime? *)

  val prime_decomposition : ?cache:Cache.t -> Z.t -> divisor list
  (** Decompose the number into a product of power-of-primes.
      Cheap if [cache=Some c] and [is_prime_cached c n] was called before.
      @param cache if provided, store results in the cache.
      @raise Invalid_argument if the number is negative *)

  val primes_leq : ?cache:Cache.t -> Z.t -> Z.t Sequence.t
  (** Sequence of prime numbers smaller than (or equal to)
      the given number.
      @param cache if [Some c], use [c] for storing intermediate results *)
end
