
module Q = QCheck

module Pr = Funarith_zarith.Prime

let rand_n n : Z.t Q.arbitrary =
  Q.map ~rev:Z.to_int Z.of_int Q.(0 -- n)

module Make(X : sig val cache : Pr.Cache.t option end) = struct
  let cache = X.cache

  let suffix = match cache with None -> "" | Some _ -> "_cached"

  let check_is_prime =
    let prop n =
      let ref_res =
        Z.to_int n=2 || Z.to_int n=3 ||
        (
          Z.to_int n >= 4 &&
          Sequence.(2 -- Z.to_int n
                    |> map Z.of_int
                    |> for_all
                      (fun m -> Z.equal m n || not (Z.equal Z.zero (Z.rem n m))))
        )
      in
      ref_res = Pr.is_prime ?cache n
    in
    Q.Test.make ~count:2_000 ~long_factor:10 ~name:("is_prime_correct"^suffix)
      (rand_n 1_000_000) prop

  let check_primes_leq_sound =
    let prop n = Pr.primes_leq ?cache n |> Sequence.for_all Pr.is_prime in
    Q.Test.make ~count:10 ~name:("primes_leq_sound"^suffix) (rand_n 20_000) prop

  let check_primes_leq_exhaustive =
    let prop n =
      let l1 = Pr.primes_leq ?cache (Z.of_int n) |> Sequence.to_list in
      let l2 =
        Sequence.(2 -- n |> map Z.of_int |> filter Pr.is_prime |> Sequence.to_list)
      in
      CCList.equal Z.equal l1 l2
    in
    Q.Test.make ~count:10 ~name:("primes_leq_exaustive"^suffix) Q.(2 -- 2000) prop

  let props = [
    check_is_prime;
    check_primes_leq_sound;
    check_primes_leq_exhaustive
  ]
end

let props =
  let module M1 = Make(struct let cache=None end) in
  let module M2 = Make(struct let cache=Some (Pr.Cache.create()) end) in
  CCList.append M1.props M2.props


