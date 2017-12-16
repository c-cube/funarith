
module type S = Prime_intf.S

module Make(I : Int.S) = struct
  module Z = I

  module Tbl = Hashtbl.Make(Z)

  type divisor = {
    prime : Z.t;
    power : int;
  }

  let two = Z.of_int 2

  module Cache = struct
    type t = Z.t option Tbl.t (* maps to a divisor (if any) *)

    let clear tbl : unit =
      Tbl.clear tbl;
      Tbl.add tbl two None;
      ()

    let create ?(init_size=64) () : t =
      let tbl = Tbl.create init_size in
      clear tbl;
      tbl

    let add = Tbl.add
    let find = Tbl.find
    let[@inline] add_prime_ tbl n = Tbl.replace tbl n None

    (* add to the table the fact that [d] is a divisor of [n] *)
    let add_divisor_ tbl n d =
      assert (not (Tbl.mem tbl n));
      Tbl.add tbl n (Some d)
  end

  (* primality test: find a divisor *)
  let find_divisor_raw_ (n0:Z.t) : Z.t option =
    let n = ref Z.one in
    let bound = Z.succ (Z.sqrt n0) in
    let is_prime = ref true in
    while !is_prime && Z.compare !n bound < 0 do
      n := Z.succ !n;
      if Z.equal Z.zero (Z.rem n0 !n) then (
        is_prime := false; (* also, break *)
      );
    done;
    if !is_prime then None else Some !n

  let find_divisor cache n : Z.t option =
    begin match cache with
      | None -> find_divisor_raw_ n
      | Some c ->
        begin match Cache.find c n with
          | Some _ as res -> res
          | None -> None
          | exception Not_found ->
            let d = find_divisor_raw_ n in
            Cache.add c n d;
            d
        end
    end

  let[@inline] is_prime_raw n = match Z.probab_prime n 7 with
    | 0 -> false
    | 2 -> true
    | 1 ->
      begin match find_divisor_raw_ n with
        | None -> true
        | Some _ -> false
      end
    | _ -> assert false

  let is_prime_cache c n =
    begin match Cache.find c n with
      | None -> true
      | Some _ -> false
      | exception Not_found ->
        begin match Z.probab_prime n 7 with
          | 0 -> false
          | 2 -> Cache.add_prime_ c n; true
          | 1 ->
            begin match find_divisor_raw_ n with
              | None -> true
              | Some d -> Cache.add_divisor_ c n d; false
            end
          | _ -> assert false
        end
    end

  let[@inline] is_prime ?cache n = match cache with
    | None -> is_prime_raw n
    | Some c -> is_prime_cache c n

  let rec merge_ l1 l2 = match l1, l2 with
    | [], _ -> l2
    | _, [] -> l1
    | p1::l1', p2::l2' ->
      match Z.compare p1.prime p2.prime with
        | 0 ->
          {prime=p1.prime; power=p1.power+p2.power} :: merge_ l1' l2'
        | n when n < 0 ->
          p1 :: merge_ l1' l2
        | _ -> p2 :: merge_ l1 l2'

  let rec decompose_rec ~cache n =
    begin match find_divisor cache n with
      | None -> [{prime=n; power=1;}]
      | Some q1 ->
        let q2 = Z.divexact n q1 in
        merge_ (decompose_rec ~cache q1) (decompose_rec ~cache q2)
    end

  let prime_decomposition ?cache n =
    if is_prime n
    then [{prime=n; power=1;}]
    else decompose_rec ~cache n

  let primes_leq ?cache n0 k =
    let n = ref two in
    while Z.compare !n n0 <= 0 do
      if is_prime ?cache !n then k !n;
      n := Z.succ !n;
    done

end
