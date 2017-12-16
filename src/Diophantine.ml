
module Fmt = CCFormat

module type S = Diophantine_intf.S

module Make(Z : Int.S) = struct
  module Z = Z

  exception Bad_shape

  let[@inline] is_zero x : bool = Z.sign x=0

  module Solution = struct
    type t = Z.t array

    let pp out (sol:t): unit =
      if Array.length sol=0 then Fmt.int out 0
      else Fmt.array ~sep:(Fmt.return "@ + ") Z.pp out sol
  end

  module Homogeneous_eqn = struct
    type t = Z.t array

    let[@inline] make a = a
    let len = Array.length
    let[@inline] get_coeff i a = a.(i)

    let pp out (e:t) =
      for i=0 to Array.length e-1 do
        if i>0 then Fmt.fprintf out " + ";
        if Z.equal Z.one e.(i) then (
          Fmt.fprintf out "X%d" i
        ) else (
          Fmt.fprintf out "%a X%d" Z.pp e.(i) i
        )
      done

    let compute (e:t) (arr:Z.t array) : Z.t =
      if Array.length arr <> len e then raise Bad_shape;
      CCArray.fold2 (fun acc coeff x -> Z.(acc + coeff * x)) Z.zero e arr

    (* temporarily make [sol] into [eye_i] *)
    let set_eye i (sol:Solution.t) k : unit =
      assert (is_zero sol.(i));
      sol.(i) <- Z.one;
      k sol;
      sol.(i) <- Z.zero

    (* previous state: compute<0, so we need to increase variable(s)
       in [X_j,X_{j+1},…] with positive coeffs *)
    let rec go_up e sol j k : unit =
      begin match Z.sign (compute e sol) with
        | 0 -> k sol
        | n when n<0 ->
          (* try to increment variables with positive coeff *)
          for i=j to len e-1 do
            if Z.sign (get_coeff i e) > 0 then (
              let old = sol.(i) in
              sol.(i) <- Z.succ old;
              go_up e sol i k; (* recurse, ignoring variables before X_i *)
              sol.(i) <- old;
            )
          done
        | n -> assert (n>0); () (* gone too far *)
      end

    (* previous state: compute>0, so we need to decrease variable(s)
       in [X_j,X_{j+1},…] with positive coeffs *)
    let rec go_down e sol j k : unit =
      begin match Z.sign (compute e sol) with
        | 0 -> k sol
        | n when n<0 -> () (* too low *)
        | n ->
          assert (n>0);
          (* try to increment variables with negative coeff *)
          for i=j to len e-1 do
            if Z.sign (get_coeff i e) < 0 then (
              let old = sol.(i) in
              sol.(i) <- Z.succ old;
              go_up e sol i k; (* recurse, ignoring variables before X_i *)
              sol.(i) <- old;
            )
          done
      end

    let solve_rec (e:t) (sol:Solution.t) (k:Solution.t->unit) : unit =
      begin match Z.sign (compute e sol) with
        | 0 -> k sol
        | n when n<0 -> go_up e sol 0 k
        | n -> assert (n>0); go_down e sol 0 k
      end

    let find_a_solution (e:t) : Solution.t option =
      let n = len e in
      let arr = Array.make n Z.zero in
      (* start with eyes *)
      Sequence.head
        (fun yield ->
           Sequence.int_range ~start:0 ~stop:(n-1)
             (fun i ->
                set_eye i arr
                  (fun sol ->
                     solve_rec e sol
                     |> Sequence.map Array.copy
                     |> Sequence.iter yield)))
  end

  module Eqn = struct
    type t = {
      coeffs: Homogeneous_eqn.t;
      offset: Z.t;
    }

    let[@inline] make coeffs offset : t = {coeffs;offset}
    let[@inline] offset t = t.offset
    let[@inline] coeffs t = Sequence.of_array t.coeffs
    let[@inline] get_coeff i t = Homogeneous_eqn.get_coeff i t.coeffs
    let[@inline] len t = Homogeneous_eqn.len t.coeffs

    let pp out (t:t) : unit =
      if Array.length t.coeffs = 0 then (
        Z.pp out t.offset
      ) else if is_zero t.offset then (
        Homogeneous_eqn.pp out t.coeffs
      ) else (
        Fmt.fprintf out "%a + %a"
          Homogeneous_eqn.pp t.coeffs Z.pp t.offset
      )

    let compute (e:t) (arr:Z.t array) : Z.t =
      if Array.length arr <> len e then raise Bad_shape;
      CCArray.fold2 (fun acc coeff x -> Z.(acc + coeff * x)) e.offset e.coeffs arr
  end
end

