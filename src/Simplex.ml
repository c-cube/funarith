(*
  copyright (c) 2014-2018, Guillaume Bury, Simon Cruanes
  *)

(* OPTIMS:
 * - distinguish separate systems (that do not interact), such as in { 1 <= 3x = 3y <= 2; z <= 3} ?
 * - Implement gomorry cuts ?
*)

open Containers

module type VAR = Simplex_intf.VAR
module type VAR_PP = Simplex_intf.VAR_PP
module type VAR_GEN = Simplex_intf.VAR_GEN

module type S = Simplex_intf.S
module type S_PP = Simplex_intf.S_PP
module type S_FULL = Simplex_intf.S_FULL

module Vec = CCVector

(* Simplex Implementation *)
module Make(Q : Rat.S)(Var: VAR) = struct

  module Q = Q
  module M = CCMap.Make(Var)

  (* Exceptions *)
  exception Unsat of Var.t
  exception AbsurdBounds of Var.t
  exception NoneSuitable
  exception UnExpected of string

  let[@inline] unexpected s = raise (UnExpected s)

  type var = Var.t

  type basic_var = var
  type nbasic_var = var

  type erat = {
    base: Q.t; (* reference number *)
    eps_factor: Q.t; (* coefficient for epsilon, the infinitesimal *)
  }

  (** Epsilon-rationals, used for strict bounds *)
  module Erat = struct
    type t = erat

    let zero : t = {base=Q.zero; eps_factor=Q.zero}

    let[@inline] make base eps_factor : t = {base; eps_factor}
    let[@inline] base t = t.base
    let[@inline] eps_factor t = t.eps_factor
    let[@inline] mul k e = Q.(make (k * e.base) (k * e.eps_factor))
    let[@inline] sum e1 e2 = Q.(make (e1.base + e2.base) (e1.eps_factor + e2.eps_factor))
    let[@inline] compare e1 e2 = match Q.compare e1.base e2.base with
      | 0 -> Q.compare e1.eps_factor e2.eps_factor
      | x -> x

    let lt a b = compare a b < 0
    let gt a b = compare a b > 0

    let[@inline] min x y = if compare x y <= 0 then x else y
    let[@inline] max x y = if compare x y >= 0 then x else y

    let[@inline] evaluate (epsilon:Q.t) (e:t) : Q.t = Q.(e.base + epsilon * e.eps_factor)
  end

  module Var_map = M

  module Mat : sig
    type 'a t

    val create : unit -> 'a t
    val init : int -> int -> (int -> int -> 'a) -> 'a t
    val get : 'a t -> int -> int -> 'a
    val set : 'a t -> int -> int -> 'a -> unit
    val get_row : 'a t -> int -> 'a Vec.vector
    val copy : 'a t -> 'a t
    val n_row : _ t -> int
    val n_col : _ t -> int
    val push_row : 'a t -> 'a -> unit (* new row, filled with element *)
    val push_col : 'a t -> 'a -> unit (* new column, filled with element *)
    val to_list : 'a t -> 'a list list

    (**/**)
    val check_invariants : _ t -> bool
    (**/**)
  end = struct
    type 'a t = {
      mutable n_col: int; (* num of columns *)
      tab: 'a Vec.vector Vec.vector;
    }

    let[@inline] create() : _ = {tab=Vec.create(); n_col=0}

    let[@inline] init n m f = {
      tab=Vec.init n (fun i -> Vec.init m (fun j -> f i j));
      n_col=m;
    }

    let[@inline] get m i j = Vec.get (Vec.get m.tab i) j
    let[@inline] get_row m i = Vec.get m.tab i
    let[@inline] set (m:_ t) i j x = Vec.set (Vec.get m.tab i) j x
    let[@inline] copy m = {m with tab=Vec.map Vec.copy m.tab}

    let[@inline] n_row m = Vec.length m.tab
    let[@inline] n_col m = m.n_col

    let push_row m x = Vec.push m.tab (Vec.make (n_col m) x)
    let push_col m x =
      m.n_col <- m.n_col + 1;
      Vec.iter (fun row -> Vec.push row x) m.tab
    let to_list m = List.map Vec.to_list (Vec.to_list m.tab)

    let check_invariants m = Vec.for_all (fun r -> Vec.length r = n_col m) m.tab
  end

  type t = {
    tab : Q.t Mat.t; (* the matrix of coefficients *)
    basic : basic_var Vec.vector; (* basic variables *)
    nbasic : nbasic_var Vec.vector; (* non basic variables *)
    mutable assign : Erat.t M.t; (* assignments *)
    mutable bounds : (Erat.t * Erat.t) M.t; (* (lower, upper) bounds for variables *)
    mutable idx_basic : int M.t; (* basic var -> its index in [basic] *)
    mutable idx_nbasic : int M.t; (* non basic var -> its index in [nbasic] *)
  }

  type cert = {
    cert_var: var;
    cert_expr: (Q.t * var) list;
  }

  type res =
    | Solution of Q.t Var_map.t
    | Unsatisfiable of cert

  (* Base manipulation functions *)
  let matrix_map f m =
    for i = 0 to Array.length m - 1 do
      for j = 0 to Array.length m.(0) - 1 do
        m.(i).(j) <- f i j m.(i).(j)
      done
    done

  let create () : t = {
    tab = Mat.create ();
    basic = Vec.create ();
    nbasic = Vec.create ();
    assign = M.empty;
    bounds = M.empty;
    idx_basic = M.empty;
    idx_nbasic = M.empty;
  }

  let copy t = {
    tab = Mat.copy t.tab;
    basic = Vec.copy t.basic;
    nbasic = Vec.copy t.nbasic;
    assign = t.assign;
    bounds = t.bounds;
    idx_nbasic = t.idx_nbasic;
    idx_basic = t.idx_basic;
  }

  let index_basic (t:t) (x:basic_var) : int = match M.find x t.idx_basic with
    | n -> n
    | exception Not_found -> -1

  let index_nbasic (t:t) (x:nbasic_var) : int = match M.find x t.idx_nbasic with
    | n -> n
    | exception Not_found -> -1

  let[@inline] mem_basic (t:t) (x:var) : bool = M.mem x t.idx_basic
  let[@inline] mem_nbasic (t:t) (x:var) : bool = M.mem x t.idx_nbasic

  (* check invariants, for test purposes *)
  let check_invariants (t:t) : bool =
    Mat.check_invariants t.tab &&
    Vec.for_all (fun v -> mem_basic t v) t.basic &&
    Vec.for_all (fun v -> mem_nbasic t v) t.nbasic &&
    Vec.for_all (fun v -> not (mem_nbasic t v)) t.basic &&
    Vec.for_all (fun v -> not (mem_basic t v)) t.nbasic &&
    Vec.for_all (fun v -> Var_map.mem v t.assign) t.nbasic &&
    Vec.for_all (fun v -> not (Var_map.mem v t.assign)) t.basic &&
    true

  let[@inline] empty_expr n = Array.make n Q.zero

  (* find the definition of the basic variable [x],
     as a linear combination of non basic variables *)
  let find_expr_basic t (x:basic_var) : Q.t Vec.vector =
    begin match index_basic t x with
      | -1 -> unexpected "Trying to find an expression for a non-basic variable."
      | i -> Mat.get_row t.tab i
    end

  (* build the expression [y = \sum_i (if x_i=y then 1 else 0)·x_i] *)
  let find_expr_nbasic t (x:nbasic_var) : Q.t Vec.vector =
    Vec.map
      (fun y -> if Var.compare x y = 0 then Q.one else Q.zero)
      t.nbasic

  (* TODO: avoid double lookup in maps *)
  (* find expression of [x] *)
  let find_expr_total (t:t) (x:var) : Q.t Vec.vector =
    if mem_basic t x then
      find_expr_basic t x
    else if mem_nbasic t x then
      find_expr_nbasic t x
    else
      unexpected "Unknown variable"

  (* find coefficient of nbasic variable [y] in the definition of
     basic variable [x] *)
  let find_coef (t:t) (x:var) (y:var) : Q.t =
    begin match index_basic t x, index_nbasic t y with
      | -1, _ | _, -1 -> assert false
      | i, j -> Mat.get t.tab i j
    end

  (* compute value of basic variable.
     It can be computed by using [x]'s definition
     in terms of nbasic variables, which have values *)
  let value_basic (t:t) (x:basic_var) : Erat.t =
    assert (mem_basic t x);
    let res = ref Erat.zero in
    let expr = find_expr_basic t x in
    for i = 0 to Vec.length expr - 1 do
      let val_nbasic_i =
        try M.find (Vec.get t.nbasic i) t.assign
        with Not_found ->
          unexpected "Basic variable in expression of a basic variable."
      in
      res := Erat.sum !res (Erat.mul (Vec.get expr i) val_nbasic_i)
    done;
    !res

  (* extract a value for [x] *)
  let[@inline] value (t:t) (x:var) : Erat.t =
    try M.find x t.assign (* nbasic variables are assigned *)
    with Not_found -> value_basic t x

  (* trivial bounds *)
  let empty_bounds : Erat.t * Erat.t = Q.(Erat.make minus_inf zero, Erat.make inf zero)

  (* find bounds of [x] *)
  let[@inline] get_bounds (t:t) (x:var) : Erat.t * Erat.t =
    try M.find x t.bounds
    with Not_found -> empty_bounds

  (* is [value x] within the bounds for [x]? *)
  let is_within_bounds (t:t) (x:var) : bool * Erat.t =
    let v = value t x in
    let low, upp = get_bounds t x in
    if Erat.compare v low < 0 then
      false, low
    else if Erat.compare v upp > 0 then
      false, upp
    else
      true, v

  (* add nbasic variables *)
  let add_vars (t:t) (l:var list) : unit =
    (* add new variable to idx and array for nbasic, removing duplicates
       and variables already present *)
    let idx_nbasic, _, l =
      List.fold_left
        (fun ((idx_nbasic, offset, l) as acc) x ->
           if mem_basic t x then acc
           else if M.mem x idx_nbasic then acc
           else (
             (* allocate new index for [x] *)
             M.add x offset idx_nbasic, offset+1, x::l
           ))
        (t.idx_nbasic, Vec.length t.nbasic, [])
        l
    in
    (* add new columns to the matrix *)
    let old_dim = Mat.n_col t.tab in
    List.iter (fun _ -> Mat.push_col t.tab Q.zero) l;
    assert (old_dim + List.length l = Mat.n_col t.tab);
    Vec.append_list t.nbasic (List.rev l);
    (* assign these variables *)
    t.assign <- List.fold_left (fun acc y -> M.add y Erat.zero acc) t.assign l;
    t.idx_nbasic <- idx_nbasic;
    ()

  (* define basic variable [x] by [eq] in [t] *)
  let add_eq (t:t) (x, eq : basic_var * _ list) : unit =
    if mem_basic t x || mem_nbasic t x then (
      unexpected "Variable already defined.";
    );
    add_vars t (List.map snd eq);
    (* add [x] as a basic var *)
    t.idx_basic <- M.add x (Vec.length t.basic) t.idx_basic;
    Vec.push t.basic x;
    (* add new row for defining [x] *)
    assert (Mat.n_col t.tab > 0);
    Mat.push_row t.tab Q.zero;
    let row_i = Mat.n_row t.tab - 1 in
    assert (row_i >= 0);
    (* now put into the row the coefficients corresponding to [eq],
       expanding basic variables to their definition *)
    List.iter
      (fun (c, x) ->
         let expr = find_expr_total t x in
         assert (Vec.length expr = Mat.n_col t.tab);
         Vec.iteri
           (fun j c' ->
              if not (Q.equal Q.zero c') then (
                Mat.set t.tab row_i j Q.(Mat.get t.tab row_i j + c * c')
              ))
           expr)
      eq;
    ()

  (* add bounds to [x] in [t] *)
  let add_bound_aux (t:t) (x:var) (low:Erat.t) (upp:Erat.t) : unit =
    add_vars t [x];
    let l, u = get_bounds t x in
    t.bounds <- M.add x (max l low, min u upp) t.bounds

  let add_bounds (t:t) ?strict_lower:(slow=false) ?strict_upper:(supp=false) (x, l, u) : unit =
    let e1 = if slow then Q.one else Q.zero in
    let e2 = if supp then Q.neg Q.one else Q.zero in
    add_bound_aux t x (Erat.make l e1) (Erat.make u e2);
    if mem_nbasic t x then (
      let b, v = is_within_bounds t x in
      if not b then (
        t.assign <- M.add x v t.assign;
      )
    )

  let add_lower_bound t ?strict x l = add_bounds t ?strict_lower:strict (x,l,Q.inf)
  let add_upper_bound t ?strict x u = add_bounds t ?strict_upper:strict (x,Q.minus_inf,u)

  (* Modifies bounds in place. Do NOT export. *)
  let add_bounds_mut
      ?force:(b=false) t
      ?strict_lower:(slow=false)
      ?strict_upper:(supp=false)
      (x, l, u) =
    let low = Erat.make l (if slow then Q.one else Q.zero) in
    let upp = Erat.make u (if supp then Q.neg Q.one else Q.zero) in
    if mem_basic t x || mem_nbasic t x then (
      if b then (
        t.bounds <- M.add x (low, upp) t.bounds
      ) else (
        let low', upp' = get_bounds t x in
        t.bounds <- M.add x (max low low', min upp upp') t.bounds;
        if mem_nbasic t x then (
          let (b, v) = is_within_bounds t x in
          if not b then (
            t.assign <- M.add x v t.assign
          )
        )
      )
    ) else (
      unexpected "Variable doesn't exists"
    )

  let change_bounds = add_bounds_mut ~force:true

  (* full assignment *)
  let full_assign (t:t) : (var * Erat.t) Sequence.t =
    Sequence.append (Vec.to_seq t.nbasic) (Vec.to_seq t.basic)
    |> Sequence.map (fun x -> x, value t x)

  let[@inline] min x y = if Q.compare x y < 0 then x else y
  let[@inline] max x y = if Q.compare x y < 0 then y else x

  (* find an epsilon that is small enough
     TODO @gbury: re-explain better
  *)
  let solve_epsilon (t:t) : Q.t =
    let emin = ref Q.minus_inf in
    let emax = ref Q.inf in
    M.iter (fun x ({base=low;eps_factor=e1}, {base=upp;eps_factor=e2}) ->
      let {base=v; eps_factor=e} = value t x in
      if Q.(e - e1 > zero) then
        emin := max !emin Q.((low - v) / (e - e1))
      else if Q.(e - e1 < zero) then (* shoudln't happen as *) (* TODO: what? *)
        emax := min !emax Q.((low - v) / (e - e1));
      if Q.(e - e2 > zero) then
        emax := min !emax Q.((upp - v) / (e - e2))
      else if Q.(e - e2 < zero) then
        emin := max !emin Q.((upp - v) / (e - e2));
    ) t.bounds;
    if Q.equal Q.minus_inf !emin && Q.equal Q.inf !emax then
      Q.zero
    else if Q.compare !emin Q.zero > 0 then
      !emin
    else if Q.compare !emax Q.one >= 0 then
      Q.one
    else
      !emax

  let get_full_assign_seq (t:t) : _ Sequence.t =
    let e = solve_epsilon t in
    let f = Erat.evaluate e in
    full_assign t
    |> Sequence.map (fun (x,v) -> x, f v)

  let get_full_assign t : Q.t Var_map.t = Var_map.of_seq (get_full_assign_seq t)

  let get_full_assign_l t : _ list = get_full_assign_seq t |> Sequence.to_rev_list

  (* Find nbasic variable suitable for pivoting with [x].
     A nbasic variable [y] is suitable if it "goes into the right direction"
     (its coefficient in the definition of [x] is of the adequate sign)
     and if it hasn't reached its bound in this direction.

     precondition: [x] is a basic variable whose value in current assignment
       is outside its bounds

     We return the smallest (w.r.t Var.compare) suitable variable.
     This is important for termination.
  *)
  let find_suitable_nbasic_for_pivot (t:t) (x:basic_var) : nbasic_var * Q.t =
    assert (mem_basic t x);
    let _, v = is_within_bounds t x in
    let b = Erat.compare (value t x) v < 0 in
    (* is nbasic var [y], with coeff [a] in definition of [x], suitable? *)
    let test (y:nbasic_var) (a:Q.t) : bool =
      assert (mem_nbasic t y);
      let v = value t y in
      let low, upp = get_bounds t y in
      if b then (
        (Erat.lt v upp && Q.compare a Q.zero > 0) ||
        (Erat.gt v low && Q.compare a Q.zero < 0)
      ) else (
        (Erat.gt v low && Q.compare a Q.zero > 0) ||
        (Erat.lt v upp && Q.compare a Q.zero < 0)
      )
    in
    let nbasic_vars = t.nbasic in
    let expr = find_expr_basic t x in
    (* find best suitable variable *)
    let rec aux i =
      if i = Vec.length nbasic_vars then (
        assert (i = Vec.length expr);
        None
      ) else (
        let y = Vec.get nbasic_vars i in
        let a = Vec.get expr i in
        if test y a then (
          (* see if other variables are better suited *)
          begin match aux (i+1) with
            | None -> Some (y,a)
            | Some (z, _) as res_tail ->
              if Var.compare y z <= 0
              then Some (y,a)
              else res_tail
          end
        ) else (
          aux (i+1)
        )
      )
    in
    begin match aux 0 with
      | Some res -> res
      | None ->  raise NoneSuitable
    end

  (* pivot to exchange [x] and [y] *)
  let pivot (t:t) (x:basic_var) (y:nbasic_var) (a:Q.t) : unit =
    (* swap values ([x] becomes assigned) *)
    let val_x = value t x in
    t.assign <- t.assign |> M.remove y |> M.add x val_x;
    (* Matrix Pivot operation *)
    let kx = index_basic t x in
    let ky = index_nbasic t y in
    for j = 0 to Vec.length t.nbasic - 1 do
      if Var.compare y (Vec.get t.nbasic j) = 0 then (
        Mat.set t.tab kx j Q.(one / a)
      ) else (
        Mat.set t.tab kx j Q.(neg (Mat.get t.tab kx j) / a)
      )
    done;
    for i = 0 to Vec.length t.basic - 1 do
      if i <> kx then (
        let c = Mat.get t.tab i ky in
        Mat.set t.tab i ky Q.zero;
        for j = 0 to Vec.length t.nbasic - 1 do
          Mat.set t.tab i j Q.(Mat.get t.tab i j + c * Mat.get t.tab kx j)
        done
      )
    done;
    (* Switch x and y in basic and nbasic vars *)
    Vec.set t.basic kx y;
    Vec.set t.nbasic ky x;
    t.idx_basic <- t.idx_basic |> M.remove x |> M.add y kx;
    t.idx_nbasic <- t.idx_nbasic |> M.remove y |> M.add x ky;
    ()

  (* find minimum element of [arr] (wrt [cmp]) that satisfies predicate [f] *)
  let find_min_filter ~cmp (f:'a -> bool) (arr:('a,_) Vec.t) : 'a option =
    (* find the first element that satisfies [f] *)
    let rec aux_find_first i =
      if i = Vec.length arr then None
      else (
        let x = Vec.get arr i in
        if f x
        then aux_compare_with x (i+1)
        else aux_find_first (i+1)
      )
    (* find if any element of [l] satisfies [f] and is smaller than [x] *)
    and aux_compare_with x i =
      if i = Vec.length arr then Some x
      else (
        let y = Vec.get arr i in
        let best = if f y && cmp y x < 0 then y else x in
        aux_compare_with best (i+1)
      )
    in
    aux_find_first 0

  (* check bounds *)
  let check_bounds (t:t) : unit =
    M.iter (fun x (l, u) -> if Erat.gt l u then raise (AbsurdBounds x)) t.bounds

  (* actual solving algorithm *)
  let solve_aux (t:t) : unit =
    check_bounds t;
    (* select the smallest basic variable that is not satisfied in the current
       assignment. *)
    let rec aux_select_basic_var () =
      match
        find_min_filter ~cmp:Var.compare
          (fun x -> not (fst (is_within_bounds t x)))
          t.basic
      with
        | Some x -> aux_pivot_on_basic x
        | None -> ()
    (* remove the basic variable *)
    and aux_pivot_on_basic x =
      let _b, v = is_within_bounds t x in
      assert (not _b);
      match find_suitable_nbasic_for_pivot t x with
        | y, a ->
          (* exchange [x] and [y] by pivoting *)
          pivot t x y a;
          (* assign [x], now a nbasic variable, to the faulty bound [v] *)
          t.assign <- M.add x v t.assign;
          (* next iteration *)
          aux_select_basic_var ()
        | exception NoneSuitable ->
          raise (Unsat x)
    in
    aux_select_basic_var ();
    ()

  (* main method for the user to call *)
  let solve (t:t) : res =
    try
      solve_aux t;
      Solution (get_full_assign t)
    with
      | Unsat x ->
        let cert_expr =
          List.combine
            (Vec.to_list (find_expr_basic t x))
            (Vec.to_list t.nbasic)
        in
        Unsatisfiable { cert_var=x; cert_expr; }
      | AbsurdBounds x ->
        Unsatisfiable { cert_var=x; cert_expr=[]; }


  (** External access functions *)

  let get_tab t =
    Vec.to_list t.nbasic,
    Vec.to_list t.basic,
    Mat.to_list t.tab

  let get_assign_map t = M.map Erat.base t.assign
  let get_assign t = List.rev_map (fun (x, v) -> x,v.base) (M.bindings t.assign)

  let get_bounds t x =
    let l, u = get_bounds t x in
    l.base, u.base

  let get_all_bounds t =
    List.map
      (fun (x,(l,u)) -> (x, (l.base, u.base)))
      (M.bindings t.bounds)

(*
  LEGACY : Branch&Bound implementation
  TODO: move it to itw own functor
  exception SolutionFound of (Var.t * Q.t) list

    (* TODO: is there a better way to do this ? *)
    let is_z v = Z.equal (Q.den v) Z.one
    let is_q v = not (Z.equal (Q.den v) Z.zero || is_z v)

    let denlcm = List.fold_left (fun k c -> if Q.is_real c then Z.lcm k (Q.den c) else k) Z.one

    let lgcd k expr =
        let expr = List.filter (fun v -> Q.is_real v && not (Q.equal v Q.zero)) expr in
        let aux = (fun g c -> Z.gcd g (Q.to_bigint (Q.mul c k))) in
        Q.of_bigint (List.fold_left aux (Q.to_bigint (Q.mul (List.hd expr) k)) (List.tl expr))

    let global_bound t =
        let m, max_coef = M.fold (fun x ((l,_), (u,_)) (m, max_coef) ->
            let m = m + (if Q.is_real l then 1 else 0) + (if Q.is_real u then 1 else 0) in
            let expr = Array.to_list (find_expr_total t x) in
            let k = Q.of_bigint (denlcm (l :: u :: expr)) in
            let k' = lgcd k expr in
            let max_coef = Z.max max_coef
                (Q.to_bigint (List.fold_left Q.max Q.zero (List.filter Q.is_real (List.map (fun x -> Q.abs (Q.div (Q.mul k x) k')) (l :: u :: expr))))) in
            m, max_coef
        ) t.bounds (0, Z.zero) in
        let n = Pervasives.max (Array.length t.nbasic) m in
        Q.of_bigint (Z.pow (Z.mul (Z.of_int 2) (Z.mul (Z.pow (Z.of_int n) 2) max_coef)) n)

    let bound_all t int_vars g =
        List.fold_left (fun t x -> add_bounds t (x, Q.neg g, g)) t (List.filter int_vars (Array.to_list t.nbasic))

    type optim =
        | Tight of Var.t
        | Multiply of Var.t * Q.t

    let floor v =
        try
            Q.of_bigint (Z.ediv (Q.num v) (Q.den v))
        with Division_by_zero -> v

    let ceil v = Q.neg (floor (Q.neg v))

    let normalize int_vars t =
        let mask = List.map int_vars (Array.to_list t.nbasic) in
        let aux x expr =
            let tmp = ref [] in
            let (l,e1), (u,e2) = get_bounds t x in
            let s1 = not (Q.equal e1 Q.zero) in
            let s2 = not (Q.equal e2 Q.zero) in
            let k = Q.of_bigint (denlcm (l :: u :: expr)) in
            let k' = lgcd k expr in
            let k'' = Q.div k k' in
            if (List.for_all2 (fun b c -> b || Q.equal c Q.zero) mask expr) &&
                (not (Q.equal k' Q.one) && (not (is_z (Q.div (Q.mul l k) k')) || not (is_z (Q.div (Q.mul u k) k')))) then begin
                let low, upp = ceil (Q.mul l k''), floor (Q.mul u k'') in
                tmp := [Tight x];
                change_bounds t ~strict_lower:s1 ~strict_upper:s2 (x, low, upp)
            end else
                change_bounds t ~strict_lower:s1 ~strict_upper:s2 (x, Q.mul l k'', Q.mul u k'');
            (Multiply (x, k'') :: !tmp, List.map (fun c -> Q.mul c k'') expr)
        in
        let o, tab = List.fold_left2 (fun (opt_l, tab_l) x e ->
            let o, e' = aux x e in (o @ opt_l, e' :: tab_l)) ([], [])
            (Array.to_list t.basic) (List.map Array.to_list (Array.to_list t.tab)) in
        List.iteri (fun i l -> List.iteri (fun j k -> t.tab.(i).(j) <- k) l) tab;
        o

    let tighten int_vars t =
        let aux acc x =
            let (l,e1), (u,e2) = get_bounds t x in
            let s1 = not (Q.equal e1 Q.zero) in
            let s2 = not (Q.equal e2 Q.zero) in
            if is_q l || is_q u then begin
                change_bounds t ~strict_lower:s1 ~strict_upper:s2 (x, ceil l, floor u);
                Tight x :: acc
            end else
                acc
        in
        List.fold_left aux [] (List.filter int_vars (Array.to_list t.nbasic))

    let apply_optims l t =
        List.fold_left (fun acc f -> acc @ (f t)) [] l

    let preprocess t int_vars =
        let l = [
            tighten int_vars;
            normalize int_vars;
        ] in
        apply_optims l t

    (* Imperative implementation of the Branch&Bound *)

    (* TODO: insert user functions between iterations ? + debug function for ksolve ? *)
    let nsolve_aux max_depth t int_vars =
        let f = fun _ _ -> () in
        let to_do = Queue.create () in
        let final = ref None in
        Queue.push (0, t.bounds, (t.nbasic.(0), Q.minus_inf, Q.inf), final) to_do;
        try
            while true do
                let depth, bounds, new_bound, res = Queue.pop to_do in
                if max_depth > 0 && depth > max_depth then
                    raise Exit;
                (* We can assume res = ref None *)
                try
                    t.bounds <- bounds;
                    add_bounds_imp t new_bound;
                    solve_aux f t;
                    let x = List.find (fun y -> not (is_z (fst (value t y)))) int_vars in
                    let v, _ = value t x in
                    let v' = Z.ediv (Q.num v) (Q.den v) in
                    let under, above = (ref None), (ref None) in
                    res := Some (Branch (x, v', under, above));
                    Queue.push (depth + 1, t.bounds, (x, Q.of_bigint (Z.succ v'), Q.inf), above) to_do;
                    Queue.push (depth + 1, t.bounds, (x, Q.minus_inf, Q.of_bigint v'), under) to_do
                with
                | Not_found ->
                    raise (SolutionFound (get_full_assign t))
                | Unsat x ->
                        res := Some (Explanation (x, List.combine
                            (Array.to_list (find_expr_basic t x)) (Array.to_list t.nbasic)))
                | AbsurdBounds x ->
                        res := Some (Explanation(x, []))
            done;
            raise (UnExpected "")
        with
        | Queue.Empty ->
                Unsatisfiable final
        | SolutionFound sol ->
                Solution sol

    let nsolve t int_vars =
        let init_bounds = t.bounds in
        if Array.length t.nbasic = 0 then
            raise (Invalid_argument "Simplex is empty.");
        let res = nsolve_aux 0 t (List.filter int_vars (Array.to_list t.nbasic @ Array.to_list t.basic)) in
        t.bounds <- init_bounds;
        res

    let nsolve_safe t int_vars =
        let g = global_bound t in
        g, nsolve (bound_all t int_vars g) int_vars

    let base_depth t = 100 + 2 * (Array.length t.nbasic)

    let nsolve_incr t int_vars =
        if Array.length t.nbasic = 0 then
            raise (Invalid_argument "Simplex is empty.");
        let init_bounds = t.bounds in
        let int_vars = (List.filter int_vars (Array.to_list t.nbasic @ Array.to_list t.basic)) in
        let max_depth = ref (base_depth t) in
        let acc = ref None in
        let f () = match !acc with
            | Some _ -> !acc
            | None ->
                    try
                        let res = nsolve_aux !max_depth t int_vars in
                        t.bounds <- init_bounds;
                        acc := Some res;
                        Some (res)
                    with Exit ->
                        max_depth := 2 * !max_depth;
                        t.bounds <- init_bounds;
                        None
        in
        f
    *)

  (* LEGACY implementation of a state printer, using PrintBox

     let pp_to_str f format =
        f Format.str_formatter format;
        Format.flush_str_formatter ()

     let tab_box var_to_string t =
        let a = Array.init (Array.length t.basic + 1) (fun i ->
                Array.init (Array.length t.nbasic + 1) (fun j ->
                    if i = 0 then
                        if j = 0 then
                            "..."
                        else
                            var_to_string (t.nbasic.(j - 1))
                    else
                        if j = 0 then
                            var_to_string (t.basic.(i - 1))
                        else (* i > 0 && j > 0 *)
                            Q.to_string t.tab.(i - 1).(j - 1)
                )) in
        PrintBox.grid_text ~pad:(PrintBox.hpad 1) ~bars:true a

     let bounds_box var_to_string t =
        let a = Array.make_matrix (M.cardinal t.bounds) 5 "<=" in
        let i = ref 0 in
        M.iter (fun x ((l,e1), (u,e2)) ->
            a.(!i).(0) <- Q.to_string l;
            if not (Q.equal e1 Q.zero) then a.(!i).(1) <- "<";
            a.(!i).(2) <- var_to_string x;
            if not (Q.equal e2 Q.zero) then a.(!i).(3) <- "<";
            a.(!i).(4) <- Q.to_string u;
            incr i;
        ) t.bounds;
        PrintBox.grid_text ~pad:(PrintBox.hpad 1) ~bars:false a

     let print_assign print_var fmt l =
        List.iter (fun (x, (c,e)) -> Format.fprintf fmt "%a -> %s + %s e;@ " print_var x (Q.to_string c) (Q.to_string e)) l

     let print_debug print_var fmt t =
        Format.fprintf fmt
            "@[*** System state ***@.%s@.%s@\n@[<hov 2>Current assign:@\n%a@]@\n******** END ********@."
            (PrintBox.to_string (tab_box (pp_to_str print_var) t))
            (PrintBox.to_string (bounds_box (pp_to_str print_var) t))
            (print_assign print_var) (M.bindings t.assign)
  *)

end

module Make_pp(Q : Rat.S)(Var : VAR_PP) = struct
  include Make(Q)(Var)

  let fmt_head = format_of_string "| %6s || "
  let fmt_cell = format_of_string "%6s | "

  let pp_erat out e =
    if Q.equal Q.zero (Erat.eps_factor e)
    then Q.pp out (Erat.base e)
    else
      Format.fprintf out "(@[<h>%a + @<1>ε * %a@])"
        Q.pp (Erat.base e) Q.pp (Erat.eps_factor e)

  let str_of_var = Format.to_string Var.pp
  let str_of_erat = Format.to_string pp_erat
  let str_of_q = Format.to_string Q.pp

  let pp_mat out t =
    let open Format in
    fprintf out "@[<v>";
    (* header *)
    fprintf out fmt_head "";
    Vec.iter (fun x -> fprintf out fmt_cell (str_of_var x)) t.nbasic;
    fprintf out "@,";
    (* rows *)
    for i=0 to Mat.n_row t.tab-1 do
      if i>0 then fprintf out "@,";
      let v = Vec.get t.basic i in
      fprintf out fmt_head (str_of_var v);
      let row = Mat.get_row t.tab i in
      Vec.iter (fun q -> fprintf out fmt_cell (str_of_q q)) row;
    done;
    fprintf out "@]"

  let pp_assign =
    let open Format in
    let pp_pair =
      within "(" ")" @@ hvbox @@ pair ~sep:(return "@ := ") Var.pp pp_erat
    in
    map Var_map.to_seq @@ within "(" ")" @@ hvbox @@ seq pp_pair

  let pp_bounds =
    let open Format in
    let pp_pairs out (x,(l,u)) =
      fprintf out "(@[%a =< %a =< %a@])" pp_erat l Var.pp x pp_erat u
    in
    map Var_map.to_seq @@ within "(" ")" @@ hvbox @@ seq pp_pairs

  let pp_full_state out (t:t) : unit =
    let open Format in
    (* print main matrix *)
    Format.fprintf out
      "(@[<hv>simplex@ :n-row %d :n-col %d@ :mat %a@ :assign %a@ :bounds %a@])"
      (Mat.n_row t.tab) (Mat.n_col t.tab) pp_mat t pp_assign t.assign
      pp_bounds t.bounds
end

module Make_full(Q : Rat.S)(Var : VAR_GEN) = struct
  include Make_pp(Q)(Var)

  type subst = Q.t Var_map.t

  module Expr = struct
    type t = Q.t Var_map.t

    let singleton c x = Var_map.singleton x c
    let singleton1 x = Var_map.singleton x Q.one
    let empty = Var_map.empty
    let is_empty = Var_map.is_empty

    let[@inline] map2 ~f a b =
      Var_map.merge_safe
        ~f:(fun _ rhs -> match rhs with
          | `Left x | `Right x -> Some x
          | `Both (x,y) -> f x y)
        a b

    let[@inline] some_if_nzero x = if Q.equal Q.zero x then None else Some x

    let filter_map ~f m =
      Var_map.fold
        (fun x y m -> match f y with
           | None -> m
           | Some z -> Var_map.add x z m)
        m Var_map.empty

    module Infix = struct
      let (+) = map2 ~f:(fun a b -> some_if_nzero Q.(a + b))
      let (-) = map2 ~f:(fun a b -> some_if_nzero Q.(a - b))
      let ( * ) q = filter_map ~f:(fun x -> some_if_nzero Q.(x * q))
    end

    include Infix

    let of_list l =
      Var_map.of_list (List.rev_map Pair.swap l)
      |> Var_map.filter (fun _ q -> not (Q.equal Q.zero q))
    let to_list e = Var_map.bindings e |> List.rev_map Pair.swap

    let pp_pair = Format.(pair ~sep:(return "@ * ") Q.pp Var.pp)
    let pp out (e:t) = Format.(hovbox @@ list ~sep:(return "@ + ") pp_pair) out (to_list e)

    let eval (subst : subst) (e:t) : Q.t =
      Var_map.fold
        (fun x c acc -> Q.(acc + c * (Var_map.find x subst)))
        e Q.zero
  end

  module Constr = struct
    type op = Leq | Geq | Lt | Gt | Eq

    let pp_op out o =
      Format.string out
        (match o with
          | Leq -> "=<" | Geq -> ">=" | Lt -> "<" | Gt -> ">" | Eq -> "=")

    type t = {
      op: op;
      expr: Expr.t;
      const: Q.t;
    }

    let make op expr const : t = {op;expr;const}
    let op t = t.op
    let expr t = t.expr
    let const t = t.const

    let pp out c =
      Format.fprintf out "(@[%a@ %a %a@])" Expr.pp c.expr pp_op c.op Q.pp c.const

    let eval (subst:subst) (c:t) : bool =
      let v = Expr.eval subst c.expr in
      begin match c.op with
        | Leq -> Q.compare v c.const <= 0
        | Geq -> Q.compare v c.const >= 0
        | Lt -> Q.compare v c.const < 0
        | Gt -> Q.compare v c.const > 0
        | Eq -> Q.compare v c.const = 0
      end
  end

  module Problem = struct
    type t = Constr.t list

    module Infix = struct
      let (&&) = List.append
    end
    include Infix

    let eval subst = List.for_all (Constr.eval subst)

    let pp = Format.(hvbox @@ list ~sep:(return "@ @<1>∧ ") Constr.pp)
  end

  let fresh_var = Var.Fresh.create()

  (* add a constraint *)
  let add_constr (t:t) (c:Constr.t) : unit =
    let (x:basic_var) = Var.Fresh.fresh fresh_var in
    let q = Constr.const c in
    add_eq t (x, Expr.to_list (Constr.expr c));
    begin match c.Constr.op with
      | Constr.Leq -> add_upper_bound t ~strict:false x q
      | Constr.Geq -> add_lower_bound t ~strict:false x q
      | Constr.Lt -> add_upper_bound t ~strict:true x q
      | Constr.Gt -> add_lower_bound t ~strict:true x q
      | Constr.Eq -> add_bounds t ~strict_lower:false ~strict_upper:false (x,q,q)
    end

  let add_problem (t:t) (pb:Problem.t) : unit = List.iter (add_constr t) pb
end

