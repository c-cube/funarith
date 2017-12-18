(*
  copyright (c) 2014-2018, Guillaume Bury, Simon Cruanes
  *)

(* OPTIMS:
 * - distinguish separate systems (that do not interact), such as in { 1 <= 3x = 3y <= 2; z <= 3} ?
 * - Implement gomorry cuts ?
*)

open Containers

module type VAR = Simplex_intf.VAR
module type VAR_GEN = Simplex_intf.VAR_GEN

module type S = Simplex_intf.S
module type S_FULL = Simplex_intf.S_FULL

module Vec = CCVector

module Matrix : sig
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

(* use non-polymorphic comparison ops *)
open Int.Infix

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

  type t = {
    tab : Q.t Matrix.t; (* the matrix of coefficients *)
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
    tab = Matrix.create ();
    basic = Vec.create ();
    nbasic = Vec.create ();
    assign = M.empty;
    bounds = M.empty;
    idx_basic = M.empty;
    idx_nbasic = M.empty;
  }

  let copy t = {
    tab = Matrix.copy t.tab;
    basic = Vec.copy t.basic;
    nbasic = Vec.copy t.nbasic;
    assign = t.assign;
    bounds = t.bounds;
    idx_nbasic = t.idx_nbasic;
    idx_basic = t.idx_basic;
  }

  let index_basic (t:t) (x:basic_var) : int =
    match M.find x t.idx_basic with
      | n -> n
      | exception Not_found -> -1

  let index_nbasic (t:t) (x:nbasic_var) : int =
    match M.find x t.idx_nbasic with
      | n -> n
      | exception Not_found -> -1

  let[@inline] mem_basic (t:t) (x:var) : bool = M.mem x t.idx_basic
  let[@inline] mem_nbasic (t:t) (x:var) : bool = M.mem x t.idx_nbasic

  (* check invariants, for test purposes *)
  let check_invariants (t:t) : bool =
    Matrix.check_invariants t.tab &&
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
  let find_expr_basic_opt t (x:var) : Q.t Vec.vector option =
    begin match index_basic t x with
      | -1 -> None
      | i -> Some (Matrix.get_row t.tab i)
    end

  let find_expr_basic t (x:basic_var) : Q.t Vec.vector =
    match find_expr_basic_opt t x with
      | None -> unexpected "Trying to find an expression for a non-basic variable."
      | Some e -> e

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
  let find_coef (t:t) (x:basic_var) (y:nbasic_var) : Q.t =
    begin match index_basic t x, index_nbasic t y with
      | -1, _ | _, -1 -> assert false
      | i, j -> Matrix.get t.tab i j
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
    let old_dim = Matrix.n_col t.tab in
    List.iter (fun _ -> Matrix.push_col t.tab Q.zero) l;
    assert (old_dim + List.length l = Matrix.n_col t.tab);
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
    assert (Matrix.n_col t.tab > 0);
    Matrix.push_row t.tab Q.zero;
    let row_i = Matrix.n_row t.tab - 1 in
    assert (row_i >= 0);
    (* now put into the row the coefficients corresponding to [eq],
       expanding basic variables to their definition *)
    List.iter
      (fun (c, x) ->
         let expr = find_expr_total t x in
         assert (Vec.length expr = Matrix.n_col t.tab);
         Vec.iteri
           (fun j c' ->
              if not (Q.equal Q.zero c') then (
                Matrix.set t.tab row_i j Q.(Matrix.get t.tab row_i j + c * c')
              ))
           expr)
      eq;
    ()

  (* add bounds to [x] in [t] *)
  let add_bound_aux (t:t) (x:var) (low:Erat.t) (upp:Erat.t) : unit =
    add_vars t [x];
    let l, u = get_bounds t x in
    t.bounds <- M.add x (Erat.max l low, Erat.min u upp) t.bounds

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

  (* full assignment *)
  let full_assign (t:t) : (var * Erat.t) Sequence.t =
    Sequence.append (Vec.to_seq t.nbasic) (Vec.to_seq t.basic)
    |> Sequence.map (fun x -> x, value t x)

  let[@inline] min x y = if Q.compare x y < 0 then x else y
  let[@inline] max x y = if Q.compare x y < 0 then y else x

  (* Find an epsilon that is small enough for finding a solution, yet
     it must be positive.

     {!Erat.t} values are used to turn strict bounds ([X > 0]) into
     non-strict bounds ([X >= 0 + ε]), because the simplex algorithm
     only deals with non-strict bounds.
     When a solution is found, we need to turn {!Erat.t} into {!Q.t} by
     finding a rational value that is small enough that it will fit into
     all the intervals of [t]. This rational will be the actual value of [ε].
  *)
  let solve_epsilon (t:t) : Q.t =
    let emax =
      M.fold
        (fun x ({base=low;eps_factor=e_low}, {base=upp;eps_factor=e_upp}) emax ->
           let {base=v; eps_factor=e_v} = value t x in
           (* lower bound *)
           let emax =
             if Q.compare low Q.minus_inf > 0 && Q.compare e_v e_low < 0
             then min emax Q.((low - v) / (e_v - e_low))
             else emax
           in
           (* upper bound *)
           if Q.compare upp Q.inf < 0 && Q.compare e_v e_upp > 0
           then min emax Q.((upp - v) / (e_v - e_upp))
           else emax)
        t.bounds
        Q.inf
    in
    if Q.compare emax Q.one >= 0 then Q.one else emax

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
    (* Matrixrix Pivot operation *)
    let kx = index_basic t x in
    let ky = index_nbasic t y in
    for j = 0 to Vec.length t.nbasic - 1 do
      if Var.compare y (Vec.get t.nbasic j) = 0 then (
        Matrix.set t.tab kx j Q.(one / a)
      ) else (
        Matrix.set t.tab kx j Q.(neg (Matrix.get t.tab kx j) / a)
      )
    done;
    for i = 0 to Vec.length t.basic - 1 do
      if i <> kx then (
        let c = Matrix.get t.tab i ky in
        Matrix.set t.tab i ky Q.zero;
        for j = 0 to Vec.length t.nbasic - 1 do
          Matrix.set t.tab i j Q.(Matrix.get t.tab i j + c * Matrix.get t.tab kx j)
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

  (* add [c·x] to [m] *)
  let add_expr_ (x:var) (c:Q.t) (m:Q.t M.t) =
    let c' = M.get_or ~default:Q.zero x m in
    let c' = Q.(c + c') in
    if Q.equal Q.zero c' then M.remove x m else M.add x c' m

  (* dereference basic variables from [c·x], and add the result to [m] *)
  let rec deref_var_ t x c m = match find_expr_basic_opt t x with
    | None -> add_expr_ x c m
    | Some expr_x ->
      let m = ref m in
      Vec.iteri
        (fun i c_i ->
           let y_i = Vec.get t.nbasic i in
           m := deref_var_ t y_i Q.(c * c_i) !m)
        expr_x;
      !m

  let check_cert (t:t) (c:cert) : bool =
    let x = c.cert_var in
    let low_x, up_x = get_bounds t x in
    begin match c.cert_expr with
      | [] ->
        Erat.compare low_x up_x > 0
      | (c1,y1) :: tail ->
        let e0 = deref_var_ t x (Q.neg Q.one) M.empty in
        (* compute bounds for the expression [c.cert_expr],
           and also compute [c.cert_expr - x] to check if it's 0] *)
        let scale_bounds c (l,u) =
          (* maybe invert bounds, if [c < 0] *)
          if Q.compare c Q.zero < 0
          then Erat.mul c u, Erat.mul c l
          else Erat.mul c l, Erat.mul c u
        in
        let ly, uy = scale_bounds c1 (get_bounds t y1) in
        let low, up, expr_minus_x =
          List.fold_left
            (fun (l,u,expr_minus_x) (c, y) ->
               let ly, uy = scale_bounds c (get_bounds t y) in
               let expr_minus_x = deref_var_ t y c expr_minus_x in
               Erat.sum l ly, Erat.sum u uy, expr_minus_x)
              (ly, uy, deref_var_ t y1 c1 e0)
              tail
        in
        (* check that the expanded expression is [x], and that
           one of the bounds on [x] is incompatible with bounds of [c.cert_expr] *)
        M.is_empty expr_minus_x &&
        (Erat.compare low_x up > 0 || Erat.compare up_x low < 0)
    end

  (** External access functions *)

  let get_tab t =
    Vec.to_list t.nbasic,
    Vec.to_list t.basic,
    Matrix.to_list t.tab

  let get_assign_map t = M.map Erat.base t.assign
  let get_assign t = List.rev_map (fun (x, v) -> x,v.base) (M.bindings t.assign)

  let get_bounds t x =
    let l, u = get_bounds t x in
    l.base, u.base

  let get_all_bounds t =
    List.map
      (fun (x,(l,u)) -> (x, (l.base, u.base)))
      (M.bindings t.bounds)

  (* printer *)

  let fmt_head = format_of_string "|%8s|| "
  let fmt_cell = format_of_string "%8s| "

  let pp_erat out e =
    if Q.equal Q.zero (Erat.eps_factor e)
    then Q.pp out (Erat.base e)
    else
      Format.fprintf out "(@[<h>%a + @<1>ε * %a@])"
        Q.pp (Erat.base e) Q.pp (Erat.eps_factor e)

  let pp_cert out (c:cert) = match c.cert_expr with
    | [] -> Format.fprintf out "(@[inconsistent-bounds %a@])" Var.pp c.cert_var
    | _ ->
      let pp_pair = Format.(pair ~sep:(return "@ * ") Q.pp Var.pp) in
      Format.fprintf out "(@[<hv>cert@ :var %a@ :linexp %a@])"
        Var.pp c.cert_var
        Format.(within "[" "]" @@ hvbox @@ list ~sep:(return "@ + ") pp_pair)
        c.cert_expr

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
    for i=0 to Matrix.n_row t.tab-1 do
      if i>0 then fprintf out "@,";
      let v = Vec.get t.basic i in
      fprintf out fmt_head (str_of_var v);
      let row = Matrix.get_row t.tab i in
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
      (Matrix.n_row t.tab) (Matrix.n_col t.tab) pp_mat t pp_assign t.assign
      pp_bounds t.bounds

end

module Make_full(Q : Rat.S)(Var : VAR_GEN) = struct
  include Make(Q)(Var)

  type subst = Q.t Var_map.t

  module Expr = struct
    type t = Q.t Var_map.t

    let empty = Var_map.empty

    let singleton c x =
      if Q.compare c Q.zero <> 0 then Var_map.singleton x c else empty

    let singleton1 x = Var_map.singleton x Q.one

    let add c x e =
      let c' = M.get_or ~default:Q.zero x e in
      let c' = Q.(c + c') in
      if Q.equal Q.zero c' then M.remove x e else M.add x c' e

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

    let of_list l = List.fold_left (fun e (c,x) -> add c x e) empty l
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

