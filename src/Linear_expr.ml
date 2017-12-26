(*
  copyright (c) 2014-2018, Guillaume Bury, Simon Cruanes
  *)

module type COEF = Linear_expr_intf.COEF

module type VAR = Linear_expr_intf.VAR
module type FRESH = Linear_expr_intf.FRESH
module type VAR_GEN = Linear_expr_intf.VAR_GEN

  module type S = Linear_expr_intf.S

module Make(C : COEF)(Var : VAR) = struct
  module C = C
  module Var_map = CCMap.Make(Var)
  module Var = Var

  type var = Var.t
  type subst = C.t Var_map.t

  (** Linear combination of variables. *)
  module Comb = struct
    (* A map from variables to their coefficient in the linear combination. *)
    type t = C.t Var_map.t

    let compare = Var_map.compare C.compare

    let empty = Var_map.empty

    let is_empty = Var_map.is_empty

    let monomial c x =
      if C.equal c C.zero then empty else Var_map.singleton x c

    let monomial1 x = Var_map.singleton x C.one

    let add c x e =
      let c' = Var_map.get_or ~default:C.zero x e in
      let c' = C.(c + c') in
      if C.equal C.zero c' then Var_map.remove x e else Var_map.add x c' e

    let[@inline] map2 ~f a b =
      Var_map.merge_safe
        ~f:(fun _ rhs -> match rhs with
            | `Left x | `Right x -> Some x
            | `Both (x,y) -> f x y)
        a b

    let[@inline] some_if_nzero x =
      if C.equal C.zero x then None else Some x

    let filter_map ~f m =
      Var_map.fold
        (fun x y m -> match f y with
           | None -> m
           | Some z -> Var_map.add x z m)
        m Var_map.empty

    module Infix = struct
      let (+) = map2 ~f:(fun a b -> some_if_nzero C.(a + b))
      let (-) = map2 ~f:(fun a b -> some_if_nzero C.(a - b))
      let ( * ) q = filter_map ~f:(fun x -> some_if_nzero C.(x * q))
    end

    include Infix

    let of_list l = List.fold_left (fun e (c,x) -> add c x e) empty l
    let to_list e = Var_map.bindings e |> List.rev_map CCPair.swap

    let to_map e = e
    let of_map e = Var_map.filter (fun _ c -> not (C.equal C.zero c)) e

    let pp_pair =
      CCFormat.(pair ~sep:(return "@ * ") C.pp Var.pp)

    let pp out (e:t) =
      CCFormat.(hovbox @@ list ~sep:(return "@ + ") pp_pair) out (to_list e)

    let eval (subst : subst) (e:t) : C.t =
      Var_map.fold
        (fun x c acc -> C.(acc + c * (Var_map.find x subst)))
        e C.zero
  end

  module Expr = struct
    type t = {
      const : C.t;
      comb : Comb.t
    }

    let[@inline] const e = e.const
    let[@inline] comb e = e.comb

    let compare e e' =
      CCOrd.(C.compare e.const e'.const
             <?> (Comb.compare, e.comb, e'.comb))

    let pp fmt e =
      Format.fprintf fmt "@[<hov>%a@ + %a" Comb.pp e.comb C.pp e.const

    let[@inline] make comb const : t = { comb; const; }

    let of_const = make Comb.empty
    let of_comb c = make c C.zero
    let of_list l = make (Comb.of_list l) C.zero
    let zero = of_const C.zero

    let is_zero e = C.equal C.zero e.const  && Comb.is_empty e.comb

    let map2 f g e e' = make (f e.comb e'.comb) (g e.const e'.const)

    module Infix = struct
      let (+) = map2 Comb.(+) C.(+)
      let (-) = map2 Comb.(-) C.(-)
      let ( * ) c e =
        if C.equal C.zero c
        then zero
        else make Comb.(c * e.comb) C.(c * e.const)
    end
    include Infix

    let eval subst e = C.(e.const + Comb.eval subst e.comb)
  end

  module Constr = struct
    type op = [ `Leq | `Geq | `Lt | `Gt | `Eq | `Neq ]

    (** Constraints are expressions implicitly compared to zero. *)
    type +'a t = {
      expr: Expr.t;
      op: 'a;
    } constraint 'a = [< op ]

    let compare c c' =
      CCOrd.(compare c.op c'.op
             <?> (Expr.compare, c.expr, c'.expr))

    let pp_op out o =
      CCFormat.string out (match o with
        | `Leq -> "=<" | `Geq -> ">=" | `Lt -> "<"
        | `Gt -> ">" | `Eq -> "=" | `Neq -> "!=")

    let pp out c =
      Format.fprintf out "(@[%a@ %a 0@])"
        Expr.pp c.expr pp_op c.op

    let op t = t.op
    let expr t = t.expr

    let of_expr expr op = { expr; op; }

    let make comb op const =
      of_expr (Expr.make comb (C.neg const)) op

    let split { expr = Expr.{const; comb; } ; op; } =
      comb, op, C.neg const

    let eval subst c =
      let v = Expr.eval subst c.expr in
      begin match c.op with
        | `Leq -> C.compare v C.zero <= 0
        | `Geq -> C.compare v C.zero >= 0
        | `Lt -> C.compare v C.zero < 0
        | `Gt -> C.compare v C.zero > 0
        | `Eq -> C.compare v C.zero = 0
        | `Neq -> C.compare v C.zero <> 0
      end
  end
end