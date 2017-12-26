
open Containers

module QC = QCheck

module Var = struct
  include CCInt

  let pp out x = Format.fprintf out "X_%d" x

  let rand n : t QC.arbitrary = QC.make ~print:(Format.to_string pp) @@ QC.Gen.(0--n)
end

module Fresh = struct
  type var = Var.t
  type t = int ref
  let create() = ref ~-1
  let fresh r = decr r; !r
end

module L = Funarith.Expr.Linear.Make(Funarith_zarith.Rat)(Var)

module Spl = Funarith_zarith.Simplex.Make_full(Fresh)(L)

let rand_n low n : Z.t QC.arbitrary =
  QC.map ~rev:Z.to_int Z.of_int QC.(low -- n)

let rand_q : Q.t QC.arbitrary =
  let n1 = rand_n ~-100 100 in
  let n2 = rand_n 1 50 in
  let qc =
    QC.map ~rev:(fun q -> Q.num q, Q.den q)
      (fun (x,y) -> Q.make x y)
      (QC.pair n1 n2)
  in
  (* avoid [undef] when shrinking *)
  let shrink q yield =
    Option.get_exn qc.QC.shrink q (fun x -> if Q.is_real x then yield x)
  in
  QC.set_shrink shrink qc

type subst = Q.t Spl.Var_map.t

(* NOTE: should arrive in qcheck at some point *)
let filter_shrink (f:'a->bool) (a:'a QC.arbitrary) : 'a QC.arbitrary =
  match a.QC.shrink with
    | None -> a
    | Some shr ->
      let shr' x yield = shr x (fun y -> if f y then yield y) in
      QC.set_shrink shr' a

module Expr = struct
  include Spl.L.Comb

  let rand n : t QC.arbitrary =
    (* generate non-empty expressions *)
    let a =
      QC.map_same_type (fun e -> if is_empty e then monomial1 0 else e) @@
      QC.map ~rev:to_list of_list @@
      QC.list_of_size QC.Gen.(1--n) @@ QC.pair rand_q (Var.rand 10)
    in
    filter_shrink (fun e -> not (is_empty e)) a
end

module Constr = struct
  include Spl.L.Constr

  let shrink c : _ t QC.Iter.t =
    let open QC.Iter in
    append
      (Option.map_or ~default:empty
         (fun s -> s c.expr >|= fun expr -> {c with expr})
         (Expr.rand 5).QC.shrink)
      (Option.map_or ~default:empty
         (fun s -> s c.const >|= fun const -> {c with const}) rand_q.QC.shrink)

  let rand n : t QC.arbitrary =
    let gen =
      QC.Gen.(
        return make <*>
          oneofl [Leq;Geq;Lt;Gt;Eq] <*>
          (Expr.rand n).QC.gen <*>
          rand_q.QC.gen)
    in
    QC.make ~print:(Format.to_string pp) ~shrink gen
end

module Problem = struct
  include Spl.Problem

  let rand ?(min=3) n : t QC.arbitrary =
    let n = max min (max n 6) in
    QC.list_of_size QC.Gen.(min -- n) @@ Constr.rand 10

end

let pp_subst : Spl.subst Format.printer =
  Format.(map Spl.Var_map.to_seq @@
    within "{" "}" @@ hvbox @@ seq ~sep:(return ",@ ") @@
    pair ~sep:(return "@ @<1>→ ") Var.pp Q.pp_print
  )

let check_invariants =
  let prop pb =
    let simplex = Spl.create() in
    Spl.add_problem simplex pb;
    Spl.check_invariants simplex
  in
  QC.Test.make ~long_factor:10 ~count:50 ~name:"simplex_invariants" (Problem.rand 20) prop

let check_invariants_after_solve =
  let prop pb =
    let simplex = Spl.create() in
    Spl.add_problem simplex pb;
    ignore (Spl.solve simplex);
    if Spl.check_invariants simplex then true
    else (
      QC.Test.fail_reportf "(@[bad-invariants@ %a@])" Spl.pp_full_state simplex
    )
  in
  QC.Test.make ~long_factor:10 ~count:50 ~name:"simplex_invariants_after_solve" (Problem.rand 20) prop

let check_sound =
  let prop pb =
    let simplex = Spl.create() in
    Spl.add_problem simplex pb;
    let old_simp = Spl.copy simplex in
    begin match Spl.solve simplex with
      | Spl.Solution subst ->
        if Problem.eval subst pb then true
        else (
          QC.Test.fail_reportf
            "(@[<hv>bad-solution@ :problem %a@ :sol %a@ :simplex-after  %a@ :simplex-before %a@])"
            Problem.pp pb pp_subst subst Spl.pp_full_state simplex Spl.pp_full_state old_simp
        )
      | Spl.Unsatisfiable cert ->
        begin match Spl.check_cert simplex cert with
          | `Ok -> true
          | `Bad_bounds (low, up) ->
            QC.Test.fail_reportf
              "(@[<hv>bad-certificat@ :problem %a@ :cert %a@ :low %s :up %s@ :simplex-after  %a@ :simplex-before %a@])"
              Problem.pp pb Spl.pp_cert cert low up Spl.pp_full_state simplex Spl.pp_full_state old_simp
          | `Diff_not_0 e ->
            QC.Test.fail_reportf
              "(@[<hv>bad-certificat@ :problem %a@ :cert %a@ :diff %a@ :simplex-after  %a@ :simplex-before %a@])"
              Problem.pp pb Spl.pp_cert cert Expr.pp e Spl.pp_full_state simplex Spl.pp_full_state old_simp
        end
    end
  in
  QC.Test.make ~long_factor:10 ~count:2_000 ~name:"simplex_sound" (Problem.rand 20) prop

let check_scalable =
  let prop pb =
    let simplex = Spl.create() in
    Spl.add_problem simplex pb;
    ignore (Spl.solve simplex);
    true
  in
  QC.Test.make ~long_factor:2 ~count:20 ~name:"simplex_scalable" (Problem.rand ~min:100 100) prop


let props = [
  check_invariants;
  check_sound;
  check_scalable;
]
