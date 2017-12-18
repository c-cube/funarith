
module QC = QCheck

module Var = CCInt
module Spl = Funarith_zarith.Simplex.Make(Var)

let rand_n low n : Z.t QC.arbitrary =
  QC.map ~rev:Z.to_int Z.of_int QC.(low -- n)

let rand_q : Q.t QC.arbitrary =
  let n1 = rand_n ~-100 100 in
  let n2 = rand_n 1 50 in
  QC.map ~rev:(fun q -> Q.num q, Q.den q)
    (fun (x,y) -> Q.make x y)
    (QC.pair n1 n2)

(* TODO: random generator of problems *)
(* TODO: test soundness (if solution, then it's correct) *)

let props = [
]
