
open Containers
module QC = QCheck
module D = Funarith_zarith.Diophantine

let mk_arr = Array.map Z.of_int
let mk_ar2 = Array.map mk_arr

module S = D.Homogeneous_system

let test_example () =
  (* example from the paper *)
  let eqns =
    S.make
      ([| [| -1; 1; 2; -3 |];
          [| -1; 3; -2; -1 |];
       |] |> mk_ar2)
  in
  let expected_sols = [
    [| 0; 1; 1; 1 |];
    [| 4; 2; 1; 0 |];
  ] |> List.map mk_arr in
  let sols = S.solve eqns |> Sequence.to_list in
  OUnit.assert_equal ~printer:string_of_int ~msg:"num solutions" 2 (List.length sols);
  OUnit.assert_bool "solutions non trivial"
    (List.for_all (Array.exists (fun x->not (Z.equal Z.zero x))) sols);
  OUnit.assert_equal ~cmp:Equal.(list @@ array Z.equal)
    ~printer:Format.(to_string @@ list @@ array Z.pp_print)
    expected_sols sols;
  ()

let suite = OUnit.( [
  "test_example" >:: test_example;
])

(* properties *)

let rand_n low n : Z.t QC.arbitrary =
  QC.map ~rev:Z.to_int Z.of_int QC.(low -- n)

let rand_eqn ~len ~range:n : Z.t array QC.arbitrary =
  QC.array_of_size (QC.Gen.return len) @@ rand_n (-n) n

let rand_system_ len_eqns : S.t QC.arbitrary =
  QC.map ~rev:S.eqns S.make @@
  QC.array_of_size QC.Gen.(1 -- 10) @@ rand_eqn ~len:len_eqns ~range:6

(* custom generator for systems: pick length of equations, then geenrate
   equations of the same size *)
let rand_system : S.t QC.arbitrary =
  let base = (rand_system_ 10) in
  let gen = QC.Gen.(1 -- 6 >>= fun n -> (rand_system_ n).QC.gen) in
  QC.set_gen gen base

(* find if there's an equation not satisfied by [sol] *)
let find_bad_eqn (sys:S.t) (sol:D.solution) : _ option =
  CCArray.find
    (fun eqn ->
       assert (Array.length eqn = Array.length sol);
       let sum = Array.fold2 (fun acc x y -> Z.(acc + x * y)) Z.zero eqn sol in
       if Z.equal Z.zero sum then None else Some eqn)
    (S.eqns sys)

let solver_correct =
  let prop sys =
    (* Format.printf "check %a@." S.pp sys; *)
    let sols = S.solve_l sys in
    begin match
        CCList.find_map
          (fun sol -> Option.map (fun e->sol,e) @@ find_bad_eqn sys sol)
          sols
      with
        | None -> true
        | Some (sol,bad_e) ->
          QC.Test.fail_reportf
            "(@[diophantine.bad_solution@ :system %a@ :sol %a@ :bad-eqn %a@])"
            S.pp sys S.pp_sol sol S.pp_sol bad_e
    end
  in
  QC.Test.make ~name:"diophantine_solver_correct" ~count:100 ~long_factor:5 rand_system prop

let props = [
  solver_correct;
]
