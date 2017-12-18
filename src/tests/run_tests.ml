
open OUnit

module D = Funarith_zarith.Diophantine

let diophantine1 () =
  let module E = D.Homogeneous_eqn in
  let e = E.make Z.( [| ~$ 1; ~$ (-2); ~$ 4 |] ) in
  let sol = E.find_a_solution e in
  OUnit.assert_bool "has solutions" (sol <> None);
  let sol = CCOpt.get_exn sol in
  OUnit.assert_bool "solution non trivial"
    begin
      CCArray.exists (fun x -> Z.sign x <> 0) sol
    end;
  OUnit.assert_bool "solution is good"
    begin
      let subst_in_sol = E.compute e sol in
      Z.equal Z.zero @@ subst_in_sol
    end;
  ()


let suite =
  "libarith" >::: [
    "diophantine" >::: [
      "" >:: diophantine1;
    ];
  ]

let props =
  List.flatten
    [ Test_prime.props;
      Test_simplex.props;
    ]

let () =
  CCFormat.set_color_default true;
  ignore (OUnit.run_test_tt suite);
  QCheck_runner.run_tests_main props

