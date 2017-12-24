
open OUnit

let suite =
  "libarith" >::: [
    "diophantine" >::: Test_diophantine.suite;
  ]

let props =
  List.flatten
    [ 
      Test_diophantine.props;
      Test_simplex.props;
      Test_prime.props;
    ]

let () =
  CCFormat.set_color_default true;
  ignore (OUnit.run_test_tt suite);
  QCheck_runner.run_tests_main props

