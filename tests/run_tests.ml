open OUnit2;;

let _ =
  run_test_tt_main CodegenTests.codegen_tests;;
