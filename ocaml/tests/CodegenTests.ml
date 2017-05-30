open OUnit2;;
open Llvm;;

let codegen_tests =
  "Codegen tests" >:::
    [
      "Statements" >:::
        [
          "Return an integer" >:: (fun ctxt -> let ast = ParserHelpers.parse_str "fnc main() int { ret 10; }" in
                                               let code = Codegen.codegen ast in
                                               let st = string_of_llmodule code in
                                               assert_command ~exit_code:(Unix.WEXITED 10) ~sinput:(Stream.of_string st) ~ctxt "lli" []
                                  );
          "Load from variable" >:: (fun ctxt -> let ast = ParserHelpers.parse_str "fnc main() int { int x = 11; int y = x; ret y; }" in
                                                let code = Codegen.codegen ast in
                                                let st = string_of_llmodule code in
                                                assert_command ~exit_code:(Unix.WEXITED 11) ~sinput:(Stream.of_string st) ~ctxt "lli" [])
        ]
    ]
