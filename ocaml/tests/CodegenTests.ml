open OUnit2;;
open Llvm;;

let codegen_tests =
  "Codegen tests" >:::
    [
      "Return an integer" >:: (fun ctxt -> let ast = ParserHelpers.parse_str "fnc main() int { ret 10; }" in
                                           let code = Codegen.codegen ast in
                                           let st = string_of_llmodule code in
                                           assert_command ~exit_code:(Unix.WEXITED 10) ~sinput:(Stream.of_string st) ~ctxt "lli" []
                              )
    ]
