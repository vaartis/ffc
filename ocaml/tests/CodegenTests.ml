open OUnit2;;
open Llvm;;

let create_mod text = let ast = ParserHelpers.parse_str text in
                      let code = Codegen.codegen ast in
                      string_of_llmodule code;;

let codegen_tests =
  "Codegen tests" >:::
    [
      "Statements" >:::
        [
          "Return an integer" >:: (fun ctxt ->
            assert_command
              ~exit_code:(Unix.WEXITED 10)
              ~sinput:(Stream.of_string @@ create_mod "fnc main() int { ret 10; }") ~ctxt "lli" []
          );
          "Load from variable" >:: (fun ctxt ->
            assert_command
              ~exit_code:(Unix.WEXITED 11)
              ~sinput:(Stream.of_string @@ create_mod "fnc main() int { int x = 11; int y = x; ret y; }") ~ctxt "lli" []
          );
          "Variable assignment" >:: (fun ctxt ->
            assert_command
              ~exit_code:(Unix.WEXITED 13)
              ~sinput:(Stream.of_string @@ create_mod "fnc main() int { int x = 10; x = 13; ret x; }") ~ctxt "lli" []
          );
          "Type field assignment" >:: (fun ctxt ->
            assert_command
              ~exit_code:(Unix.WEXITED 16)
              ~sinput:(Stream.of_string @@ create_mod "type T { int x } fnc main() int { T x = T{ x = 10 }; x.x = 16; ret x.x; }") ~ctxt "lli" []
          )
        ];
      "Expressions" >:::
        [
          "Function call" >:: (fun ctxt ->
            assert_command
              ~exit_code:(Unix.WEXITED 12)
              ~sinput:(Stream.of_string @@ create_mod "fnc test(int x) int { ret x; } fnc main() int { ret test(12); }") ~ctxt "lli" []
          );

          "Type literal and field load" >:: (fun ctxt ->
            assert_command
              ~exit_code:(Unix.WEXITED 14)
              ~sinput:(Stream.of_string @@ create_mod "type T { int x, int y } fnc main() int { T x = T{ x = 14, y = 0}; ret x.x; }") ~ctxt "lli" []
          );

          "Construct type literal and return it's field in place" >:: (fun ctxt ->
            assert_command
              ~exit_code:(Unix.WEXITED 15)
              ~sinput:(Stream.of_string @@ create_mod "type T { int x, int y } fnc main() int { ret T{ x = 1, y = 15}.y; }") ~ctxt "lli" []
          )
        ]
    ]
