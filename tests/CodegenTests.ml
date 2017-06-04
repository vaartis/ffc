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
          );

          "If statement" >:: (fun ctxt ->
            assert_command
              ~exit_code:(Unix.WEXITED 19)
              ~sinput:(Stream.of_string @@ create_mod "fnc main() int { int x = 0; if false { x = 10; } else { x = 19; } ret x; }") ~ctxt "lli" []
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
          );

          "Operator usage" >:: (fun ctxt ->
            assert_command
              ~exit_code:(Unix.WEXITED 16)
              ~sinput:(Stream.of_string @@ create_mod "operator ++(int x, int y) int { ret x + y + 1; } fnc main() int { ret 8 ++ 7; } ") ~ctxt "lli" []
          );

           "Type function call" >:: (fun ctxt ->
            assert_command
              ~exit_code:(Unix.WEXITED 17)
              ~sinput:(Stream.of_string @@ create_mod "type T { int x } implement for T { fnc pl_some(int x) int { ret self.x + x; } }\
                                                       fnc main() int { ret T{ x = 10 }.pl_some(7); }") ~ctxt "lli" []
          );

           "If expression" >:: (fun ctxt ->
            assert_command
              ~exit_code:(Unix.WEXITED 18)
              ~sinput:(Stream.of_string @@ create_mod "fnc main() int { ret if true { if false { 0 } else { 18 } } else { 0 }; }") ~ctxt "lli" []
           );

           "Mixin usage" >:: (fun ctxt ->
            assert_command
              ~exit_code:(Unix.WEXITED 19)
              ~sinput:(Stream.of_string @@ create_mod "mixin mix { fnc test() int { ret 1; } } type test_ty with mixin mix { int x }\
                                                       implement for test_ty { fnc chew() int { ret 18; } } fnc main() int {\
                                                       test_ty x = test_ty { x = 10 }; ret x.test() + x.chew(); }") ~ctxt "lli" []
           );
        ]
    ]
