open ParserHelpers;;
open Codegen;;
open Llvm;;

let _ =
  let ast = parse () in
  let modu = codegen ast in
  dump_module @@ modu;
  Llvm_analysis.assert_valid_module modu;;
