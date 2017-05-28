open ParserHelpers;;
open Codegen;;
open Llvm;;

let _ =
  let ast = parse () in
  dump_module @@ codegen ast;;
