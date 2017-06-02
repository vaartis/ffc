open ParserHelpers;;
open Codegen;;
open Llvm;;

let _ =
  let ast = parse () in
  let modu = codegen ast in
  Llvm_analysis.assert_valid_module modu;

  let pmgr = PassManager.create () in
  Llvm_ipo.add_always_inliner pmgr;
  ignore(PassManager.run_module modu pmgr);
  dump_module @@ modu;;
