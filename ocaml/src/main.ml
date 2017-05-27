open Llvm;;
open AST;;

let set_filename (fname:string) (lexbuf:Lexing.lexbuf) =
  ( lexbuf.Lexing.lex_curr_p <-
      { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fname }
  ; lexbuf
  );;

let parse ?file () = let (fname, channel) = match file with
                       | None -> ("stdin", stdin)
                       | Some x -> (x, open_in x) in
                     let lexbuf = set_filename fname @@ Lexing.from_channel channel in
                     Parser.main Lexer.token lexbuf;;

let functions = Hashtbl.create 1;;
let curr_variables = Hashtbl.create 0;;

let ast = parse () in
    let context = global_context () in
    let modl = create_module context "stdin" in
    let builder = builder context in

    let get_llvm_type tp =
      match tp with
      | Int -> i32_type context
      | Float -> float_type context
      | Str -> pointer_type @@ i8_type context
      | Void -> void_type context in

    let gen_expr ex =
      match ex with
      | IntLit x -> const_int (get_llvm_type Int) x.value in

    let gen_stmt st =
      match st with
      | Decl x -> begin
          let alloca = build_alloca (get_llvm_type x.tp) x.name builder in
          begin match x.value with
          | Some v -> build_store alloca (gen_expr v) builder; ()
          | None -> ()
          end;
          Hashtbl.replace curr_variables x.name alloca;
        end in

    List.iter (fun node ->
        match node with
        | FncDef x -> begin
            let ftype = function_type (get_llvm_type x.ret_t) (Array.of_list @@ List.map (fun (x,y) -> get_llvm_type y) x.args) in
            let fn = define_function x.name ftype modl in
            position_at_end (entry_block fn) builder;

            List.iter gen_stmt x.body;

            Hashtbl.replace functions x.name fn;
            dump_module modl;
          end
    ) ast;;
