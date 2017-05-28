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

let ast = parse () in
    let context = global_context () in
    let modl = create_module context "stdin" in
    let builder = builder context in

    let functions = Hashtbl.create 1 in
    let curr_variables = Hashtbl.create 0 in

    let get_llvm_type tp =
      match tp with
      | Int -> i32_type context
      | Float -> float_type context
      | Str -> pointer_type @@ i8_type context
      | Void -> void_type context in

    let mangle fn =
      let trav_ar_list x = List.fold_left (fun acc (_, tp) ->
                               let ltp = get_llvm_type tp in
                               acc ^ "A" ^ (match (classify_type ltp) with
                                            | Llvm.TypeKind.Struct -> begin
                                                match struct_name ltp with
                                                | Some nm -> (string_of_int @@ String.length nm) ^ nm
                                                | None -> assert false
                                              end
                                            | _ -> begin
                                                let ltp_s = string_of_lltype ltp in
                                                (string_of_int @@ String.length ltp_s) ^ ltp_s
                                              end
                             )) "" x in
      let (tp, name, arg_str, ret_t) = match fn with
      | FncDef { FncDef.name; args; ret_t; _ } -> ("F", name, trav_ar_list args, ret_t)
      | OperatorDef { FncDef.name; args; ret_t; _ } -> ("O", name, trav_ar_list args, ret_t)
      | _ -> assert false in
      let ret_s =
        let ltp_s = string_of_lltype @@ get_llvm_type ret_t in
        "R" ^ (string_of_int @@ String.length ltp_s) ^ ltp_s in

      "_FF" ^ tp ^ (* TODO: types*)"" ^
        "N" ^ (string_of_int @@ String.length name) ^ name ^ arg_str ^ ret_s in

    let gen_expr ex =
      match ex with
      | IntLit x -> let open AST.Int in
                    const_int (get_llvm_type Int) x.value
      | FloatLit x -> let open AST.Float in
                      const_float (get_llvm_type Float) x.value
      | StrLit x -> let open AST.Str in
                    const_string context x.value
    in

    let gen_stmt st =
      match st with
      | Decl x -> begin
          let open AST.Decl in
          let alloca = build_alloca (get_llvm_type x.tp) x.name builder in
          begin match x.value with
          | Some v -> ignore(build_store alloca (gen_expr v) builder)
          | None -> ()
          end;
          Hashtbl.replace curr_variables x.name alloca;
        end
      | ExprAsStmt x -> ignore(gen_expr x)
    in

    List.iter (fun node ->
        match node with
        | FncDef x | OperatorDef x -> begin
            let open AST.FncDef in
            let m_name = if x.name <> "main" then mangle node else x.name in
            let ftype = function_type (get_llvm_type x.FncDef.ret_t) (Array.of_list @@ List.map (fun (x,y) -> get_llvm_type y) x.args) in
            let fn = define_function m_name ftype modl in
            position_at_end (entry_block fn) builder;

            List.iter gen_stmt x.body;

            Hashtbl.replace functions m_name fn;
          end
    ) ast; dump_module modl;;
