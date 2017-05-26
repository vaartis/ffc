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
    List.iter (fun node ->
        match node with
        | FncDef x -> begin
          end
      ) ast;;
