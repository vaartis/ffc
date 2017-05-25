open AST;;
open Lexer;;
open Parser;;

let set_filename (fname:string) (lexbuf:Lexing.lexbuf) =
  ( lexbuf.Lexing.lex_curr_p <-
      { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fname }
  ; lexbuf
  );;

let _ =
  let lexbuf = set_filename "stdin" @@ Lexing.from_channel stdin in
  let ast = Parser.main Lexer.token lexbuf in
  print_string @@ string_of_int @@ List.length ast;;
