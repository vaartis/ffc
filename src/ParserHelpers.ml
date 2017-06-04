let set_filename (fname:string) (lexbuf:Lexing.lexbuf) =
  ( lexbuf.Lexing.lex_curr_p <-
      { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fname }
  ; lexbuf
  );;

let parse_str st =
  let lexbuf = set_filename "<string>" @@ Lexing.from_string st in
    Parser.main Lexer.token lexbuf;;

let parse ?file () =
  let (fname, channel) = match file with
    | None -> ("<stdin>", stdin)
    | Some x -> (x, open_in x) in
  let lexbuf = set_filename fname @@ Lexing.from_channel channel in
  Parser.main Lexer.token lexbuf;;
