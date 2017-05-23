{
  open Parser;;
  exception Eof;;
  let filename = ref "";;
}

rule token = parse
     [' ' '\t' '\n'] { token lexbuf }
     | [';'] { SEMICOLON }
     | ['0' - '9']+ { INT(int_of_string (Lexing.lexeme lexbuf)) }
     | "fnc" { FNC }
     | "include" { INCLUDE }
     | '\"' ( [^'\"']+ as st ) '\"' { STR(st) }
     | eof { EOF  }

{
let sprintf = Printf.sprintf;;

(*let to_string = function
  | FNC -> sprintf "FNC"
  | STR(str) -> sprintf "STR(%s)" str
  | SEMICOLON -> sprintf ";"
  | INT(va) -> sprintf "INT(%i)" va
  | INCLUDE -> sprintf "INCLUDE"
  | EOF -> sprintf "EOF" *)
}
