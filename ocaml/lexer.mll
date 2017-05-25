{
  open Parser;;
  exception Eof;;
  let filename = ref "";;
}

rule token = parse
     [' ' '\t' '\n'] { token lexbuf }
     | [';'] { SEMICOLON }
     | ['0' - '9']+ { INT(int_of_string (Lexing.lexeme lexbuf)) }
     | ['0' - '9'] '.' ['0' - '9'] { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
     | "fnc" { FNC }
     | "include" { INCLUDE }
     | '\"' ( [^'\"']+ as st ) '\"' { STR(st) }
     | eof { EOF }
     | '(' { OP_P }
     | ')' { CL_P }
     | '{' { OP_CB }
     | '}' { CL_CB }
     | ("int")|("float")|("str") { TYPE(Lexing.lexeme lexbuf) }
     | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0' - '9']* { IDENT(Lexing.lexeme lexbuf) }

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
