%{
open AST;;
%}

%token <int> INT
%token <string> STR

%type <AST.toplevel> top_level
%type <AST.toplevel list> top_levels
%type <AST.include_ast> include
%type <string list> strings

%token FNC INCLUDE SEMICOLON EOF

%start top_levels

%%

strings:
    STR { [$1] }
    | STR STR { [$1; $2] }

include:
    INCLUDE strings SEMICOLON { new include_ast $2 }

top_level:
    include { $1 }

top_levels:
    top_level { [$1] }
    | top_level top_level { [$1; $2] }
    | top_level EOF { [$1] }