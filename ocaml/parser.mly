%{
open AST;;
%}

%token <int> INT
%token <string> STR IDENT TYPE
%token <float> FLOAT

%type <AST.toplevel> top_level
%type <AST.toplevel list> main
%type <AST.include_ast> incl
%type <AST.fnc_def_ast> fncdef

%type <AST.expression> expr
%type <AST.statement> stmt

%type <string> str

%token FNC INCLUDE SEMICOLON EOF OP_P CL_P OP_CB CL_CB

%start main

%%

expr:
    INT { new int_ast $1 }
    | FLOAT { new float_ast $1 }
    | STR { new str_ast $1 }

stmt:
    expr { $1 }

str: STR { $1 }

incl:
    INCLUDE str+ SEMICOLON { new include_ast $2 }

fncdef:
    FNC IDENT OP_P pair(TYPE, IDENT)* CL_P TYPE? OP_CB stmt* CL_CB {
                              new fnc_def_ast $2 (List.map (fun (x,y) -> (x,ttype_of_string y)) $4) $8 (match $6 with
                                                                                                        | None -> Void
                                                                                                        | Some x -> ttype_of_string x) }

top_level:
    incl { $1 }
    | fncdef { $1 }

main:
    top_level+ EOF { $1 }