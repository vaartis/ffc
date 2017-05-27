%{
open AST;;
%}

%token <int> INT
%token <string> STR IDENT TYPE OPERATOR
%token <float> FLOAT

%type <AST.toplevel> top_level
%type <AST.toplevel list> main
%type <AST.include_ast> incl
%type <AST.fnc_def_ast> fnc_def
%type <AST.type_def_ast> type_def
%type <AST.extern_ast> extern
%type <AST.operator_def_ast> operator_def
%type <AST.implement_ast> impl
%type <AST.decl_ast> decl

%type <AST.expression> expr
%type <AST.statement> stmt

%type <string> str

%token FNC INCLUDE SEMICOLON COMMA EOF OP_P CL_P OP_CB CL_CB TYPE_KW OPERATOR_KW EXTERN FOR IMPLEMENT EQ

%start main

%%

expr:
    INT { IntLit { value = $1 } }
    | FLOAT { FloatLit { value = $1 } }
    | str { StrLit { value = $1 } }

stmt:
    expr SEMICOLON { ExprAsStmt $1 }
    | decl SEMICOLON { Decl $1 }

decl:
    TYPE IDENT EQ expr { { name = $2; tp = ttype_of_string $1; value = Some $4 } }
    | TYPE IDENT { { name = $2; tp = ttype_of_string $1; value = None } }

str: STR { $1 }

incl:
    INCLUDE str+ { { modules = $2 } }

fnc_def:
    FNC IDENT OP_P separated_list(COMMA, pair(TYPE, IDENT)) CL_P TYPE? OP_CB stmt* CL_CB {
                              { name = $2; args = (List.map (fun (x,y) -> (y, ttype_of_string x)) $4); body = $8; ret_t = (match $6 with
                                                                                                        | None -> Void
                                                                                                        | Some x -> ttype_of_string x) } }

operator_def:
    OPERATOR_KW OPERATOR OP_P separated_list(COMMA, pair(TYPE, IDENT)) CL_P TYPE OP_CB stmt* CL_CB {
                { name = $2;  args = (List.map (fun (x,y) -> (y, ttype_of_string x)) $4); body = $8; ret_t = (ttype_of_string $6) } }

type_def:
    TYPE_KW IDENT OP_CB separated_list(COMMA, pair(TYPE, IDENT)) CL_CB {
                               { name = $2; fields = (List.map (fun (x,y) -> (y, ttype_of_string x)) $4) } }

extern:
    EXTERN IDENT OP_P separated_list(COMMA, pair(TYPE, IDENT*)) CL_P { { name = $2; args = (List.map (fun (x,y) -> ttype_of_string x) $4) } }

impl:
    IMPLEMENT FOR IDENT OP_CB fnc_def* CL_CB { { tp = $3; functions = $5 } }

top_level:
    incl { Include $1 }
    | impl { Implement $1 }
    | extern { Extern $1 }
    | type_def { TypeDef $1 }
    | fnc_def { FncDef $1 }
    | operator_def { OperatorDef $1 }

main:
    top_level+ EOF { $1 }