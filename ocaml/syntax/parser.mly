%{
open AST;;
open AST.Expression;;
%}

%token <int> INT
%token <string> STR IDENT TYPE OPERATOR
%token <float> FLOAT

%type <AST.toplevel> top_level
%type <AST.toplevel list> main
%type <AST.Include.t> incl
%type <AST.FncDef.t> fnc_def
%type <AST.TypeDef.t> type_def
%type <AST.Extern.t> extern
%type <AST.FncDef.t> operator_def
%type <AST.Implement.t> impl

%type <AST.Expression.t> expr

%type <AST.Decl.t> decl
%type <AST.Ret.t> ret
%type <AST.statement> stmt

%type <string> str

%token FNC INCLUDE SEMICOLON COMMA EOF OP_P CL_P OP_CB CL_CB TYPE_KW OPERATOR_KW EXTERN FOR IMPLEMENT EQ RET

%start main

%%

expr:
    INT { IntLit { Int.value = $1 } }
    | FLOAT { FloatLit { Float.value = $1 } }
    | str { StrLit { Str.value = $1 } }
    | IDENT OP_P separated_list(COMMA, expr) CL_P { FncCall { FncCall.name = $1; args = $3 } }
    | IDENT { Ident { Ident.value = $1 } }

stmt:
    expr SEMICOLON { ExprAsStmt $1 }
    | decl SEMICOLON { Decl $1 }
    | ret SEMICOLON { Ret $1 }

ret:
    RET expr { { Ret.value = Some $2 } }
    | RET { { Ret.value = None } }

decl:
    TYPE IDENT EQ expr { { Decl.name = $2; tp = ttype_of_string $1; value = Some $4 } }
    | TYPE IDENT { { Decl.name = $2; tp = ttype_of_string $1; value = None } }

str: STR { $1 }

incl:
    INCLUDE str+ { { Include.modules = $2 } }

fnc_def:
    FNC IDENT OP_P separated_list(COMMA, pair(TYPE, IDENT)) CL_P TYPE? OP_CB stmt* CL_CB {
                              { FncDef.name = $2; args = (List.map (fun (x,y) -> (y, ttype_of_string x)) $4); body = $8; ret_t = (match $6 with
                                                                                                        | None -> Void
                                                                                                        | Some x -> ttype_of_string x) } }

operator_def:
    OPERATOR_KW OPERATOR OP_P separated_list(COMMA, pair(TYPE, IDENT)) CL_P TYPE OP_CB stmt* CL_CB {
                { FncDef.name = $2;  args = (List.map (fun (x,y) -> (y, ttype_of_string x)) $4); body = $8; ret_t = (ttype_of_string $6) } }

type_def:
    TYPE_KW IDENT OP_CB separated_list(COMMA, pair(TYPE, IDENT)) CL_CB {
                               { TypeDef.name = $2; fields = (List.map (fun (x,y) -> (y, ttype_of_string x)) $4) } }

extern:
    EXTERN IDENT OP_P separated_list(COMMA, pair(TYPE, IDENT*)) CL_P { { Extern.name = $2; args = (List.map (fun (x,y) -> ttype_of_string x) $4) } }

impl:
    IMPLEMENT FOR IDENT OP_CB fnc_def* CL_CB { { Implement.tp = $3; functions = $5 } }

top_level:
    incl { Include $1 }
    | impl { Implement $1 }
    | extern { Extern $1 }
    | type_def { TypeDef $1 }
    | fnc_def { FncDef $1 }
    | operator_def { OperatorDef $1 }

main:
    top_level+ EOF { $1 }