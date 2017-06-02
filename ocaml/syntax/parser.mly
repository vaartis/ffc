%{
open AST;;
open AST.Expression;;
open AST.Statement;;

let types = Hashtbl.create 0;;
%}

%token <int> INT
%token <string> STR IDENT TYPE OPERATOR
%token <float> FLOAT
%token <bool> BOOL

%type <AST.toplevel> top_level
%type <AST.toplevel list> main
%type <AST.Include.t> incl
%type <AST.FncDef.t> fnc_def
%type <AST.TypeDef.t> type_def
%type <AST.Extern.t> extern
%type <AST.FncDef.t> operator_def
%type <AST.Implement.t> impl
%type <AST.ttype> tp custom_tp

%type <AST.Expression.t> expr

%type <AST.Decl.t> decl
%type <AST.Ret.t> ret
%type <AST.TypeFieldAssign.t> type_field_assign
%type <AST.Statement.t> stmt

%type <string> str

%token FNC INCLUDE SEMICOLON COMMA DOT EOF OP_P CL_P OP_CB CL_CB TYPE_KW OPERATOR_KW EXTERN FOR IMPLEMENT EQ RET

%start main

%%

custom_tp:
    IDENT { try ignore(Hashtbl.find types $1); Custom $1 with Not_found -> failwith (Printf.sprintf "Unknown type: %s" $1) }

tp:
    TYPE { ttype_of_string $1 }
    | custom_tp { $1 }

expr:
    INT { IntLit { Int.value = $1 } }
    | FLOAT { FloatLit { Float.value = $1 } }
    | BOOL { BoolLit { Bool.value = $1 } }
    | str { StrLit { Str.value = $1 } }
    | IDENT OP_P separated_list(COMMA, expr) CL_P { FncCall { FncCall.name = $1; args = $3; from = None } } (* Function call *)
    | IDENT { Ident { Ident.value = $1 } } (* Variable *)
    | custom_tp OP_CB separated_list(COMMA, separated_pair(IDENT, EQ, expr)) CL_CB { TypeLit { TypeLit.name = (string_of_ttype $1); fields = $3 } } (*Type literal*)
    | expr DOT IDENT { TypeFieldLoad { TypeFieldLoad.from = $1; field_name = $3 } } (* Type field load *)
    | expr DOT IDENT OP_P separated_list(COMMA, expr) CL_P { FncCall { FncCall.name = $3; args = $5; from = Some $1 } }
    | expr OPERATOR expr { FncCall { FncCall.name = $2; args = [$1;$3]; from = None } } (* Operator usage *)

stmt:
    expr SEMICOLON { ExprAsStmt $1 }
    | type_field_assign SEMICOLON { TypeFieldAssign $1 }
    | assign SEMICOLON { Assign $1 }
    | decl SEMICOLON { Decl $1 }
    | ret SEMICOLON { Ret $1 }

assign:
    IDENT EQ expr { { Assign.name = $1; value = $3 } }

type_field_assign:
    IDENT DOT IDENT EQ expr { { TypeFieldAssign.name = $1; field_name = $3; value = $5 } }

ret:
    RET expr { { Ret.value = Some $2 } }
    | RET { { Ret.value = None } }

decl:
    tp IDENT EQ expr { { Decl.name = $2; tp = $1; value = Some $4 } }
    | tp IDENT { { Decl.name = $2; tp = $1; value = None } }

str: STR { $1 }

incl:
    INCLUDE str+ { { Include.modules = $2 } }

fnc_def:
    FNC IDENT OP_P separated_list(COMMA, pair(tp, IDENT)) CL_P tp? OP_CB stmt* CL_CB {
                              { FncDef.name = $2; args = Array.of_list (List.map (fun (x,y) -> (y, x)) $4); body = $8; ret_t = (match $6 with
                                                                                                        | None -> Void
                                                                                                        | Some x -> x) } }

operator_def:
    OPERATOR_KW OPERATOR OP_P separated_list(COMMA, pair(tp, IDENT)) CL_P tp OP_CB stmt* CL_CB {
                { FncDef.name = $2;  args = Array.of_list (List.map (fun (x,y) -> (y, x)) $4); body = $8; ret_t = ($6) } }

type_def:
    TYPE_KW IDENT OP_CB separated_list(COMMA, pair(tp, IDENT)) CL_CB {
                               Hashtbl.replace types $2 ();
                               { TypeDef.name = $2; fields = (List.map (fun (x,y) -> (y, x)) $4) } }

extern:
    EXTERN IDENT OP_P separated_list(COMMA, pair(tp, IDENT*)) CL_P { { Extern.name = $2; args = (List.map (fun (x,y) -> x) $4) } }

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