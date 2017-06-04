type ttype =
  | Int
  | Float
  | Str
  | Void
  | Bool
  | Custom of string

let string_of_ttype x =
  match x with
  | Int -> "Int"
  | Float -> "Float"
  | Str -> "Str"
  | Void -> "Void"
  | Bool -> "Bool"
  | Custom(x) -> x

let ttype_of_string x =
  match x with
  | "int" -> Int
  | "float" -> Float
  | "str" -> Str
  | "bool" -> Bool
  | x -> Custom x


module rec Expression : sig
  type t =
    | IntLit of Int.t
    | FloatLit of Float.t
    | StrLit of Str.t
    | BoolLit of Bool.t
    | Ident of Ident.t
    | FncCall of FncCall.t
    | TypeLit of TypeLit.t
    | TypeFieldLoad of TypeFieldLoad.t
    | If of If.t
end = Expression

   and Int : sig
     type t = { value: int }
   end = Int

   and Float : sig
      type t = { value: float }
   end = Float

   and Str : sig
      type t = { value: string }
   end = Str

   and Bool : sig
     type t = { value: bool }
   end = Bool

   and Ident : sig
     type t = { value: string }
   end = Ident

   and FncCall : sig
     type t = { name: string; mutable args: Expression.t list; from: Expression.t option }
   end = FncCall

   and TypeLit : sig
     type t = { name: string; fields: (string * Expression.t) list }
   end = TypeLit

   and TypeFieldLoad : sig
     type t = { from: Expression.t; field_name: string }
   end = TypeFieldLoad

   and If : sig
     type t = { cond: Expression.t; if_br: Statement.t list;
                else_br: (Statement.t list) option; if_val: Expression.t option; else_val: Expression.t option }
   end = If

   and Statement : sig
     type t =
       | ExprAsStmt of Expression.t
       | If of If.t
       | Decl of Decl.t
       | Ret of Ret.t
       | Assign of Assign.t
       | TypeFieldAssign of TypeFieldAssign.t
   end = Statement

   and Decl : sig
     type t = { name: string; tp: ttype; value : Expression.t option }
   end = Decl

   and Ret : sig
     type t = { value: Expression.t option }
   end = Ret

   and Assign : sig
     type t = { name: string; value: Expression.t }
   end = Assign

   and TypeFieldAssign : sig
     type t = { name: string; field_name: string; value: Expression.t }
   end = TypeFieldAssign

module rec Include : sig
  type t = { modules: string list }
end = Include

   and FncDef : sig
     type t = { name: string;
                mutable args: (string * ttype) list;
                body: Statement.t list;
                ret_t: ttype;
                from: string option;
                mixin: string option;
              }
   end = FncDef

   and TypeDef : sig
     type t = {
         name: string;
         fields : (string * ttype) list;
         mixins: string list option
       }
   end = TypeDef

   and MixinDef : sig
     type t = { name: string; functions: FncDef.t list }
   end = MixinDef

   and Extern : sig
     type t = { name: string; args: ttype list; ret_t: ttype }
   end = Extern

   and Implement : sig
     type t = { tp: string; functions : FncDef.t list}
   end = Implement

type toplevel =
  | Include of Include.t
  | FncDef of FncDef.t
  | OperatorDef of FncDef.t
  | TypeDef of TypeDef.t
  | MixinDef of MixinDef.t
  | Extern of Extern.t
  | Implement of Implement.t;;
