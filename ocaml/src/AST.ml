type ttype =
  | Int
  | Float
  | Str
  | Void
  | Custom of string

let string_of_ttype x =
  match x with
  | Int -> "Int"
  | Float -> "Float"
  | Str -> "Str"
  | Void -> "Void"
  | Custom(x) -> x

let ttype_of_string x =
  match x with
  | "int" -> Int
  | "float" -> Float
  | "str" -> Str
  | _ -> Void


module rec Expression : sig
  type t =
    | IntLit of Int.t
    | FloatLit of Float.t
    | StrLit of Str.t
    | Ident of Ident.t
    | FncCall of FncCall.t
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

   and Ident : sig
     type t = { value: string }
   end = Ident

   and FncCall : sig
     type t = { name: string; args: Expression.t list }
   end = FncCall

module Decl = struct
  type t = { name: string; tp: ttype; value : Expression.t option }
end;;
module Ret = struct
  type t = { value: Expression.t option }
end;;
type statement =
  | ExprAsStmt of Expression.t
  | Decl of Decl.t
  | Ret of Ret.t

module Include = struct
  type t = { modules: string list }
end;;
module FncDef = struct
    type t = { name: string; args: (string * ttype) array; body: statement list; ret_t: ttype }
end;;
module TypeDef = struct
  type t = { name: string; fields : (string * ttype) list }
end;;
module Extern = struct
  type t = { name: string; args: ttype list }
end;;
module Implement = struct
  type t = { tp: string; functions : FncDef.t list}
end;;

type toplevel =
  | Include of Include.t
  | FncDef of FncDef.t
  | OperatorDef of FncDef.t
  | TypeDef of TypeDef.t
  | Extern of Extern.t
  | Implement of Implement.t;;
