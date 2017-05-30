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


module Int = struct
  type t = { value: int }
end;;
module Float = struct
  type t = { value: float }
end;;
module Str = struct
  type t = { value: string }
end;;
module Ident = struct
  type t = { value: string }
end;;
type expression =
  | IntLit of Int.t
  | FloatLit of Float.t
  | StrLit of Str.t
  | Ident of Ident.t

module Decl = struct
  type t = { name: string; tp: ttype; value : expression option }
end;;
module Ret = struct
  type t = { value: expression option }
end;;
type statement =
  | ExprAsStmt of expression
  | Decl of Decl.t
  | Ret of Ret.t

module Include = struct
  type t = { modules: string list }
end;;
module FncDef = struct
    type t = { name: string; args: (string * ttype) list; body: statement list; ret_t: ttype }
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
  | Implement of Implement.t

(*class extern_ast name (args : ttype list) = object
  inherit toplevel

  method dump =
    let fld_str = List.map string_of_ttype args in
    "Extern " ^ name ^ "(" ^ (String.concat ", " fld_str) ^ ")\n"
end;;

class implement_ast tp (functions : fnc_def_ast list) = object
  inherit toplevel

  method dump =
    "Implement for " ^ tp ^ " {\n" ^ (List.map (fun f -> f#dump) functions |> String.concat "\n") ^ "\n}"
end;;

class decl_ast name tp (value : expression option) = object
  inherit statement

  method dump =
    let v = match value with
      | Some x -> " = " ^ x#dump
      | None -> "" in
    "Decl (" ^ name ^ ")" ^ v
end;;


class virtual base_ast = object
          method virtual dump : string
        end;;

class virtual toplevel = object
          inherit base_ast
        end;;

class virtual statement = object
          inherit base_ast
        end;;

class virtual expression value tp = object
          inherit statement (* Expression can also be used as a statement *)
        end;;*)

(*class operator_def_ast name args body ret_t = object
  inherit fnc_def_ast name args body ret_t

  method dump =
    let arg_str = List.map (fun x -> let (name, tp) = x in string_of_ttype tp ^ " " ^ name) args |> String.concat ", " in
    let body_str = List.map (fun x -> x#dump) body |> String.concat "\n" in
    "OperatorDef " ^ name ^ "(" ^ arg_str ^ ") " ^ (string_of_ttype ret_t) ^ " {\n" ^ body_str ^ "\n}"
end;;

class type_def_ast name (fields : (string * ttype) list) = object
  inherit toplevel

  method dump =
    let fld_str = List.map (fun x -> let (name, tp) = x in string_of_ttype tp ^ " " ^ name) fields in
    "TypeDef " ^ name ^ " {\n" ^ (String.concat ",\n" fld_str) ^ "\n}"
end;;*)

(*class include_ast modules = object(self)
  inherit toplevel
  method dump =
    "Include(" ^ (String.concat ", " modules) ^ ")"
end;;*)

(*class int_ast value = object
  inherit base_ast
  method dump =
    "Int(" ^ (string_of_int value) ^ ")"
end;;*)

(*class float_ast value = object
  inherit base_ast
  method dump =
    "Float(" ^ (string_of_float value) ^ ")"
end;;

class str_ast value = object
  inherit base_ast
  method dump =
    "Str(" ^ value ^ ")"
end;;*)

(*class fnc_def_ast name
                  (args : (string * ttype) list)
                  (body : statement list)
                  ret_t = object
  inherit toplevel

  method dump =
    let arg_str = List.map (fun x -> let (name, tp) = x in string_of_ttype tp ^ " " ^ name) args |> String.concat ", " in
    let body_str = List.map (fun x -> x#dump) body |> String.concat "\n" in
    "FncDef " ^ name ^ "(" ^ arg_str ^ ") " ^ (string_of_ttype ret_t) ^ " {\n" ^ body_str ^ "\n}"
end;;*)
