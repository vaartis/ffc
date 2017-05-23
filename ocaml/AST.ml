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
        end;;

class include_ast modules = object(self)
  inherit toplevel
  method dump =
    "Include(" ^ (String.concat ", " modules) ^ ")"
end;;

class int_ast value = object(self)
  inherit base_ast
  method dump =
    "Int(" ^ (string_of_int value) ^ ")"
end;;

class fnc_def_ast name
                  (args : (string * ttype) list)
                  (body : expression list)
                  ret_t = object(self)
  inherit toplevel

  method dump =
    let arg_str = List.map (fun x -> let (name, tp) = x in name ^ " " ^ string_of_ttype tp) args in
    let body_str = List.map (fun x -> x#dump) body in
    "FncDef " ^ name ^ "(" ^ (String.concat ", " arg_str) ^ ") " ^ (string_of_ttype ret_t) ^ " {\n" ^
      (String.concat "\n" body_str) ^ "\n}"
end;;
