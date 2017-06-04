open Llvm;;
open AST;;

module BuiltFnc = struct
  type t = { fnc: llvalue; ret_value: llvalue option; ret_block: llbasicblock; fnc_def: FncDef.t }
end;;

module BuiltType = struct
  type t = { tp: ttype; ltp: lltype; fields: (string * ttype) list; mixins: string list option }
end;;

module DefinedVar = struct
  type t = { tp: ttype; instance: llvalue }
end;;

(* SO question #31279920 *)
let rec list_index x lst =
    match lst with
    | [] -> raise Not_found
    | h :: t -> if x = h then 0 else 1 + list_index x t;;

let unwrap o = match o with
  | Some x -> x
  | None -> failwith "Unwrapped None";;

let is_some o = match o with
  | Some _ -> true
  | None -> false;;

let codegen ast =
  let context = global_context () in
  let modl = create_module context "stdin" in
  let builder = builder context in

  let functions = Hashtbl.create 1 in
  let mixins = Hashtbl.create 0 in
  let externed = Hashtbl.create 0 in
  let curr_variables = Hashtbl.create 0 in
  let types = Hashtbl.create 0 in
  let curr_fn_name = ref "" in

  let get_llvm_type tp =
    match tp with
    | Int -> i32_type context
    | Float -> float_type context
    | Str -> pointer_type @@ i8_type context
    | Bool -> i1_type context
    | Void -> void_type context
    | Custom x ->
       try
         (Hashtbl.find types x).BuiltType.ltp
       with Not_found ->
         failwith (Printf.sprintf "Undefined custom type: %s" x)
  in

  let rec expr_type ex =
    let open Expression in
    match ex with
    | IntLit _ -> Int
    | FloatLit _ ->  Float
    | StrLit _ ->  Str
    | BoolLit _ -> Bool
    | Ident x -> let open AST.Ident in
                 (Hashtbl.find curr_variables x.value).DefinedVar.tp
    | FncCall x -> let open AST.FncCall in
                   (Hashtbl.find functions !curr_fn_name).BuiltFnc.fnc_def.ret_t
    | TypeLit x -> let open AST.TypeLit in
                   (Hashtbl.find types x.name).BuiltType.tp
    | TypeFieldLoad x -> let open AST.TypeFieldLoad in
                         List.assoc x.TypeFieldLoad.field_name (Hashtbl.find types (string_of_ttype (expr_type x.from))).BuiltType.fields
    | If x -> let open AST.If in
              match x.if_val with
              | Some x -> expr_type x
              | None -> failwith "This `if` is not an expression!"

  and mangle_type_name ltp = match (classify_type ltp) with
    | Llvm.TypeKind.Struct -> begin
        let nm = unwrap (struct_name ltp) in
        Printf.sprintf "%i%s" (String.length nm) nm
      end
    | _ -> begin
        let ltp_s = string_of_lltype ltp in
        Printf.sprintf "%i%s" (String.length ltp_s) ltp_s
      end

  and mangle (fn : FncDef.t) =
    let trav_ar_list x = List.fold_left (fun acc (_, tp) ->
                             let ltp = get_llvm_type tp in
                             Printf.sprintf "%sA%s" acc (mangle_type_name ltp)) "" x in

    let (name, arg_str, ret_t) = (fn.name, trav_ar_list fn.args, fn.ret_t) in

    let ret_s =
      let ltp_s = string_of_lltype @@ get_llvm_type ret_t in
      Printf.sprintf "R%i%s" (String.length ltp_s) ltp_s
    in

    let tp_or_mixin_s = match fn.from with
      | Some x -> Printf.sprintf "T%i%s" (String.length x) x
      | None -> match fn.mixin with
                | Some x -> Printf.sprintf "MI%i%s" (String.length x) x
                | None -> ""
    in

    Printf.sprintf "_FF%sN%i%s%s%s" tp_or_mixin_s (String.length name) name arg_str ret_s

  and mangle_call ca =
    let name = ca.FncCall.name in
    let ar_str =
      let ar_types = List.map expr_type ca.args in
      List.fold_left (fun acc tp ->
          let ltp = get_llvm_type tp in
          Printf.sprintf "%sA%s" acc (mangle_type_name ltp)) "" ar_types
    in
    let ret_s =
      let s = (string_of_lltype @@ get_llvm_type @@ expr_type @@ FncCall ca) in
      Printf.sprintf "R%i%s" (String.length s) s
    in

    let from_tp = match ca.from with
      | Some x -> let tp = string_of_ttype @@ expr_type x in
                  Printf.sprintf "T%i%s" (String.length tp) tp
      | None -> ""
    in

    Printf.sprintf "_FF%sN%i%s%s%s" from_tp (String.length name) name ar_str ret_s
  in

  let string_of_fnc_call f =
    let tp_s = match f.FncCall.from with
             | Some x -> Printf.sprintf "%s." @@ string_of_ttype @@ expr_type x
             | None -> ""
    in

    let ar_l = List.map (fun x -> string_of_ttype @@ expr_type x) f.FncCall.args in
    let ar_s = String.concat ", " ar_l in
    let ret_s = string_of_ttype @@ expr_type @@ FncCall f in
    Printf.sprintf "%s%s(%s) %s" tp_s f.name ar_s ret_s
  in

  let rec gen_expr ex =
    let open AST.Expression in
    match ex with
    | IntLit x -> let open AST.Int in
                  const_int (get_llvm_type Int) x.value
    | FloatLit x -> let open AST.Float in
                    const_float (get_llvm_type Float) x.value
    | StrLit x -> let open AST.Str in
                  build_global_stringptr x.value "" builder
    | BoolLit x -> let open AST.Bool in
                   const_int (get_llvm_type Bool) (if x.value then 1 else 0)
    | Ident x -> begin
        let open AST.Ident in
        try
          let var = Hashtbl.find curr_variables x.value in
          build_load var.DefinedVar.instance "" builder
        with Not_found ->
          failwith (Printf.sprintf "Undefined variable: %s" x.value)
      end
    | FncCall x -> begin
        let open AST.FncCall in
        begin
          match x.from with
          | Some fr ->
             begin
               let tp = Hashtbl.find types (string_of_ttype @@ expr_type @@ unwrap x.from) in
               match tp.mixins with
               | Some ms ->
                  begin
                    if not (List.exists (fun m -> let mm = Hashtbl.find mixins m in
                                                  List.exists (fun f -> (f.FncDef.name = x.name)) mm.MixinDef.functions
                                        ) ms) then
                      x.args <- fr :: x.args
                  end
               | None -> x.args <- fr :: x.args
             end
          | None -> ()
        end;

        let f =
          begin
            try
              (Hashtbl.find functions (mangle_call x)).fnc
            with Not_found ->
              begin
                try
                  Hashtbl.find externed x.name
                with Not_found ->
                  failwith (Printf.sprintf "Undefined function: %s" (string_of_fnc_call x))
              end
          end
        in
        let args = Array.of_list @@ List.map gen_expr x.args in
        build_call f args "" builder
      end
    | TypeLit t_l -> begin
        let open AST.TypeLit in
        try
          let stp = Hashtbl.find types t_l.TypeLit.name in
          let tal = build_alloca stp.ltp "" builder in
          List.iter (fun (nm, ex) ->
              if List.mem_assoc nm stp.fields then
                if (List.assoc nm stp.fields) = expr_type ex then
                  let gep = build_struct_gep tal (list_index (nm, expr_type ex) stp.fields) "" builder in
                  ignore(build_store (gen_expr ex) gep builder)
                else
                  failwith @@ Printf.sprintf "Wrong type assigned to field %s of custom type %s, expected %s, but got %s"
                                             nm
                                             t_l.name (string_of_ttype @@ List.assoc nm stp.fields) (string_of_ttype @@ expr_type ex)
              else
                failwith (Printf.sprintf "Can't find field %s with type %s in custom type %s"
                                         nm
                                         (string_of_ttype @@ expr_type ex) t_l.name)
            ) t_l.TypeLit.fields;
          build_load tal "" builder
        with Not_found ->
          failwith (Printf.sprintf "Undefined custom type: %s" t_l.name)
      end
    | TypeFieldLoad x -> begin
        let open AST.TypeFieldLoad in
        begin
          match expr_type x.TypeFieldLoad.from with
          | Custom _ -> ()
          | t -> failwith @@ Printf.sprintf "Cannot load field %s from non-custom type %s" x.field_name (string_of_ttype t)
        end;

        let tt_name = (string_of_ttype @@ expr_type x.TypeFieldLoad.from) in
        let tp = Hashtbl.find types tt_name in

        if List.mem_assoc x.field_name tp.BuiltType.fields then
          let f_tp = List.assoc x.field_name tp.BuiltType.fields in

          (* Build a temporary value to load from *)
          let tmp_val = build_alloca (get_llvm_type @@ expr_type x.from) "" builder in
          ignore(build_store (gen_expr x.from) tmp_val builder);

          build_load (build_struct_gep tmp_val (list_index (x.field_name, f_tp) tp.fields) "" builder) "" builder
        else
          failwith @@ Printf.sprintf "Undefined field %s for type %s" x.field_name tt_name
      end
    | If x -> gen_if x

  and gen_if iff =
    let open AST.If in
    let curr_f = Hashtbl.find functions !curr_fn_name in

    let curr_bb = insertion_block builder in

    let cond = gen_expr iff.cond
    and th_b = ref (append_block context "if.then" curr_f.fnc)
    and el_b = ref (append_block context "if.else" curr_f.fnc)
    and af_b = append_block context "if.end" curr_f.fnc in

    move_block_after curr_bb !th_b;
    move_block_after !th_b !el_b;
    move_block_after !el_b af_b;

    position_at_end curr_bb builder;
    ignore(build_cond_br cond !th_b !el_b builder);

    position_at_end !th_b builder;
    List.iter gen_stmt iff.if_br;
    let if_val = match iff.if_val with
      | Some x -> Some (gen_expr x)
      | None -> None
    in
    th_b := insertion_block builder; (* Update for phi *)
    ignore(build_br af_b builder);


    let else_val = begin
        match iff.else_br with
        | Some else_br -> begin
            position_at_end !el_b builder;
            List.iter gen_stmt else_br;
            let else_val = match iff.else_val with
              | Some x -> Some (gen_expr x)
              | None -> None
            in
            el_b := insertion_block builder; (* Update for phi *)
            ignore(build_br af_b builder);
            else_val
          end
        | None -> None
      end
    in

    position_at_end af_b builder;

    match (if_val, else_val) with
    | (Some i, Some e) -> begin
        if (type_of i) = (type_of e) then
          build_phi [(i, !th_b); (e, !el_b)] "" builder
        else
          failwith @@
            Printf.sprintf "If and else branches have different types: %s and %s"
                           (string_of_ttype @@ expr_type @@ unwrap iff.if_val)
                           (string_of_ttype @@ expr_type @@ unwrap iff.else_val)
      end
    | (Some _, None) | (None, Some _) -> failwith "Both if and else branches in an if-expression must have value" (* Won't happen, parser handles that*)
    | _ -> cond

  and gen_stmt st =
    let open AST.Statement in
    match st with
    | Decl x -> begin
        let open AST.Decl in
        let alloca = build_alloca (get_llvm_type x.tp) x.name builder in

        begin match x.value with
        | Some v -> begin
            let vtp = expr_type v
            and atp = x.tp in
            if vtp <> x.tp then
              failwith @@ Printf.sprintf "Wrong type of expression for `%s`, expected %s, but got %s" x.name (string_of_ttype atp) (string_of_ttype vtp)
            else
              ignore(build_store (gen_expr v) alloca builder)
          end
        | None -> ()
        end;
        Hashtbl.replace curr_variables x.name { DefinedVar.instance = alloca; tp = x.tp };
      end
    | ExprAsStmt x -> ignore(gen_expr x)
    | Ret x -> begin
        let open AST.Ret in
        let f = Hashtbl.find functions !curr_fn_name in
        begin
          match x.value with
          | None -> ()
          | Some va ->
             let ret_v = match f.BuiltFnc.ret_value with
               | Some v -> v
               | None -> failwith "Can't return value from void function" in
             ignore(build_store (gen_expr va) ret_v builder)
        end;
        ignore(build_br f.BuiltFnc.ret_block builder)
      end
    | Assign x -> begin
        let open AST.Assign in
        try
          let v = Hashtbl.find curr_variables x.Assign.name in
          if v.tp <> (expr_type x.value) then
            failwith (Printf.sprintf
                        "Wrong type assigned to `%s`, expected %s, but got %s"
                        x.name (string_of_ttype v.tp) (string_of_ttype @@ expr_type x.value))
          else
            ignore(build_store (gen_expr x.value) v.instance builder)
        with Not_found ->
          failwith (Printf.sprintf "Undefined variable: %s" x.name)
      end
    | TypeFieldAssign x ->
       let open AST.TypeFieldAssign in
       begin
         try
           let v = Hashtbl.find curr_variables x.TypeFieldAssign.name in
           let tt_name = string_of_ttype v.tp in
           let tp = Hashtbl.find types tt_name in

           begin
             try
               let f_tp = List.assoc x.field_name tp.BuiltType.fields in
               let fld = build_struct_gep v.instance (list_index (x.field_name, f_tp) tp.fields) "" builder in

               if f_tp = (expr_type x.value) then
                 ignore(build_store (gen_expr x.value) fld builder)
               else
                 failwith @@ Printf.sprintf "Wrong type assigned to field %s of %s (of type %s), expected %s, but got %s"
                               x.field_name x.name tt_name (string_of_ttype f_tp) (string_of_ttype @@ expr_type x.value)

             with Not_found ->
               failwith @@ Printf.sprintf "Undefined field %s for type %s" x.field_name tt_name
           end
         with Not_found ->
           failwith @@ Printf.sprintf "Undefined variable: %s" x.name
       end
    | If x -> ignore(gen_if x)
  in

  let gen_compiled_in () =
    let gen (name, arg1_t, arg2_t, ret_t, f) =
      let f_def = { FncDef.name = name; args = [("x", arg1_t); ("y", arg2_t)]; body = []; ret_t; from = None; mixin = None } in
      let m_name = mangle @@ f_def in
      let fnc = define_function m_name (function_type (get_llvm_type ret_t) [|get_llvm_type arg1_t; get_llvm_type arg2_t|]) modl in
      set_linkage Linkage.Link_once fnc;
      add_function_attr fnc (create_enum_attr context "alwaysinline" 0L) AttrIndex.Function;
      position_at_end (entry_block fnc) builder;

      let r_val = f (Array.get (params fnc) 0) ((Array.get (params fnc) 1)) "" builder in
      ignore(build_ret r_val builder);
      Hashtbl.replace functions m_name { BuiltFnc.fnc = fnc; ret_value = None; ret_block = entry_block fnc; fnc_def = f_def }
    in

    let gen_c_i (name, pred, arg1_t, arg2_t, ret_t, f) =
      let f_def = { FncDef.name = name; args = [("x", arg1_t); ("y", arg2_t)]; body = []; ret_t; from = None; mixin = None } in
      let m_name = mangle @@ f_def in
      let fnc = define_function m_name (function_type (get_llvm_type ret_t) [|get_llvm_type arg1_t; get_llvm_type arg2_t|]) modl in
      set_linkage Linkage.Link_once fnc;
      add_function_attr fnc (create_enum_attr context "alwaysinline" 0L) AttrIndex.Function;
      position_at_end (entry_block fnc) builder;

      let r_val = f pred (Array.get (params fnc) 0) ((Array.get (params fnc) 1)) "" builder in
      ignore(build_ret r_val builder);
      Hashtbl.replace functions m_name { BuiltFnc.fnc = fnc; ret_value = None; ret_block = entry_block fnc; fnc_def = f_def }
    in

    let gen_c_f (name, pred, arg1_t, arg2_t, ret_t, f) =
      let f_def = { FncDef.name = name; args = [("x", arg1_t); ("y", arg2_t)]; body = []; ret_t; from = None; mixin = None } in
      let m_name = mangle @@ f_def in
      let fnc = define_function m_name (function_type (get_llvm_type ret_t) [|get_llvm_type arg1_t; get_llvm_type arg2_t|]) modl in
      set_linkage Linkage.Link_once fnc;
      add_function_attr fnc (create_enum_attr context "alwaysinline" 0L) AttrIndex.Function;
      position_at_end (entry_block fnc) builder;

      let r_val = f pred (Array.get (params fnc) 0) ((Array.get (params fnc) 1)) "" builder in
      ignore(build_ret r_val builder);
      Hashtbl.replace functions m_name { BuiltFnc.fnc = fnc; ret_value = None; ret_block = entry_block fnc; fnc_def = f_def }
    in

    List.iter gen [
                ("+", Int, Int, Int, build_add);
                ("-", Int, Int, Int, build_sub);
                ("*", Int, Int, Int, build_mul);
                ("/", Int, Int, Int, build_sdiv);

                ("+", Float, Float, Float, build_fadd);
                ("-", Float, Float, Float, build_fsub);
                ("*", Float, Float, Float, build_fmul);
                ("/", Float, Float, Float, build_fdiv);
              ];

    List.iter gen_c_i [
                ("==", Icmp.Eq,  Int, Int, Bool, build_icmp);
                ("!=", Icmp.Ne,  Int, Int, Bool, build_icmp);
                (">=", Icmp.Sge, Int, Int, Bool, build_icmp);
                ("<=", Icmp.Sle, Int, Int, Bool, build_icmp);
                (">",  Icmp.Sgt, Int, Int, Bool, build_icmp);
                ("<",  Icmp.Slt, Int, Int, Bool, build_icmp)
              ];
    List.iter gen_c_f [
                ("==", Fcmp.Oeq,  Float, Float, Bool, build_fcmp);
                ("!=", Fcmp.One,  Float, Float, Bool, build_fcmp);
                (">=", Fcmp.Oge, Float, Float, Bool, build_fcmp);
                ("<=", Fcmp.Ole, Float, Float, Bool, build_fcmp);
                (">",  Fcmp.Ogt, Float, Float, Bool, build_fcmp);
                ("<",  Fcmp.Olt, Float, Float, Bool, build_fcmp);
              ]
  in

  gen_compiled_in ();

  List.iter (fun node ->
      Hashtbl.reset curr_variables;

      let gen_fnc x =
        let open AST.FncDef in

        begin
          match x.from with
          | Some xx -> x.args <- ("self", Custom xx) :: x.args
          | None -> ()

        end;

        let m_name = if x.name <> "main" then mangle x else x.name in

        curr_fn_name := m_name;
        let ftype = function_type (get_llvm_type x.FncDef.ret_t) (Array.of_list(List.map (fun (x,y) -> get_llvm_type y) x.args)) in
        let fnc = define_function m_name ftype modl in
        position_at_end (entry_block fnc) builder;

        let ret_block = append_block context "exit" fnc in
        let ret_value = match x.FncDef.ret_t with
          | Void -> position_at_end ret_block builder;
                    ignore(build_ret_void builder);
                    None
          | va ->
             let r = build_alloca (get_llvm_type va) "ret_val" builder in
             position_at_end ret_block builder;
             ignore(build_ret (build_load r "" builder) builder);
             Some r in
        position_at_end (entry_block fnc) builder;

        position_at_end (entry_block fnc) builder;

        (* Make variables from arguments *)

        Array.iteri (fun i par ->
            let r_param = List.nth x.args i in
            let p = build_alloca (type_of par) (fst r_param) builder in
            ignore(build_store par p builder);
            Hashtbl.replace curr_variables (fst r_param) { DefinedVar.tp = snd r_param; instance = p };
          ) (params fnc);

        Hashtbl.replace functions m_name { BuiltFnc.fnc; ret_value; ret_block; fnc_def = x };
        List.iter gen_stmt x.body
      in

      match node with
      | FncDef x | OperatorDef x -> gen_fnc x
      | TypeDef x -> begin
          let open AST.TypeDef in

          let st_fields = List.map (fun (_, tp) -> get_llvm_type tp ) x.TypeDef.fields in
          let st = named_struct_type context x.name in
          struct_set_body st (Array.of_list st_fields) false;
          Hashtbl.replace types x.name { BuiltType.ltp = st; tp = Custom x.name; fields = x.fields; mixins = x.mixins };

          begin
            match x.mixins with
            | Some ms -> List.iter (fun m ->
                             let m_o = try
                                 Hashtbl.find mixins m
                               with Not_found ->
                                 failwith @@ Printf.sprintf "Undefined mixin: %s" m in
                             List.iter (fun fn ->
                                 let name_in_mixin = mangle fn in
                                 let genned_mixin_fnc = Hashtbl.find functions name_in_mixin in
                                 let name_in_type = mangle {
                                                        fn with mixin = None;
                                                                from = Some x.name;
                                                      }
                                 in
                                 let genned_in_type = add_alias modl (type_of genned_mixin_fnc.fnc) genned_mixin_fnc.fnc name_in_type in
                                 Hashtbl.replace functions name_in_type { genned_mixin_fnc with fnc = genned_in_type }
                               ) m_o.MixinDef.functions
                           ) ms
            | None -> ()
          end
        end
      | Implement { functions = x }  -> List.iter gen_fnc x
      | MixinDef x -> begin
          List.iter gen_fnc x.functions;
          Hashtbl.replace mixins x.name x
        end

      | Extern x -> let fnc = declare_function x.Extern.name
                                               (function_type
                                                  (get_llvm_type x.ret_t)
                                                  (Array.of_list (List.map get_llvm_type x.args))) modl
                    in
                    Hashtbl.replace externed x.name fnc
    ) ast; modl;;
