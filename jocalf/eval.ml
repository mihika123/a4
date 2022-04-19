open Ast

type value =
  | VBool of bool
  | VInt of int
  | VString of string
  | VUd
  | VClosure of id list * expr * (id * value) list
  | VRecClosure of id list * expr * (id * value) list ref
  | VExtern of (int * (value -> value))
  | VLoc of int

type result =
  | RValue of value
  | RExpn of value

module Env = Map.Make (String)

type env = (id * value) list

type state = (int * value) list

let is_int v = match v with VInt x -> v | _ -> VBool false

let is_bool v = match v with VBool x -> v | _ -> VBool false

let is_string v = match v with VString x -> v | _ -> VBool false

let is_defined v = match v with VUd -> VBool false | _ -> v

let is_prim v =
  match v with
  | VInt _ | VBool _ | VString _ | VUd -> v
  | _ -> VBool false

let length v =
  match v with VString x -> VInt (x |> String.length) | _ -> VUd

let initial_env =
  [
    ("is_int", VExtern (1, is_int));
    ("is_bool", VExtern (1, is_bool));
    ("is_string", VExtern (1, is_string));
    ("is_defined", VExtern (1, is_defined));
    ("is_prim", VExtern (1, is_prim));
    ("length", VExtern (1, length));
  ]

let initial_state = []

let one_quote = "\""

let prim_value_of v =
  match v with VInt _ | VString _ | VBool _ | VUd -> v | _ -> VUd

let string_of_value v =
  match v with
  | VBool x -> x |> string_of_bool
  | VInt x -> x |> string_of_int
  | VString x -> one_quote ^ x ^ one_quote
  | VUd -> "undefined"
  | VClosure _ | VRecClosure _ -> "<closure>"
  | VLoc _ -> "<location>"
  | VExtern _ -> "<extern>"

let string_of_result r =
  match r with
  | RValue x -> x |> string_of_value
  | RExpn x -> "Exception: " ^ (x |> string_of_value)

(* Display the contents in the following format : [x = v, x1 = v1 ...] *)
let string_of_env env =
  let f (a, b) = "[" ^ a ^ " = " ^ (b |> string_of_value) ^ "]" in
  String.concat "," (List.map f env)

let string_of_state st = failwith "Unimplemented"

let int_of = function false -> 0 | true -> 1

let bool_of = function
  | VBool x -> VBool x
  | VInt x -> if x <> 0 then VBool true else VBool false
  | VString x -> if x <> "" then VBool true else VBool false
  | VUd -> VBool false
  | _ -> VBool true

let integer_of_value v =
  match v with
  | VInt x -> VInt x
  | VBool x -> ( match x with false -> VInt 0 | true -> VInt 1 )
  | VString x -> ( try VInt (int_of_string x) with _ -> VUd )
  | _ -> VUd

let not_of = function
  | VBool true -> VBool false
  | VBool false -> VBool true
  | _ -> failwith "will never reach this branch"

let type_of = function
  | VBool _ -> "bool"
  | VInt _ -> "int"
  | VString _ -> "string"
  | VClosure _ | VExtern _ -> "closure"
  | VLoc _ -> "location"
  | VUd -> "undefined"
  | _ -> "undefined"

let eval_uop (uop, v, env, st) =
  match (v, uop) with
  | VLoc x, UopDeref -> (
      match List.assoc_opt x st with
      | None -> (RExpn (VString "Assignment to non-location"), st)
      | Some v' -> (RValue v', st) )
  | VInt x, UopMinus -> (RValue (VInt (-x)), st)
  | VBool x, UopMinus ->
      let v' = x |> int_of in
      (RValue (VInt (-v')), st)
  | x, UopNot ->
      let res = x |> bool_of |> not_of in
      (RValue res, st)
  | x, UopTypeof -> (RValue (VString (x |> type_of)), st)
  | _ -> (RValue VUd, st)

let eval_bop_compare (x1, x2, bop, env, st) =
  match (x1, x2, bop) with
  | VString a, VString b, BopLt -> (RValue (VBool (a < b)), st)
  | VString a, VString b, BopLeq -> (RValue (VBool (a <= b)), st)
  | VString a, VString b, BopGt -> (RValue (VBool (a > b)), st)
  | VString a, VString b, BopGeq -> (RValue (VBool (a >= b)), st)
  | VUd, _, _ | _, VUd, _ -> (RValue (VBool false), st)
  | _ -> (
      let x1' = x1 |> integer_of_value in
      let x2' = x2 |> integer_of_value in
      match (x1', x2', bop) with
      | VInt y1, VInt y2, BopLt -> (RValue (VBool (y1 < y2)), st)
      | VInt y1, VInt y2, BopLeq -> (RValue (VBool (y1 <= y2)), st)
      | VInt y1, VInt y2, BopGt -> (RValue (VBool (y1 > y2)), st)
      | VInt y1, VInt y2, BopGeq -> (RValue (VBool (y1 >= y2)), st)
      | _ -> (RValue (VBool false), st) )

let rec get_deref_val (x1, x2, env, st) =
  let y1 = eval_uop (UopDeref, x1, env, st) in
  let y2 = eval_uop (UopDeref, x2, env, st) in
  match (y1, y2) with
  | (RValue v1, _), (RValue v2, _) -> eval_equals (v1, v2, env, st)
  | _ -> (RValue (VBool false), st)

and eval_equals (x1, x2, env, st) =
  match (x1, x2) with
  | VUd, VUd -> (RValue (VBool true), st)
  | VUd, _ | _, VUd -> (RValue (VBool false), st)
  | VBool a, VBool b -> (RValue (VBool (a = b)), st)
  | VInt a, VInt b -> (RValue (VBool (a = b)), st)
  | VString a, VString b -> (RValue (VBool (a = b)), st)
  | VLoc a, VLoc b -> get_deref_val (x1, x2, env, st)
  | VClosure _, VClosure _ -> (RValue (VBool false), st)
  | VInt a, y ->
      let y' = y |> integer_of_value in
      eval_equals (VInt a, y', env, st)
  | y, VInt b ->
      let y' = y |> integer_of_value in
      eval_equals (y', VInt b, env, st)
  | _ -> (RValue (VBool false), st)

let eval_eqstrict (x1, x2, env, st) =
  match (x1, x2) with
  | VUd, VUd -> (RValue (VBool true), st)
  | VBool a, VBool b -> (RValue (VBool (a == b)), st)
  | VInt a, VInt b -> (RValue (VBool (a == b)), st)
  | VString a, VString b -> (RValue (VBool (a == b)), st)
  | VLoc a, VLoc b -> (RValue (VBool (x1 == x2)), st)
  | VClosure _, VClosure _ -> (RValue (VBool false), st)
  | _ -> (RValue (VBool false), st)

let eval_bop (bop, v1, v2, env, st) =
  match (v1, v2, bop) with
  | VInt x1, VInt x2, BopPlus -> (RValue (VInt (x1 + x2)), st)
  | VBool x1, VBool x2, BopPlus ->
      let x1 = x1 |> int_of in
      let x2 = x2 |> int_of in
      (RValue (VInt (x1 + x2)), st)
  | VString x1, VString x2, BopPlus -> (RValue (VString (x1 ^ x2)), st)
  | VString x1, VInt x2, BopPlus ->
      (RValue (VString (x1 ^ (x2 |> string_of_int))), st)
  | VInt x1, VString x2, BopPlus ->
      (RValue (VString (string_of_int x1 ^ x2)), st)
  | _, _, BopPlus -> (RValue VUd, st)
  | x1, x2, BopMinus -> (
      let x1' = x1 |> integer_of_value in
      let x2' = x2 |> integer_of_value in
      match (x1', x2') with
      | VInt a, VInt b -> (RValue (VInt (a - b)), st)
      | VUd, _ | _, VUd -> (RValue VUd, st)
      | _ -> failwith "will never reach this branch" )
  | x1, x2, BopTimes -> (
      let x1' = x1 |> integer_of_value in
      let x2' = x2 |> integer_of_value in
      match (x1', x2') with
      | VInt a, VInt b -> (RValue (VInt (a * b)), st)
      | VUd, _ | _, VUd -> (RValue VUd, st)
      | _ -> failwith "will never reach this branch" )
  | x1, x2, BopDiv -> (
      let x1' = x1 |> integer_of_value in
      let x2' = x2 |> integer_of_value in
      match (x1', x2') with
      | VInt a, VInt b ->
          if b <> 0 then (RValue (VInt (a / b)), st)
          else (RExpn (VString "Error: Division by zero"), st)
      | VUd, _ | _, VUd -> (RValue VUd, st)
      | _ -> failwith "will never reach this branch" )
  | x1, x2, BopMod -> (
      let x1' = x1 |> integer_of_value in
      let x2' = x2 |> integer_of_value in
      match (x1', x2') with
      | VInt a, VInt b ->
          if b <> 0 then (RValue (VInt (a mod b)), st)
          else (RExpn (VString "Error: Division by zero"), st)
      | VUd, _ | _, VUd -> (RValue VUd, st)
      | _ -> failwith "will never reach this branch" )
  | x1, x2, BopLt | x1, x2, BopLeq | x1, x2, BopGt | x1, x2, BopGeq ->
      eval_bop_compare
        (x1 |> prim_value_of, x2 |> prim_value_of, bop, env, st)
  | x1, x2, BopEq -> eval_equals (x1, x2, env, st)
  | x1, x2, BopNeq -> (
      match eval_equals (x1, x2, env, st) with
      | RValue v, st' -> (
          match v with
          | VBool true -> (RValue (VBool false), st')
          | VBool false -> (RValue (VBool true), st')
          | _ -> failwith "will never reach this branch" )
      | _ -> failwith "will never reach this branch" )
  | x1, x2, BopEqStrict -> eval_eqstrict (x1, x2, env, st)
  | x1, x2, BopNeqStrict -> (
      match eval_eqstrict (x1, x2, env, st) with
      | RValue v, st' -> (
          match v with
          | VBool true -> (RValue (VBool false), st')
          | VBool false -> (RValue (VBool true), st')
          | _ -> failwith "will never reach this branch" )
      | _ -> failwith "will never reach this branch" )
  | x1, VInt x2, BopAssign -> (
      match x1 with
      | VLoc ref_e ->
          let x = ref ref_e in
          x := x2;
          (RValue (VInt x2), (ref_e, VInt x2) :: st)
      | _ -> (RExpn (VString "Error: Assignment to non-location"), st) )
  | _ -> (RValue VUd, st)

let rec eval_expr (e, env, st) =
  match e with
  | EBool x -> (RValue (VBool x), st)
  | EInt x -> (RValue (VInt x), st)
  | EString x -> (RValue (VString x), st)
  | EUd -> (RValue VUd, st)
  | EBop (bop, e1, e2) -> expr_to_value_bop (bop, e1, e2, env, st)
  | EAnd (e1, e2) -> expr_to_value_and (e1, e2, env, st)
  | EOr (e1, e2) -> expr_to_value_or (e1, e2, env, st)
  | ELet (x, e1, e2) -> eval_let (x, e1, e2, env, st)
  | ELetRec (f, x, e1, e2) -> eval_let_rec (f, x, e1, e2, env, st)
  | EVar x -> eval_var (x, env, st)
  | EFunc (xs, e) -> (RValue (VClosure (xs, e, env)), st)
  | EAppl (e, es) -> eval_appl (e, es, env, st)
  | EIf (e1, e2, e3) -> eval_if (e1, e2, e3, env, st)
  | EIfPartial (e1, e2) -> eval_if (e1, e2, EUd, env, st)
  | ESeq (e1, e2) -> eval_seq (e1, e2, env, st)
  | ERef e -> eval_ref (e, env, st)
  | EUop (uop, e) -> expr_to_value_uop (uop, e, env, st)
  | EWhile (e1, e2) -> eval_while (e1, e2, env, st)
  | EThrow e -> eval_throw (e, env, st)
  | ETry (e1, x, e2) -> eval_try (e1, e2, x, env, st)
  | ETryF (e1, x, e2, e3) -> eval_tryf (e1, e2, e3, x, env, st)
  | _ -> (RValue VUd, st)

and expr_to_value_bop (bop, e1, e2, env, st) =
  match eval_expr (e1, env, st) with
  | RValue v', st' -> (
      match eval_expr (e2, env, st') with
      | RValue v'', st'' -> eval_bop (bop, v', v'', env, st'')
      | RExpn v'', st'' -> (RExpn v'', st'') )
  | RExpn v', st' -> (RExpn v', st')

and expr_to_value_uop (uop, e, env, st) =
  match eval_expr (e, env, st) with
  | RValue v', st' -> eval_uop (uop, v', env, st')
  | RExpn v', st' -> (RExpn v', st')

and expr_to_value_and (e1, e2, env, st) =
  match eval_expr (e1, env, st) with
  | RValue v', st' -> (
      match v' |> bool_of with
      | VBool false -> (RValue v', st')
      | VBool true -> (
          match eval_expr (e2, env, st') with
          | RValue v'', st'' -> (
              match v'' |> bool_of with
              | VBool false -> (RValue v'', st'')
              | VBool true -> (RValue v'', st'')
              | _ ->
                  (RExpn (VString "will never reach this branch"), st'')
              )
          | RExpn v'', st'' -> (RExpn v'', st'') )
      | _ -> (RExpn (VString "will never reach this branch"), st') )
  | RExpn v', st' -> (RExpn v', st')

and expr_to_value_or (e1, e2, env, st) =
  match eval_expr (e1, env, st) with
  | RValue v', st' -> (
      match v' |> bool_of with
      | VBool true -> (RValue v', st')
      | VBool false -> (
          match eval_expr (e2, env, st') with
          | RValue v'', st'' -> (
              match v'' |> bool_of with
              | VBool false -> (RValue v'', st'')
              | VBool true -> (RValue v'', st'')
              | _ ->
                  (RExpn (VString "will never reach this branch"), st'')
              )
          | RExpn v'', st'' -> (RExpn v'', st'') )
      | _ -> (RExpn (VString "will never reach this branch"), st') )
  | RExpn v', st' -> (RExpn v', st')

and eval_let (x, e1, e2, env, st) =
  match eval_expr (e1, env, st) with
  | RValue v', st' ->
      let env' = (x, v') :: env in
      eval_expr (e2, env', st')
  | RExpn v', st' -> (RExpn v', st')

and eval_var (x, env, st) =
  match List.assoc_opt x env with
  | None -> (RExpn (VString "Error: Unbound variable"), st)
  | Some v' -> (RValue v', st)

and eval_let_rec (f, x, e1, e2, env, st) =
  let env_ref = ref [] in
  env_ref := (f, VRecClosure (x, e1, env_ref)) :: env;
  eval_expr (e2, !env_ref, st)

and eval_appl (e, es, env, st) =
  match eval_expr (e, env, st) with
  | RValue (VClosure (xs, e', env')), st' -> (
      if List.length xs <> List.length es then
        (RExpn (VString "Error: Wrong number of arguments"), st')
      else
        try
          let n_env = eval_params xs es env env' st' in
          eval_expr (e', n_env, st')
        with Failure _ ->
          (RExpn (VString "Error: Wrong number of arguments"), st') )
  | RValue (VRecClosure (xs, e', env')), st' -> (
      if List.length xs <> List.length es then
        (RExpn (VString "Error: Wrong number of arguments"), st')
      else
        try
          let n_env = eval_params xs es env !env' st' in
          eval_expr (e', n_env, st')
        with Failure _ ->
          (RExpn (VString "Error: Wrong number of arguments"), st') )
  | RValue (VExtern (xs, f)), st' ->
      if xs <> List.length es then
        (RExpn (VString "Error: Wrong number of arguments"), st')
      else (RValue (VExtern (xs, f)), st')
  | RExpn v, st' -> (RExpn (VString "Error: Not a function"), st')
  | _ -> (RExpn (VString "Error: Not a function"), st)

and eval_params xs es env env' st =
  match (xs, es) with
  | h :: t, x :: s -> (
      let value, st' = eval_expr (x, env, st) in
      match value with
      | RValue v' -> eval_params t s env ((h, v') :: env') st'
      | _ -> failwith "will never reach this branch" )
  | [], [] -> env'
  | _ -> failwith "will never reach this branch"

and eval_if (e1, e2, e3, env, st) =
  match eval_expr (e1, env, st) with
  | RValue v, st' -> (
      match v |> bool_of with
      | VBool true -> eval_expr (e2, env, st')
      | VBool false -> eval_expr (e3, env, st')
      | _ -> (RExpn (VString "will never reach this branch"), st') )
  | RExpn v, st' -> (RExpn v, st')

and eval_seq (e1, e2, env, st) =
  match eval_expr (e1, env, st) with
  | RExpn v, st' -> (RExpn v, st')
  | RValue v, st' -> eval_expr (e2, env, st')

and eval_ref (e, env, st) =
  match eval_expr (e, env, st) with
  | RExpn v, st' -> (RExpn v, st')
  | RValue v, st' ->
      let len = List.length st' in
      let st'' = (len, v) :: st' in
      (RValue (VLoc len), st'')

and eval_while (e1, e2, env, st) =
  eval_expr (EIfPartial (e1, ESeq (e2, EWhile (e1, e2))), env, st)

and eval_throw (e, env, st) =
  match eval_expr (e, env, st) with
  | RValue v, st' | RExpn v, st' -> (RExpn v, st')

and eval_try (e1, e2, x, env, st) =
  match eval_expr (e1, env, st) with
  | (RValue v, st') as h -> h
  | RExpn v, st' -> eval_expr (e2, (x, v) :: env, st')

and eval_tryf (e1, e2, e3, x, env, st) =
  match eval_expr (e1, env, st) with
  | (RValue v, st') as h -> h
  | RExpn v, st' -> (
      let res = eval_expr (e2, (x, v) :: env, st') in
      match eval_expr (e3, env, res |> snd) with
      | RValue v', st'' -> (res |> fst, st'')
      | RExpn v', st'' -> (RExpn v', st'') )

let eval_defn (d, env, st) =
  match d with
  | DLet (x, e) -> (
      match eval_expr (e, env, st) with
      | RValue v', st' -> (RValue v', (x, v') :: env, st')
      | RExpn v', st' -> (RExpn v', env, st') )
  | DLetRec (f, x, e) ->
      let env_ref = ref [] in
      env_ref := (f, VRecClosure (x, e, env_ref)) :: env;
      let env'' = (f, VRecClosure (x, e, env_ref)) :: env in
      (RValue (VRecClosure (x, e, env_ref)), env'', st)

let eval_phrase (p, env, st) =
  match p with
  | Expr c ->
      let r, st' = eval_expr (c, env, st) in
      (r, env, st')
  | Defn d -> eval_defn (d, env, st)

let eval_expr_init e = eval_expr (e, initial_env, initial_state)
