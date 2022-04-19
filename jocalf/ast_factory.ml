open Ast

let make_let_defn x e = DLet (x, e)

let make_let_rec_defn f x e = DLetRec (f, x, e)

let make_seq e1 e2 = ESeq (e1, e2)

let make_app e es = EAppl (e, es)

let make_unop uop e = EUop (uop, e)

let make_binop bop e1 e2 = EBop (bop, e1, e2)

let make_and e1 e2 = EAnd (e1, e2)

let make_or e1 e2 = EOr (e1, e2)

let make_if e1 e2 e3 = EIf (e1, e2, e3)

let make_if_partial e1 e2 = EIfPartial (e1, e2)

let make_let x e1 e2 = ELet (x, e1, e2)

let make_let_rec f x e1 e2 = ELetRec (f, x, e1, e2)

let make_try e1 x e2 = ETry (e1, x, e2)

let make_try_finally e1 x e2 e3 = ETryF (e1, x, e2, e3)

let make_throw e = EThrow e

let make_ref e = ERef e

let make_fun xs e = EFunc (xs, e)

let make_while e1 e2 = EWhile (e1, e2)

let make_delete_field e1 e2 = failwith "Unimplemented"

let make_var x = EVar x

let make_int s = EInt (s |> int_of_string)

let make_string s = EString s

let make_bool b = EBool b

let make_undefined () = EUd

let make_object fields = failwith "Unimplemented"

let make_get_field e1 e2 = failwith "Unimplemented"
