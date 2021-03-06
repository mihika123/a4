(******************************************************************)
(* DO NOT EDIT THIS FILE                                          *)
(******************************************************************)

module type InterpSig = sig
  val interp_expr : string -> string
end

module InterpCheck : InterpSig = Interp

module type AuthorsSig = sig
  val hours_worked : int list
end

module AuthorsCheck : AuthorsSig = Authors
