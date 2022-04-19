(** The abstract syntax tree type. *)

(******************************************************************************
   These types (id, unop, binop) are used by the parser.  You do not want to
   change them.
 ******************************************************************************)

type id = string

type unop =
  | UopMinus
  | UopNot
  | UopTypeof
  | UopDeref

type binop =
  | BopPlus
  | BopMinus
  | BopTimes
  | BopDiv
  | BopMod
  | BopLt
  | BopLeq
  | BopGt
  | BopGeq
  | BopEq
  | BopNeq
  | BopEqStrict
  | BopNeqStrict
  | BopAssign
  | BopUpdate

(******************************************************************************
   [expr] is the type of the AST for expressions. You may implement
   this type however you wish.  Use the example interpreters seen in
   the textbook as inspiration.
 ******************************************************************************)

type expr =
  | EBool of bool
  | EInt of int
  | EString of string
  | EUd
  | EBop of binop * expr * expr
  | EAnd of expr * expr
  | EOr of expr * expr
  | ELet of id * expr * expr
  | ELetRec of id * id list * expr * expr
  | EVar of id
  | EFunc of id list * expr
  | EAppl of expr * expr list
  | EIf of expr * expr * expr
  | EIfPartial of expr * expr
  | ESeq of expr * expr
  | ERef of expr
  | EUop of unop * expr
  | EWhile of expr * expr
  | EThrow of expr
  | ETry of expr * id * expr
  | ETryF of expr * id * expr * expr
  | EObj of id * expr list

(******************************************************************************
  [defn] is the type of the AST for definitions. You may implement this
  type however you wish. There is only one kind of definition---the let
  [rec] definition---so this type can be quite simple.
  ******************************************************************************)

type defn =
  | DLet of id * expr
  | DLetRec of id * id list * expr

(******************************************************************************
   [phrase] is the type of the AST for phrases. It is used by the
   parser.  You do not want to change it.
 ******************************************************************************)

type phrase =
  | Expr of expr
  | Defn of defn
