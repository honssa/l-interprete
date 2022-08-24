
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyPair of ty * ty
  | TyTuple of ty list
  | TyFloat
  | TyChar
  | TyString
  | TyRecord of (string * ty) list
;;

type context =
  (string * ty) list
;;

type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLet of string * term * term
  | TmEq of string * term
  | TmPair of term * term
  | TmProj1 of term
  | TmProj2 of term
  | TmTuple of term list
  | TmProj of term * int
  | TmProjRCD of term * string
  | TmFix of term
  | TmFloat of term
  | TmFloatDown of term
  | TmChar of char
  | TmString of term
  | TmRecord of (string * term) list
;;

type vcontext =
  (string * term * ty) list
;;

val emptyctx : context;;
val addbinding : context -> string -> ty -> context;;
val getbinding : context -> string -> ty;;

val string_of_ty : ty -> string;;
exception Type_error of string;;
val typeof : context -> term -> ty;;

val string_of_term : term -> string;;
exception NoRuleApplies;;
val eval : term -> vcontext -> bool -> term * vcontext;;

