
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyTuple of ty list
  | TyRecord of (string * ty) list
  | TyVarTy of string
  | TyList of ty
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
  | TmLetIn of string * term * term
  | TmFix of term
  | TmString of string
  | TmConcat of term * term
  | TmTuple of term list
  | TmProj of term * string
  | TmRecord of (string * term) list
  | TmNil of ty
  | TmCons of ty * term * term
  | TmIsNil of ty * term
  | TmHead of ty * term
  | TmTail of ty * term

;;

type command = 
    Eval of term
  | Bind of string * term
  | BindTy of string * ty
  | Quit
;; 

type binding = 
    TyBind of ty
  | TyTmBind of (ty*term)
;;

type context = 
    (string * binding) list
  ;;

val emptyctx : context;;
val addtbinding : context -> string -> ty -> context;;
val addvbinding : context -> string -> ty -> term -> context;;

val gettbinding : context -> string -> ty;;
val getvbinding : context -> string -> term;;


val string_of_ty : ty -> string;;
exception Type_error of string;;
val typeof : context -> term -> ty;;
val typeofTy : context -> ty -> ty;;

val string_of_term : term -> string;;
exception NoRuleApplies;;
val eval : context -> term -> term;;

val execute : context -> command -> context;;

