
(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyTuple of ty list
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
;;

type command = 
    Eval of term
  | Bind of string * term
  | Quit
;;

type binding = 
    TyBind of ty
  | TyTmBind of (ty * term)
;;

type context = 
    (string * binding) list
;;


(* CONTEXT MANAGEMENT *)

let emptyctx =
  []
;;

let addtbinding ctx s ty =
  (s, TyBind ty) :: ctx
;;

let addvbinding ctx s ty tm =
  (s, TyTmBind(ty, tm)) :: ctx
;;

let gettbinding ctx s =
  match List.assoc s ctx with
    TyBind ty -> ty
    | TyTmBind (ty,_) -> ty
;;

let getvbinding ctx s =
  match List.assoc s ctx with
  TyTmBind (_, tm) -> tm
  | _ -> raise Not_found
;;


(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ty = 
  match ty with
  | TyBool -> "Bool"
  | TyNat -> "Nat"
  | TyString -> "String"
  | TyArr (ty1, ty2) ->
      let s1 = string_of_ty_prec ty1 10 in
      let s2 = string_of_ty_prec ty2 0 in
      if ty1 <> TyArr (ty1, ty2) then s1 ^ " -> " ^ s2
      else "(" ^ s1 ^ ") -> " ^ s2

  | TyTuple tys -> 
      let rec f = function
          [] -> ""
        | (ty::[]) -> string_of_ty ty
        | (ty::tail) -> string_of_ty ty ^ ", " ^ f tail
      in
      "(" ^ f tys ^ ")"
    
and string_of_ty_prec ty prec = 
  match ty with
  | TyArr (ty1, ty2) ->
      let s = string_of_ty ty in
      if prec > 0 then "(" ^ s ^ ")" else s
  | _ -> string_of_ty ty

exception Type_error of string
;;

let rec typeof ctx tm = match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof ctx t1 = TyBool then
        let tyT2 = typeof ctx t2 in
        if typeof ctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")

    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof ctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try gettbinding ctx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let ctx' = addtbinding ctx x tyT1 in
      let tyT2 = typeof ctx' t2 in
      TyArr (tyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if tyT2 = tyT11 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addtbinding ctx x tyT1 in
      typeof ctx' t2

    (* T-Fix *)
  | TmFix t1 ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if tyT11 = tyT12 then tyT12
             else raise (Type_error "result of body not compatible with domain")
         | _ -> raise (Type_error "arrow type expected"))
    (* new rule for string *)     
  | TmString _ -> TyString

   (* new rule for concat *)
  | TmConcat (t1, t2) -> 
      if typeof ctx t1 = TyString && typeof ctx t2 = TyString then TyString
      else raise (Type_error "arguments of concat are not strings")

  | TmTuple tms -> TyTuple (List.map (typeof ctx) tms)  (* Devuelve el typeof de cada elemento de la tupla *)  

  | TmProj (t1, i) -> 
    (match typeof ctx t1 with
        TyTuple tys -> 
              (try List.nth tys (int_of_string i - 1) with  
                  _ -> raise (Type_error ("cannot project element " ^ i ^ ", this index does not exist in the tuple")))
        | _ -> raise (Type_error "projection of non-tuple type"))
  ;;


(* TERMS MANAGEMENT (EVALUATION) *)

let rec string_of_term tm = string_of_term_prec tm 0

and string_of_term_prec tm prec =
  match tm with
  | TmTrue -> "true"
  | TmFalse -> "false"
  | TmIf (t1, t2, t3) ->
      let s = "if " ^ string_of_term_prec t1 0 ^ " then " ^
              string_of_term_prec t2 0 ^ " else " ^ string_of_term_prec t3 0 in
      if prec > 0 then "(" ^ s ^ ")" else s

  | TmZero -> "0"
  | TmSucc t ->
      let rec f n t' =
        match t' with
        | TmZero -> string_of_int n
        | TmSucc s -> f (n + 1) s
        | _ -> "succ (" ^ string_of_term_prec t 10 ^ ")"
      in
      f 1 t

  | TmPred t -> "pred " ^ string_of_term_prec t 10
  | TmIsZero t -> "iszero " ^ string_of_term_prec t 10

  | TmVar s -> s

  | TmAbs (s, tyS, t) ->
      let s = "lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term_prec t 0 in
      if prec > 0 then "(" ^ s ^ ")" else s

  | TmApp (t1, t2) ->
      let s = string_of_term_prec t1 10 ^ " " ^ string_of_term_prec t2 11 in
      if prec > 10 then "(" ^ s ^ ")" else s

  | TmLetIn (s, t1, t2) ->
      let s = "let " ^ s ^ " = " ^ string_of_term_prec t1 0 ^ " in " ^ string_of_term_prec t2 0 in
      if prec > 0 then "(" ^ s ^ ")" else s

  | TmFix t -> "fix " ^ string_of_term_prec t 10
  | TmString str -> "\"" ^ str ^ "\""
  | TmConcat (t1, t2) ->
      let s = "concat " ^ string_of_term_prec t1 10 ^ " " ^ string_of_term_prec t2 10 in
      if prec > 10 then "(" ^ s ^ ")" else s

  | TmTuple tms ->
    let rec f tms' =
      match tms' with
          [] -> ""
        | (tm::[]) -> string_of_term tm
        | (tm::t) -> string_of_term tm ^ ", " ^ f t
    in "{" ^ f tms ^ "}"

  | TmProj (t1, n) ->
      let rec proj_string t n =
        match t with
        | TmTuple tms ->
            if n > 0 && n <= List.length tms then string_of_term_prec (List.nth tms (n - 1)) 0
            else raise (Failure "tuple index out of bounds")
        | _ -> string_of_term_prec t 10 ^ "." ^ string_of_int n
      in proj_string t1 (int_of_string n)

  ;;




let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t ->
      free_vars t
  | TmString _ -> []
  | TmConcat (t1, t2) -> 
    lunion (free_vars t1) (free_vars t2)    
  | TmTuple tms -> List.flatten (List.map free_vars tms)
  | TmProj (t, n) -> free_vars t
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;

let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) ->
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmFix t ->
      TmFix (subst x s t)  
      
  | TmString st -> TmString st
  | TmConcat (t1, t2) -> TmConcat (subst x s t1, subst x s t2)
  | TmTuple tms -> TmTuple (List.map (subst x s) tms)

  | TmProj (t, n) -> TmProj (subst x s t, n)

;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmString _ -> true
  | TmTuple tms -> List.for_all isval tms
  | t when isnumericval t -> true
  | _ -> false
;;

exception NoRuleApplies
;;

let rec eval1 ctx tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 ctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 ctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 ctx t1 in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 ctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmLetIn (x, t1', t2)

    (* E-FixBeta *)  
  | TmFix (TmAbs (x, _, t2)) ->
      subst x tm t2

    (* E-Fix *)  
  | TmFix t1 ->
      let t1' = eval1 ctx t1 in
      TmFix t1'

    (* String 1 *)  
  | TmConcat(TmString s1, TmString s2) -> TmString (s1 ^ s2)

    (* String 2 *)  
  | TmConcat (TmString s1, t2) ->
      let t2' = eval1 ctx t2 in
      TmConcat (TmString s1, t2')

    (* String 3 *)  
  | TmConcat (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmConcat ( t1', t2)    

  | TmVar s ->
      getvbinding ctx s

  | TmTuple tup ->
      let rec eval_tuple tup = match tup with
        | [] -> raise NoRuleApplies
        | tm::t when isval tm -> tm::(eval_tuple t)
        | tm::t -> (eval1 ctx tm)::t
      in TmTuple (eval_tuple tup)

  (* E-ProjTuple *)
    | TmProj (TmTuple tms, n) ->
    let index = int_of_string n in
    if index > 0 && index <= List.length tms then List.nth tms (index - 1)
    else raise NoRuleApplies

(* E-Proj *)
  | TmProj (t, n) ->
    let t' = eval1 ctx t in
    TmProj (t', n)

  | _ ->
      raise NoRuleApplies
;;

let apply_ctx ctx tm = 
  List.fold_left (fun t x -> subst x (getvbinding ctx x) t) tm (free_vars tm)  
;;



let rec eval ctx tm =
  try
    let tm' = eval1 ctx tm in
    eval ctx tm'
  with
    NoRuleApplies -> apply_ctx ctx tm
;;

let execute ctx = function 
    Eval tm ->
      let tyTm = typeof ctx tm in
      let tm' = eval ctx tm in
      print_endline("- : " ^string_of_ty tyTm ^ " = " ^ string_of_term tm');
      ctx

  | Bind (s, tm) ->
    let tyTm = typeof ctx tm in
    let tm' = eval ctx tm in
    print_endline(s ^ " : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
    addvbinding ctx s tyTm tm'

  | Quit ->
      raise End_of_file
  ;;
