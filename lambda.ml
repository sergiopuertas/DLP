
(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyTuple of ty list
  | TyRecord of (string * ty) list
  | TyVarTy of string
  | TyList of ty
  | TyVariant of (string * ty) list

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
  | TmCase of term * (string * string * term) list
  | TmTag of string * term * ty
;;



type command = 
    Eval of term
  | Bind of string * term
  | BindTy of string * ty
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
exception Type_error of string

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
        | (ty::t) -> string_of_ty ty ^ ", " ^ f t
      in
      "(" ^ f tys ^ ")"
  | TyRecord tys ->
    let rec f = function
        [] -> ""
      | (s, ty)::[] -> s ^ ": " ^ string_of_ty ty
      | (s, ty)::t -> s ^ ": " ^ string_of_ty ty ^ ", " ^ f t
    in
    "{" ^ f tys ^ "}"
  | TyVarTy s -> s
  | TyList ty -> 
    "List [" ^ string_of_ty ty ^ "]"
  
    | TyVariant tys -> 
      let rec f = function
          [] -> ""
        | (s, ty)::[] -> s ^ ": " ^ string_of_ty ty
        | (s, ty)::t -> s ^ ": " ^ string_of_ty ty ^ ", " ^ f t
      in
      "<" ^ f tys ^ ">"


    
and string_of_ty_prec ty prec = 
  match ty with
  | TyArr (ty1, ty2) ->
      let s = string_of_ty ty in
      if prec > 0 then "(" ^ s ^ ")" else s
  | _ -> string_of_ty ty
;;
let rec base_ty ctx = function
    TyBool -> TyBool
  | TyNat -> TyNat
  | TyString -> TyString
  | TyArr (t1,t2) -> TyArr (base_ty ctx t1, base_ty ctx t2)
  | TyTuple tms -> TyTuple (List.map (base_ty ctx) tms)
  | TyRecord tms -> TyRecord (List.map (fun (s, ty) -> (s, base_ty ctx ty)) tms)
  | TyVariant tms -> TyVariant (List.map (fun (s, ty) -> (s, base_ty ctx ty)) tms)
  | TyList ty -> TyList (base_ty ctx ty)
  | TyVarTy s -> (try gettbinding ctx s with _ -> raise (Type_error ("type variable " ^ s ^ " not found")))

let rec typeofTy ctx ty =
  match ty with
      TyBool ->
        TyBool
    | TyNat ->
        TyNat
    | TyString ->
        TyString
    | TyVarTy s ->
          gettbinding ctx s
    | TyVariant l -> 
            TyVariant (List.map (fun (s, ty) -> (s, typeofTy ctx ty)) l)
    | _ -> raise (Type_error "type not supported")
;;


let rec subtypeof t1 t2 = match (t1, t2) with
  | (TyArr(x1, x2), TyArr(y1, y2)) -> ((subtypeof x1 y1) && (subtypeof y2 x2))
  | (TyRecord(r1), TyRecord(r2)) ->
    let check (j, ty) l =
      try 
        subtypeof ty (List.assoc j l)
    with _ -> false
    in let rec contains r1 r2 = match r1 with
      | [] -> true
      | (h::t) -> (&&) (check h r2) (contains t r2)
      in contains r1 r2
  | (t1, t2) -> t1 = t2
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
      let btyT1 = base_ty ctx tyT1 in
      let ctx' = addtbinding ctx x btyT1 in
      let tyT2 = typeof ctx' t2 in
      TyArr (btyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if subtypeof tyT2 tyT11 then tyT12
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
            if subtypeof tyT11 tyT12 then tyT12
             else raise (Type_error "result of body not compatible with domain")
         | _ -> raise (Type_error "arrow type expected"))
    (* new rule for string *)     
  | TmString _ -> TyString

   (* new rule for concat *)
  | TmConcat (t1, t2) -> 
      if typeof ctx t1 = TyString && typeof ctx t2 = TyString then TyString
      else raise (Type_error "arguments of concat are not strings")

  | TmTuple tms -> TyTuple (List.map (typeof ctx) tms)  

  | TmProj (t1, i) -> 
    (match typeof ctx t1 with
        TyTuple tys -> 
              (try List.nth tys (int_of_string i - 1) with  
                  _ -> raise (Type_error ("cannot project element " ^ i ^ ", this index does not exist in the tuple")))
        | TyRecord tys ->
              (try List.assoc i tys with
              _ -> raise (Type_error ("cannot project element " ^ i ^ ", this index does not exist in the record")))
        | _ -> raise (Type_error "projection of non-tuple type"))

  | TmRecord tms -> TyRecord (List.map (fun (s, tm) -> (s, typeof ctx tm)) tms)
  | TmNil ty -> TyList ty
  | TmCons (ty, t1, t2) -> 
    let tyb = base_ty ctx ty in
      if typeof ctx t1 = tyb then 
        if typeof ctx t2 = TyList tyb then TyList tyb
        else raise (Type_error "second argument of cons is not a list")
      else raise (Type_error "first argument of cons does not match the type of the list")
  | TmIsNil (ty, t) -> 
    let tyb = base_ty ctx ty in
      if typeof ctx t = TyList tyb then TyBool
      else raise (Type_error "argument of isnil is not a list")
  | TmHead (ty, t) ->
    let tyb = base_ty ctx ty in
      if typeof ctx t = TyList tyb then tyb
      else raise (Type_error "argument of head is not a list")
  | TmTail (ty, t) -> 
    let tyb = base_ty ctx ty in
      if typeof ctx t = TyList tyb then TyList tyb
      else raise (Type_error "argument of tail is not a list")
  
  | TmTag(s,t,ty) ->
        let tyT1 = typeof ctx t in
        let tyT2 = base_ty ctx ty in
        (match tyT2 with
        | TyVariant l -> 
          (try 
            if tyT1 = List.assoc s l then tyT2
            else raise (Type_error "field does not match the type of the variant")
          with
            Not_found -> raise (Type_error ( "case " ^ s ^ "not present in the variant")))
        | _ -> raise (Type_error "variant type expected"))
        
  | TmCase(t,cases) ->
        let tyT1 = typeof ctx t in 
        match tyT1 with
            TyVariant l -> 
              let vtags = List.map (fun (tag,_) -> tag) l in
              let ctags = List.map (fun (tag,_,_) -> tag) cases in
              if List.length vtags = List.length ctags && List.for_all (function tag -> List.mem tag vtags) ctags 
                then
                let (tag1, id1, tm1) = List.hd cases in
                let ty1 = List.assoc tag1 l in
                let ctx1 = addtbinding ctx id1 ty1 in
                let rty =  typeof ctx1 tm1 in
                let rec aux = function
                [] -> rty
                  | (tagi, idi, tmi)::rest -> 
                    let tyi = List.assoc tagi l in
                    let ctxi = addtbinding ctx idi tyi in
                    let tyi =  typeof ctxi tmi in
                    if tyi = rty then aux rest
                    else raise (Type_error "case branches have different types")
                  in aux (List.tl cases)
                else raise (Type_error "case branches do not match the variant")
            | _ -> raise (Type_error "variant type expected")

;;

(* TERMS MANAGEMENT (EVALUATION) *)

let rec string_of_term tm = string_of_term_prec tm 0 0

and string_of_term_prec tm prec indent =
  let indent_str n = String.make (n * 2) ' ' in
  match tm with
  | TmTrue -> "true"
  | TmFalse -> "false"
  | TmIf (t1, t2, t3) ->
      let s = "if " ^ string_of_term_prec t1 0 (indent + 1) ^ " then " ^
              string_of_term_prec t2 0 (indent + 1) ^"\n" ^ indent_str (indent - 1) ^ " else " ^
              string_of_term_prec t3 0 (indent + 1) in
      if prec > 0 then "(" ^ s ^ ")" else s

  | TmZero -> "0"
  | TmSucc t ->
      let rec f n t' =
        match t' with
        | TmZero -> string_of_int n
        | TmSucc s -> f (n + 1) s
        | _ -> "succ (" ^ string_of_term_prec t 10 (indent + 1) ^ ")"
      in
      f 1 t

  | TmPred t -> "pred " ^ string_of_term_prec t 10 (indent + 1)
  | TmIsZero t -> "iszero " ^ string_of_term_prec t 10 (indent + 1)

  | TmVar s -> s

  | TmAbs (s, tyS, t) ->
      let s = "lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ".\n" ^ indent_str (indent + 1 ) ^ string_of_term_prec t 0 (indent + 2) in
      if prec > 0 then "(" ^ s ^ ")" else s

  | TmApp (t1, t2) ->
      let s = string_of_term_prec t1 10 (indent + 1) ^ " " ^ string_of_term_prec t2 11 (indent + 1) in
      if prec > 10 then "(" ^ s ^ ")" else s

  | TmLetIn (s, t1, t2) ->
      let s = "let " ^ s ^ " = " ^ string_of_term_prec t1 0 (indent + 1) ^ " in\n" ^ indent_str (indent + 1) ^ string_of_term_prec t2 0 (indent + 1) in
      if prec > 0 then "(" ^ s ^ ")" else s

  | TmFix t -> "fix " ^ string_of_term_prec t 10 (indent + 1)
  | TmString str -> "\"" ^ str ^ "\""
  | TmConcat (t1, t2) ->
      let s = "concat " ^ string_of_term_prec t1 10 (indent + 1) ^ " " ^ string_of_term_prec t2 10 (indent + 1) in
      if prec > 10 then "(" ^ s ^ ")" else s

  | TmTuple tms ->
    let rec f tms' =
      match tms' with
          [] -> ""
        | (tm::[]) -> string_of_term_prec tm 0 (indent + 1)
        | (tm::t) -> string_of_term_prec tm 0 (indent + 1) ^ ", " ^ f t
    in "{" ^ f tms ^ "}"

  | TmProj (t1, n) ->
      let rec proj_string t n =
        match t with
        | TmTuple tms ->
            if n > 0 && n <= List.length tms then string_of_term_prec (List.nth tms (n - 1)) 0 (indent + 1)
            else raise (Failure "tuple index out of bounds")
        | _ -> string_of_term_prec t 10 (indent + 1) ^ "." ^ string_of_int n
      in proj_string t1 (int_of_string n)

  | TmRecord tms -> 
      let rec f = function
            [] -> ""
          | (s, tm)::[] -> s ^ ": " ^ string_of_term_prec tm 0 (indent + 1)
          | (s, tm)::t -> s ^ ": " ^ string_of_term_prec tm 0 (indent + 1) ^ ", " ^ f t
      in "{" ^ f tms ^ "}"
  | TmNil ty -> "nil [" ^ string_of_ty ty ^ "]"
  | TmCons (ty, t1, t2) -> "cons[" ^string_of_ty ty ^ "] (" ^ string_of_term_prec t1 0 (indent + 1) ^ " " ^ string_of_term_prec t2 0 (indent + 1) ^ ")"
  | TmIsNil (ty, t) -> "isnil [" ^ string_of_ty ty ^ "] (" ^ string_of_term_prec t 0 (indent + 1) ^ ")"
  | TmHead (ty, t) -> "head[" ^string_of_ty ty ^ "] (" ^ string_of_term_prec t 0 (indent + 1) ^ ")"
  | TmTail (ty, t) -> "tail[" ^string_of_ty ty ^ "] (" ^ string_of_term_prec t 0 (indent + 1) ^ ")"
  | TmTag (s, t, ty) -> "<" ^ s ^ " = " ^ string_of_term_prec t 0 (indent + 1) ^ "> as " ^ string_of_ty ty 
  | TmCase (t, cases) -> 
      let rec f = function
          [] -> ""
        | (s, id, tm)::[] -> s ^ " " ^ id ^ " => " ^ string_of_term_prec tm 0 (indent + 1)
        | (s, id, tm)::t -> s ^ " " ^ id ^ " => " ^ string_of_term_prec tm 0 (indent + 1) ^ " | " ^ f t
      in "case " ^ string_of_term_prec t 0 (indent + 1) ^ " of " ^ f cases

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
  | TmRecord tms -> List.flatten (List.map (fun (s, tm) -> free_vars tm) tms)
  | TmNil ty -> []
  | TmCons (ty, t1, t2) -> lunion (free_vars t1) (free_vars t2)
  | TmIsNil (ty, t) -> free_vars t
  | TmHead (ty, t) -> free_vars t
  | TmTail (ty, t) -> free_vars t
  | TmTag (_, t, _) -> free_vars t
  | TmCase (t,cases)-> lunion (free_vars t) (List.fold_left (fun fvar (label,id,tm) -> lunion (ldif(free_vars tm) [id]) fvar) [] cases)

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
  | TmRecord tms -> TmRecord (List.map (fun (label, tm) -> (label, subst x s tm)) tms)
  | TmNil ty -> tm
  | TmCons (ty, t1, t2) -> TmCons (ty, subst x s t1, subst x s t2)
  | TmIsNil (ty, t) -> TmIsNil (ty, subst x s t)
  | TmHead (ty, t) -> TmHead (ty, subst x s t)
  | TmTail (ty, t) -> TmTail (ty, subst x s t)
  | TmTag (label, tm, ty) -> TmTag (label, subst x s tm, ty)
  | TmCase(t,cases) -> TmCase (subst x s t, 
      List.map (fun (label, id, t1) -> 
      if id = x then (label, id, t1) else (label, id, subst x s t1)) cases)

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
  | TmRecord tms -> List.for_all (fun (_, tm) -> isval tm) tms
  | TmNil _ -> true
  | TmCons (_, t1, t2) -> isval t1 && isval t2
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
    
      (* E-Tuple *)
  | TmTuple tup ->
      let rec eval_tuple tup = match tup with
        | [] -> raise NoRuleApplies
        | tm::t when isval tm -> tm::(eval_tuple t)
        | tm::t -> (eval1 ctx tm)::t
      in TmTuple (eval_tuple tup)

   (* E-Record *)
  | TmRecord tms ->
    let rec eval_record tms = match tms with
      | [] -> raise NoRuleApplies
      | (s, tm)::t when isval tm -> (s, tm)::(eval_record t)
      | (s, tm)::t -> (s, eval1 ctx tm)::t
    in TmRecord (eval_record tms)

    (* E-ProjTuple *)
  | TmProj (TmTuple tms, n) ->
    let index = int_of_string n in
    if index > 0 && index <= List.length tms then List.nth tms (index - 1)
    else raise NoRuleApplies

  (* E-ProjRecord1 *)
  | TmProj (TmRecord tms, s) when isval (TmRecord tms) -> 
    List.assoc s tms

  (* E-ProjRecord2 *)
  | TmProj(TmRecord tms, s) ->
    (try List.assoc s tms with
      _ -> raise NoRuleApplies)

  (* E-Proj *)
  | TmProj (t, n) ->
    (match eval1 ctx t with
      | TmTuple _ | TmRecord _ as t' -> TmProj (t', n)
      | _ -> raise NoRuleApplies)
  
  (* E-Cons1 *)
  | TmCons (ty, t1, t2) when isval t1 -> TmCons (ty, t1, eval1 ctx t2)

  (* E-Cons2 *)
  | TmCons (ty, t1, t2) -> TmCons (ty, eval1 ctx t1, t2)

  (* E-IsNilNil *)
  | TmIsNil (ty, TmNil (_)) -> TmTrue

  (* E-IsNilCons *)
  | TmIsNil (ty, TmCons (_, _, _)) -> TmFalse

  (* E-IsNil *)
  | TmIsNil (ty, t) -> TmIsNil (ty, eval1 ctx t)

  (* E-HeadCons *)
  | TmHead (ty, TmCons (_, t1, _)) -> t1

  (* E-Head *)
  | TmHead (ty, t) -> TmHead (ty, eval1 ctx t)

  (* E-TailCons *)
  | TmTail (ty, TmCons (_, _, t2)) -> t2

  (* E-Tail *)
  | TmTail (ty, t) -> TmTail (ty, eval1 ctx t)  
  

    (*E-CaseVariant*)
  | TmCase (TmTag (s, v, _), cases)
      when isval v ->
        let (_,id,tm) = List.find (fun (tag,_,_) -> tag = s) cases in subst id v tm
  
  (*E-Case*)
  | TmCase (t, cases) ->
    let t1' = eval1 ctx t in
    TmCase (t1', cases)
  (*E-Tag*)
  | TmTag (s, t, ty) ->
    TmTag (s, eval1 ctx t, ty)


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
  | BindTy (s, ty) ->
    let ty' = typeofTy ctx ty in
    print_endline("- : " ^ s ^ " : " ^ string_of_ty ty');
    addtbinding ctx s ty'

  | Quit ->
      raise End_of_file
  ;;