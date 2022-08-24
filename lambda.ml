open Printf
open String
(* TYPE DEFINITIONS *)

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


(* CONTEXT MANAGEMENT *)

let emptyctx =
  []
;;

let addbinding ctx x bind =
  (x, bind) :: ctx
;;

let getbinding ctx x =
  List.assoc x ctx
;;


(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyArr (TyArr(ty1,ty2),ty3) -> string_of_ty(TyArr(ty1,ty2)) ^ " -> " ^ string_of_ty(ty3)
  | TyArr (ty3,TyArr(ty1,ty2)) -> string_of_ty(ty3) ^ " -> " ^ string_of_ty(TyArr(ty1,ty2))
  | TyArr (ty1, ty2) -> string_of_ty ty1 ^ "" ^ " -> " ^ "" ^ string_of_ty ty2
  | TyPair (ty1,ty2) -> string_of_ty(ty1) ^ " * " ^ string_of_ty(ty2)
  | TyTuple (ty1::[]) -> string_of_ty(ty1) ^ " Tuple"
  | TyTuple (ty1::tail) -> string_of_ty(ty1) ^ " * " ^ string_of_ty(TyTuple(tail))
  | TyTuple ([]) -> "Empty Tuple"
  | TyFloat -> "Float"
  | TyChar -> "Char"
  | TyString -> "String"
  | TyRecord ((s,ty1)::tail) -> string_of_ty(ty1) ^ " * " ^ string_of_ty(TyRecord(tail))
  | TyRecord ([]) -> "Record"

;;

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
      (try getbinding ctx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let ctx' = addbinding ctx x tyT1 in
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
  | TmLet (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addbinding ctx x tyT1 in
      typeof ctx' t2
    (**)
  | TmEq(x,t) ->
      typeof(ctx)(t)
    (*T-Pair*)
  | TmPair(t1,t2) ->
      TyPair(typeof(ctx)(t1),typeof(ctx)(t2))
    (*T-Proj1*)
  | TmProj1(t) -> (match typeof(ctx)(t) with
      TyPair(ty1,_) -> ty1
      | _ -> raise(Type_error "projection applied to a non-pair type"))
    (*T-Proj2*)
  | TmProj2(t) -> (match typeof(ctx)(t) with
      TyPair(_,ty2) -> ty2
      | _ -> raise(Type_error "projection applied to a non-pair type"))
    (*T-Fix*)  
  | TmFix t ->       
      let tyT = typeof ctx t in 
      (match tyT with
         TyArr (tyT1, tyT2) ->
           if tyT1 = tyT2 then tyT1
           else raise (Type_error "parameter type mismatch on recursion")
        | _ -> raise (Type_error "arrow type expected RECURSION")
      )

  (*T-Rcd*)
  | TmTuple(tml) -> TyTuple(List.map(function tm -> typeof(ctx)(tm))tml)
  (*T-Proj*)
  | TmProj(t,i) -> (match typeof(ctx)(t) with
	TyTuple(tyl) -> (try List.nth(tyl)(i-1) with Failure "nth" -> raise(Type_error "tuple too short for this projection"))
        | _ -> raise(Type_error "projection applied to a non-tuple type"))
  | TmFloat(t1) -> (match t1 with 
        TmFloat(t2) -> typeof(ctx)(TmFloat t2)
        | TmVar(t2) -> TyFloat
        | TmFloatDown(t2) -> typeof(ctx)(TmFloatDown t2)
        | t -> if (typeof ctx t) = TyNat then TyFloat
		else raise(Type_error "floating point on something that isnt a natural number"))
  | TmFloatDown(t1) -> (match t1 with
        | TmFloat(t2) -> if (typeof ctx t2) = TyNat then TyNat else typeof(ctx)(TmFloat t2)
        | TmVar(t2) -> TyNat
        | TmFloatDown(t2) -> typeof(ctx)(TmFloatDown t2)
        | t -> TyNat)
  | TmChar (ch)-> TyChar
  | TmString(_) -> TyString
  | TmRecord(tml) -> TyRecord(List.map(function (s,tm) -> (s, typeof(ctx)(tm))) tml )
  | TmProjRCD(t,k) -> (match typeof(ctx)(t) with
      TyRecord(tyl) -> (try List.assoc k (tyl) with Failure "nth" -> raise(Type_error "tuple too short for this projection"))
            | _ -> raise(Type_error "projection applied to a non-tuple type"))


;;


(* TERMS MANAGEMENT (EVALUATION) *)

let rec string_of_term = 
  let rec string_of_tuple = function
     h::[] -> string_of_term h 
     | h::t -> string_of_term h ^ " * " ^ string_of_tuple t
     | [] -> ""
  in
  let rec string_of_float i = function
     TmFloat(t) -> string_of_float(i+1)(t)
     | t -> let s = string_of_term(t) in if (String.length(s) <= i)
					 then  let rec zero_string = function
							0 -> ""
							| i -> "0"^zero_string(i-1)
						in "0." ^ zero_string(i-String.length(s)) ^ s
					 else  String.sub(s)(0)(String.length(s)-i) ^ "." ^ String.sub(s)(String.length(s)-i)(i)
  in
  let rec string_of_record l = match l with
     (s,h)::[] -> s ^ ":=" ^ string_of_term h
     | (s,h)::t -> s ^ ":=" ^ string_of_term h ^ ", " ^ string_of_record t
     | [] -> ""
  in
  function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred(TmVar s) ->
      "pred " ^ string_of_term(TmVar s)
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero(TmVar s) ->
      "iszero " ^ string_of_term(TmVar s)
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t
  | TmApp(TmAbs(s,tyS,t),t1) -> "lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". ( " ^ string_of_term t ^ "" ^ " ) " ^ string_of_term t1

  | TmApp (t,TmApp(t1,t2)) -> string_of_term t ^ "(" ^ string_of_term(TmApp(t1,t2)) ^")"
  | TmApp (TmApp(t1,t2),t) -> string_of_term(TmApp(t1,t2)) ^ " " ^ string_of_term t
  | TmApp (t1, t2) -> string_of_term t1 ^ " " ^ string_of_term t2
  | TmLet (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmEq(x,t) -> x ^" = "^ string_of_term t
  | TmPair(t1,t2) -> "{"^string_of_term t1 ^ " * " ^ string_of_term t2 ^"}"
  | TmProj1(t) -> string_of_term(t)^".1"
  | TmProj2(t) -> string_of_term(t)^".2"
  | TmFix t -> "fix " ^ string_of_term t
  | TmTuple(t) -> "{" ^ string_of_tuple(t) ^ "}"
  | TmProj(t,i) -> string_of_term(t) ^ "." ^ string_of_int(i)
  | TmProjRCD(t,k) -> string_of_term(t) ^ "." ^ k
  | TmFloat(t) -> string_of_float(0)(TmFloat(t))
  | TmFloatDown(t) -> "*10(" ^ string_of_term(t) ^ ")"
  | TmChar(c) -> String.make 1 c
  | TmString(TmTuple(h::t)) -> string_of_term(h) ^ string_of_term(TmString(TmTuple(t)))
  | TmString(_) -> ""
  | TmRecord(t) -> "@{" ^ string_of_record(t) ^ "}"
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
  | TmChar c ->
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
  | TmLet (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmEq (x,t) -> [x]
  | TmFix t -> free_vars t
  | TmPair (t1,t2) -> lunion (free_vars(t1)) (free_vars(t2))
  | TmProj1 (TmPair(t1,_)) -> free_vars(t1)
  | TmProj1 (_) -> []
  | TmProj2(TmPair(_,t2)) -> free_vars(t2)
  | TmProj2 (_) -> []
  | TmTuple(h::t) -> lunion (free_vars(h))(free_vars(TmTuple(t)))
  | TmTuple([]) -> []
  | TmProj(TmTuple(tl),i) -> free_vars(List.nth(tl)(i-1))
  | TmProj(_,_) -> []
  | TmFloat(t) -> free_vars(t)
  | TmFloatDown(t) -> free_vars(t)
  | TmString(t) -> free_vars(t)
  | TmRecord((l,tm)::t) -> lunion (free_vars(tm)) (free_vars(TmRecord(t)))
  | TmRecord([]) -> []
  | TmProjRCD(TmRecord(tl),k) -> let rec aux k l = match l with
      (var,h2)::tail -> if var=k then h2 else aux k tail
      | [] -> raise(Type_error "record key not fount for this projection")
    in free_vars(aux k tl)
  | TmProjRCD(_,_) -> []
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
  | TmChar c ->
      TmChar c
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
  | TmLet (y, t1, t2) ->
      if y = x then TmLet (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLet (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLet (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmEq (x,t) -> TmEq(x,subst x s t)
  | TmPair (t1,t2) -> TmPair(subst x s t1,subst x s t2)
    (*E-Proj1*)
  | TmProj1(t) -> (match t with
      TmPair(t1,t2) -> TmProj1(TmPair(subst x s t1,t2))
      | _ -> TmProj1(subst x s t))
    (*E-Proj2*)
  | TmProj2(t) -> (match t with
      TmPair(t1,t2) -> TmProj2(TmPair(t1,subst x s t2))
      | _ -> TmProj2(subst x s t))
  | TmFix t ->
       TmFix (subst x s t)
  | TmTuple(t) -> TmTuple(List.map(fun tm -> (subst x s tm))(t))
    (*E-Proj*)
  | TmProj(t,i) -> (match t with
      TmTuple(t) -> TmProj(TmTuple(List.mapi(fun j tm -> if j = i then (subst x s tm) else tm)(t)),i)
      | _ -> TmProj(subst x s t,i))
  | TmFloat(t) ->
      TmFloat(subst x s t)
  | TmFloatDown(t) ->
      TmFloatDown(subst x s t)
  | TmString(t) -> TmString(subst x s t)
  (*| TmRecord(tml) -> TyRecord(List.map(function (s,tm) -> typeof(ctx)(tm))tml)*)
  | TmRecord(t) -> TmRecord(List.map(fun (v,tm) -> (v,(subst x s tm))) (t))
    (*E-ProjRCD*)
  | TmProjRCD(t, k) -> let aux tm k = (match tm with
      (var,h2) -> if var=k then (var,(subst x s h2)) else (var,h2))
    in let rec aux2 l1 l2 k = (match l1 with
      h::t -> aux2 t ((aux h k)::l2) k 
      | [] -> (List.rev l2) )
    in (match t with
      | TmRecord(t) -> TmProjRCD(TmRecord(aux2 t [] k),k)
      | _ -> TmProjRCD(subst x s t,k))
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | TmFloat(t) -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with  (*Incluir TmVar _ -> true ?*)
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmPair(t1,t2) -> isval t1 && isval t2 (*v::= {v,v}*)
  | TmTuple(h::t) -> isval h && isval(TmTuple(t)) (*v::= {vi i∈1..n}*)
  | TmTuple([]) -> true
  | TmRecord((h1,h2)::t) -> isval h2 && isval(TmRecord(t))
  | TmRecord([]) -> true
  | t when isnumericval t -> true
  | _ -> false
;;

exception NoRuleApplies
;;
exception TermIs

let rec eval1 tm vctx = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 t1 vctx in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 t1 vctx in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 t1 vctx in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 t1 vctx in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 t2 vctx in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 t1 vctx in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLet (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLet(x, t1, t2) ->
      let t1' = eval1 t1 vctx in
      TmLet (x, t1', t2) 
    (**)
  | TmEq(x,t) ->
      TmEq(x,t)
  | TmPair(t1,t2) ->
     (*E-Pair2*)
      if (isval t1) then TmPair(t1,eval1 t2 vctx)
     (*E-Pair2*)
      else TmPair(eval1 t1 vctx,t2)
    (*E-PairBeta1*)
  | TmProj1(tm1) ->
	(let rec auxeval t =
		(try auxeval(eval1(t)(vctx))
		with NoRuleApplies -> t) in match auxeval(tm1) with 
	TmPair(t1,t2) -> t1
	| t3 -> t3 )
    (*E-PairBeta2*)
  | TmProj2(tm1) ->
	(let rec auxeval t =
		(try auxeval(eval1(t)(vctx))
		with NoRuleApplies -> t) in match auxeval(tm1) with 
	TmPair(t1,t2) -> t2
	| t3 -> t3 )
    (*E-FixBeta*)
  | TmFix (TmAbs(x,tt,t1)) ->
      subst x (TmFix(TmAbs(x,tt,t1))) t1
    (*E-Fix*)    
  | TmFix (t) ->
      let t' = eval1 t vctx in
      TmFix t'

     (*E-Tuple*)
  | TmTuple(t) -> let rec aux t l = match t with
      head::tail -> if (isval head) then aux(tail)([head]@l) else TmTuple(List.rev(l)@[eval1 head vctx]@tail)
      | [] -> raise NoRuleApplies
      in aux t []
    (*E-ProjTuple*)
  | TmProj(tm1,i) ->
	(let rec auxeval t =
		(try auxeval(eval1(t)(vctx))
		with NoRuleApplies -> t) in match auxeval(tm1) with 
	TmTuple(t1) -> List.nth(t1)(i-1)
	| t2 -> t2 )
      (*List.nth(t1)(i-1)*)
    (*E-RCD*)
  | TmRecord(t) -> let rec aux t l = match t with
      (h1,h2)::tail -> if (isval h2) then aux(tail)([(h1,h2)]@l) else TmRecord(List.rev(l)@[(h1, eval1 h2 vctx)]@tail)
      | [] -> raise NoRuleApplies
      in aux t []
    (*E-ProjRCD*)
  | TmProjRCD(tm1,k) -> 
    (let rec auxeval t =
      (try auxeval(eval1(t)(vctx))
      with NoRuleApplies -> t) in match auxeval(tm1) with 
        TmRecord(tl) -> List.assoc k tl
        | t2 -> t2 )

  | TmFloat t1 ->
      let t1' = eval1 t1 vctx in
      TmFloat t1'

  | TmFloatDown(TmSucc nv) ->
      (TmSucc(nv))
  | TmFloatDown (TmFloat nv1) when isnumericval nv1 ->
      nv1
  | TmFloatDown t1 ->
      let t1' = eval1 t1 vctx in
      TmFloatDown t1'



  | _ ->
      raise NoRuleApplies
;;

let rec substcontext tm context = match context with
  (x,tm',_)::t -> substcontext(subst(x)(tm')(tm))(t)
  | [] -> tm;;

let rec getvarcontext x context = List.assoc(x)(context);;


let rec removeFromContext (x',ty') context context2 = match context with
   (x,ty,tt)::tail -> if  x = x' then removeFromContext(x',ty')(tail)(context2) else removeFromContext(x',ty')(tail)(context2@[(x,ty,tt)])
   | [] -> context2;;

let rec eval tm vctx debug =
  (*let tm'' = subst("x")(TmVar("5"))(tm) in*)
  let tm'' = substcontext(tm)(vctx) in
  try match eval1 tm'' vctx with
    TmEq(x,t) -> (t,[(x,t,typeof [] t)]@removeFromContext((x,t))(vctx)([]))
    | tm' ->
    if debug then
      print_endline ("DEBUG-> "^string_of_term (tm'));
    eval(tm')(vctx)(debug);
  with
    NoRuleApplies ->(tm'',vctx)
;;

