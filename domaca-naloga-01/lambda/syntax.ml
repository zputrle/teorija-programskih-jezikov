type ident = string

type exp =
  | Var of ident
  | Int of int
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | Bool of bool
  | Equal of exp * exp
  | Less of exp * exp
  | Greater of exp * exp
  | IfThenElse of exp * exp * exp
  | Lambda of ident * exp
  | RecLambda of ident * ident * exp
  | Apply of exp * exp
  | Pair of exp * exp
  | Fst of exp
  | Snd of exp
  | Nil
  | Cons of exp * exp
  | Match of exp * exp * ident * ident * exp

let let_in (x, e1, e2) = Apply (Lambda (x, e2), e1)
let let_rec_in (f, x, e1, e2) = let_in (f, RecLambda (f, x, e1), e2)

(* Previous substitution method. *)
(* let rec subst sbst = function
  | Var x as e ->
      begin match List.assoc_opt x sbst with
      | None -> e
      | Some e' -> e'
      end
  | Int _ | Bool _ as e -> e
  | Plus (e1, e2) -> Plus (subst sbst e1, subst sbst e2)
  | Minus (e1, e2) -> Minus (subst sbst e1, subst sbst e2)
  | Times (e1, e2) -> Times (subst sbst e1, subst sbst e2)
  | Equal (e1, e2) -> Equal (subst sbst e1, subst sbst e2)
  | Less (e1, e2) -> Less (subst sbst e1, subst sbst e2)
  | Greater (e1, e2) -> Greater (subst sbst e1, subst sbst e2)
  | IfThenElse (e, e1, e2) -> IfThenElse (subst sbst e, subst sbst e1, subst sbst e2)
  | Lambda (x, e) ->
      let sbst' = List.remove_assoc x sbst in
      Lambda (x, subst sbst' e)
  | RecLambda (f, x, e) ->
      let sbst' = List.remove_assoc f (List.remove_assoc x sbst) in
      RecLambda (f, x, subst sbst' e)
  | Apply (e1, e2) -> Apply (subst sbst e1, subst sbst e2)
  | Nil -> Nil
  | Cons (e1, e2) -> Cons (subst sbst e1, subst sbst e2)
  | Match (e, e1, x, xs, e2) ->
    Match (
      subst sbst e,
      subst sbst e1,
      x, xs,
      let
        sbst' = List.remove_assoc x (List.remove_assoc xs sbst)
      in
        subst sbst' e2)
  | Pair (e1, e2) -> Pair (subst sbst e1, subst sbst e2)
  | Fst e1 -> Fst (subst sbst e1) 
  | Snd e1 -> Snd (subst sbst e1)  *)

let assgin_new_names l = List.map (fun x -> (x, Var ("__" ^ x))) l

(* Substitute the variables in the expression.

 Only variables that are free in a given context (or any underlying contexts) will be substitued.

 @param sbst a lsit of (var, exp) pairs for which each 'var' will be substitued with 'exp' in the expression
 @param bound a list of variables that are bound
 @param e an expression in which the variables will be substitued

 @returns the expression with substitued variables.
*)
let rec _subst sbst bound = function
  | Var x as e ->
        begin match List.assoc_opt x sbst with
        | None -> e
        (* Rename the variables bound in a give contect. *)
        | Some e' -> _subst (assgin_new_names bound) bound e'
        end
  | Int _ | Bool _ as e -> e
  | Plus (e1, e2) -> Plus (_subst sbst bound e1, _subst sbst bound e2)
  | Minus (e1, e2) -> Minus (_subst sbst bound e1, _subst sbst bound e2)
  | Times (e1, e2) -> Times (_subst sbst bound e1, _subst sbst bound e2)
  | Equal (e1, e2) -> Equal (_subst sbst bound e1, _subst sbst bound e2)
  | Less (e1, e2) -> Less (_subst sbst bound e1, _subst sbst bound e2)
  | Greater (e1, e2) -> Greater (_subst sbst bound e1, _subst sbst bound e2)
  | IfThenElse (e, e1, e2) -> IfThenElse (_subst sbst bound e, _subst sbst bound e1, _subst sbst bound e2)
  | Lambda (x, e) ->
      let sbst' = List.remove_assoc x sbst
      and bound' = x::bound
      in
      Lambda (x, _subst sbst' bound' e)
  | RecLambda (f, x, e) ->
      let sbst' = List.remove_assoc f (List.remove_assoc x sbst)
      and bound' = f::x::bound
      in
      RecLambda (f, x, _subst sbst' bound' e)
  | Apply (e1, e2) -> Apply (_subst sbst bound e1, _subst sbst bound e2)
  | Nil -> Nil
  | Cons (e1, e2) -> Cons (_subst sbst bound e1, _subst sbst bound e2)
  | Match (e, e1, x, xs, e2) ->
    Match (
      _subst sbst bound e,
      _subst sbst bound e1,
      x, xs,
      let sbst' = List.remove_assoc x (List.remove_assoc xs sbst)
      and bound' = x::xs::bound
      in
        _subst sbst' bound' e2)
  | Pair (e1, e2) -> Pair (_subst sbst bound e1, _subst sbst bound e2)
  | Fst e1 -> Fst (_subst sbst bound e1) 
  | Snd e1 -> Snd (_subst sbst bound e1)

(* Fixed substitution metod. *)
let rec subst sbst = _subst sbst []

let rec string_of_exp3 = function
  | IfThenElse (e, e1, e2) ->
      "IF " ^ string_of_exp2 e ^ " THEN " ^ string_of_exp2 e1 ^ " ELSE " ^ string_of_exp3 e2
  | Lambda (x, e) ->
      "FUN " ^ x ^ " -> " ^ string_of_exp3 e
  | RecLambda (f, x, e) ->
      "REC " ^ f ^ " " ^ x ^ " -> " ^ string_of_exp3 e
  | Match (e, e1, x, xs, e2) ->
      "MATCH " ^ string_of_exp2 e ^ " WITH | [] -> " ^ string_of_exp2 e1 ^ " | " ^ x ^ "::" ^ xs ^ " -> " ^ string_of_exp3 e2
  | e -> string_of_exp2 e
and string_of_exp2 = function
  | Equal (e1, e2) ->
    string_of_exp1 e1 ^ " = " ^ string_of_exp1 e2
  | Less (e1, e2) ->
    string_of_exp1 e1 ^ " < " ^ string_of_exp1 e2
  | Greater (e1, e2) ->
    string_of_exp1 e1 ^ " > " ^ string_of_exp1 e2
  | Plus (e1, e2) ->
    string_of_exp1 e1 ^ " + " ^ string_of_exp1 e2
  | Minus (e1, e2) ->
    string_of_exp1 e1 ^ " - " ^ string_of_exp1 e2
  | Cons (e1, e2) ->
    string_of_exp1 e1 ^ " :: " ^ string_of_exp2 e2
  | Times (e1, e2) ->
    string_of_exp1 e1 ^ " * " ^ string_of_exp1 e2
  | e -> string_of_exp1 e
and string_of_exp1 = function
  | Apply (e1, e2) ->
    string_of_exp0 e1 ^ " " ^ string_of_exp0 e2
  | Fst e -> "FST " ^ string_of_exp0 e
  | Snd e -> "SND " ^ string_of_exp0 e
  | e -> string_of_exp0 e
and string_of_exp0 = function
  | Int n -> string_of_int n
  | Bool b -> if b then "TRUE" else "FALSE"
  | Var x -> x
  | Nil -> "[]"
  | Pair (e1, e2) -> "{" ^ string_of_exp2 e1 ^ ", " ^ string_of_exp2 e2 ^ "}"
  | e -> "(" ^ string_of_exp3 e ^ ")"

let string_of_exp = string_of_exp3