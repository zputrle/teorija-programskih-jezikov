module S = Syntax

let rec eval_exp = function
  | S.Var x -> failwith "Expected a closed term"
  | S.Int _ | S.Bool _ | S.Lambda _ | S.RecLambda _ | S.Nil | S.Pair _ | S.Cons _ as e -> e
  | S.Plus (e1, e2) ->
      let n1 = eval_int e1
      and n2 = eval_int e2
      in S.Int (n1 + n2)
  | S.Minus (e1, e2) ->
      let n1 = eval_int e1
      and n2 = eval_int e2
      in S.Int (n1 - n2)
  | S.Times (e1, e2) ->
      let n1 = eval_int e1
      and n2 = eval_int e2
      in S.Int (n1 * n2)
  | S.Equal (e1, e2) ->
      let n1 = eval_int e1
      and n2 = eval_int e2
      in S.Bool (n1 = n2)
  | S.Less (e1, e2) ->
      let n1 = eval_int e1
      and n2 = eval_int e2
      in S.Bool (n1 < n2)
  | S.Greater (e1, e2) ->
      let n1 = eval_int e1
      and n2 = eval_int e2
      in S.Bool (n1 > n2)
  | S.IfThenElse (e, e1, e2) ->
      begin match eval_exp e with
      | S.Bool true -> eval_exp e1
      | S.Bool false -> eval_exp e2
      | _ -> failwith "Boolean expected"
      end
  | S.Apply (e1, e2) ->
      let f = eval_exp e1
      in
      begin match f with
      | S.Lambda (x, e) -> eval_exp (S.subst [(x, e2)] e)
      | S.RecLambda (f, x, e) as rec_f -> eval_exp (S.subst [(f, rec_f); (x, e2)] e)
      | _ -> failwith "Function expected"
      end
  | S.Match (e, e1, x, xs, e2) ->
    let v = eval_exp e
    in
      begin match v with
      | S.Nil -> eval_exp e1
      | S.Cons (v1 ,v2) -> eval_exp (S.subst [(x, v1); (xs, v2)] e2)
      | _ -> failwith (
              "Evaluation 'match' expression. " ^
              "Expected Cons or Nil but received: " ^ Syntax.string_of_exp v)
      end
  | S.Fst(Pair(e1, e2)) -> e1
  | S.Fst e -> S.Fst (eval_exp e)
  | S.Snd(Pair(e1, e2)) -> e2
  | S.Snd e -> S.Snd (eval_exp e)
and eval_int e =
  match eval_exp e with
  | S.Int n -> n
  | _ -> failwith "Integer expected"

let rec is_value ex = 
  match ex with
  | S.Int _ | S.Bool _ | S.Lambda _ | S.RecLambda _ -> true
  | S.Nil -> true
  | S.Cons (e1, e2) -> true
  | S.Pair (e1, e2) -> true
  | S.Var _ | S.Plus _ | S.Minus _ | S.Times _ | S.Equal _ | S.Less _ | S.Greater _
  | S.IfThenElse _ | S.Apply _ | S.Match _| S.Fst _ | S.Snd _ -> false

let rec step = function
  | S.Var _
  | S.Int _
  | S.Bool _
  | S.Lambda _
  | S.RecLambda _
  | S.Nil
  | S.Cons _
  | S.Pair _
    -> failwith "Expected a non-terminal expression"
  | S.Plus (S.Int n1, S.Int n2) -> S.Int (n1 + n2)
  | S.Plus (S.Int n1, e2) -> S.Plus (S.Int n1, step e2)
  | S.Plus (e1, e2) -> S.Plus (step e1, e2)
  | S.Minus (S.Int n1, S.Int n2) -> S.Int (n1 - n2)
  | S.Minus (S.Int n1, e2) -> S.Minus (S.Int n1, step e2)
  | S.Minus (e1, e2) -> S.Minus (step e1, e2)
  | S.Times (S.Int n1, S.Int n2) -> S.Int (n1 * n2)
  | S.Times (S.Int n1, e2) -> S.Times (S.Int n1, step e2)
  | S.Times (e1, e2) -> S.Times (step e1, e2)
  | S.Equal (S.Int n1, S.Int n2) -> S.Bool (n1 = n2)
  | S.Equal (S.Int n1, e2) -> S.Equal (S.Int n1, step e2)
  | S.Equal (e1, e2) -> S.Equal (step e1, e2)
  | S.Less (S.Int n1, S.Int n2) -> S.Bool (n1 < n2)
  | S.Less (S.Int n1, e2) -> S.Less (S.Int n1, step e2)
  | S.Less (e1, e2) -> S.Less (step e1, e2)
  | S.Greater (S.Int n1, S.Int n2) -> S.Bool (n1 > n2)
  | S.Greater (S.Int n1, e2) -> S.Greater (S.Int n1, step e2)
  | S.Greater (e1, e2) -> S.Greater (step e1, e2)
  | S.IfThenElse (S.Bool b, e1, e2) -> if b then e1 else e2
  | S.IfThenElse (e, e1, e2) -> S.IfThenElse (step e, e1, e2)
  | S.Apply (S.Lambda (x, e), e1) -> S.subst [(x, e1)] e
  | S.Apply (S.RecLambda (f, x, e) as rec_f, e1) -> S.subst [(f, rec_f); (x, e1)] e
  | S.Apply (e1, e2) -> S.Apply (step e1, e2)
  | S.Match (S.Nil, e1, x, xs, e2) -> e1
  | S.Match (S.Cons(e', e''), e1, x, xs, e2) -> (S.subst [(x, e'); (xs, e'')] e2)
  | S.Match (e, e1, x, xs, e2) -> S.Match (step e, e1, x, xs, e2)
  | S.Snd Pair (e1, e2) -> e2
  | S.Snd e -> S.Snd (step e)
  | S.Fst Pair (e1, e2) -> e1 
  | S.Fst e -> S.Fst (step e)

let big_step e =
  let v = eval_exp e in
  print_endline (S.string_of_exp v)

let rec small_step e =
  print_endline (S.string_of_exp e);
  if not (is_value e) then
    (print_endline "  ~>";
    small_step (step e))

(* Same as 'big_step' except that the evaluated expression is returned. *)
let r_big_step e =
  let v = eval_exp e in
    print_endline (S.string_of_exp v);
    v

(* Same as function 'small_step' except that the evaluated expression is returned. *)
let rec r_small_step e =
  print_endline (S.string_of_exp e);
  if is_value e then
    e
  else
    (print_endline "  ~>";
    r_small_step (step e))

