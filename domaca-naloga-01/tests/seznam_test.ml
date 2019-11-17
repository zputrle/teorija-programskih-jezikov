exception TestFaild of string

let parse_and_run e_str = 
  let e = Parser.parse e_str in
      print_endline "MALI KORAKI:";
      Eval.small_step e;;

let ensure_equal e_str expected_result =
  print_endline ("> Running test " ^ e_str ^ " ...");
  let
    res = (Syntax.string_of_exp (parse_and_run e_str))
  in
    if String.equal res expected_result
    then
      (print_endline ("> OK: " ^ e_str);
      print_endline "";)
    else
      raise (TestFaild ("Failed: in_expr = '" ^ e_str ^ "': result '" ^ res ^ "' != '" ^ expected_result ^ "'"));;


(* Tests *)

ensure_equal "[]" "[]";;
ensure_equal "10 :: []" "10 :: []";;
ensure_equal "10 :: 20 :: []" "10 :: 20 :: []";;
ensure_equal "10 :: (20 + 5) :: []" "10 :: 25 :: []";;
ensure_equal "((FUN x -> x * x) 10) :: 10 :: []" "100 :: 10 :: []";;

ensure_equal
  "MATCH [] WITH
  | [] -> FALSE
  | x :: xs -> TRUE"
  "FALSE";;
ensure_equal
  "MATCH 10 :: [] WITH
  | [] -> FALSE
  | x :: xs -> TRUE"
  "TRUE";;
ensure_equal
  "MATCH (10 + 5) :: [] WITH
  | [] -> FALSE
  | x :: xs -> TRUE"
  "TRUE";;
ensure_equal
  "MATCH 10 :: [] WITH
  | [] -> FALSE
  | x :: xs -> x"
  "10";;
ensure_equal
  "LET
    REC sum ls =
      MATCH ls WITH
      | [] -> 0
      | x :: xs -> x + (sum xs)
  IN
    sum (1 :: 2 :: 4 :: 5 :: 100 :: [])"
  "112";;

ensure_equal
  "MATCH 10 :: [] WITH
  | [] -> FALSE
  | x :: xs -> 5 :: x :: xs"
  "5 :: 10 :: []";;


ensure_equal "(FUN x -> (10 :: (20 + x) :: [])) 5" "10 :: 25 :: []";;
