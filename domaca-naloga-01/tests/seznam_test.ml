module U = Utils;;

(* Tests *)

U.ensure_equal "[]" "[]";;
U.ensure_equal "10 :: []" "10 :: []";;
U.ensure_equal "10 :: 20 :: []" "10 :: 20 :: []";;
U.ensure_equal "10 :: (20 + 5) :: []" "10 :: 25 :: []";;
U.ensure_equal "((FUN x -> x * x) 10) :: 10 :: []" "100 :: 10 :: []";;

U.ensure_equal
  "MATCH [] WITH
  | [] -> FALSE
  | x :: xs -> TRUE"
  "FALSE";;
U.ensure_equal
  "MATCH 10 :: [] WITH
  | [] -> FALSE
  | x :: xs -> TRUE"
  "TRUE";;
U.ensure_equal
  "MATCH (10 + 5) :: [] WITH
  | [] -> FALSE
  | x :: xs -> TRUE"
  "TRUE";;
U.ensure_equal
  "MATCH 1 :: 2 :: [] WITH
  | [] -> []
  | x :: xs -> xs"
  "2 :: []";;
U.ensure_equal
  "LET
    REC sum ls =
      MATCH ls WITH
      | [] -> 0
      | x :: xs -> x + (sum xs)
  IN
    sum (1 :: 2 :: 4 :: 5 :: 100 :: [])"
  "112";;

U.ensure_equal
  "MATCH 10 :: [] WITH
  | [] -> FALSE
  | x :: xs -> 5 :: x :: xs"
  "5 :: 10 :: []";;


U.ensure_equal "(FUN x -> (10 :: (20 + x) :: [])) 5" "10 :: 25 :: []";;
