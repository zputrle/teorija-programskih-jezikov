module U = Utils;;

U.ensure_equal_only_with_lazy_eval "(FUN x -> x * x) (2 + 1)" "9";;

U.ensure_equal_only_with_lazy_eval
  "MATCH (1 + 2) :: [] WITH
  | [] -> []
  | x :: xs -> x"
  "3";;

let
  fib_fun_text = 
    "LET REC fib n =
      IF n = 0 THEN 0
      ELSE IF n = 1 THEN 1
      ELSE fib (n - 1) + fib (n - 2)
    IN "
in

  U.ensure_equal_only_with_lazy_eval
    (fib_fun_text ^
    "(FUN x -> FALSE) (fib 1000)")
    "FALSE";

  U.ensure_equal_only_with_lazy_eval
    (fib_fun_text ^
    "(FUN x -> ((x * x) + x)) (fib 10)")
    "3080";

  U.ensure_equal_only_with_eager_eval
    (fib_fun_text ^
    "(FUN x -> ((x * x) + x)) (fib 10)")
    "3080";

  U.ensure_equal_only_with_eager_eval
    (fib_fun_text ^
    "LET REC n_fib_nums n =
      IF n = 0 THEN
        []
      ELSE
        ((fib n) :: ((n_fib_nums) (n -1)))
    IN
      (n_fib_nums 5)")
    "5 :: 3 :: 2 :: 1 :: 1 :: []";

  U.ensure_equal_only_with_lazy_eval
    "{1 + 1, {1, 2 + 2}}"
    "{1 + 1, {1, 2 + 2}}";