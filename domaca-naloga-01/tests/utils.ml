exception TestFaild of string

let run_lazy_eval e =
  print_endline "-- LAZY EVAL --";
  print_endline "MALI KORAKI:";
  let ss = LazyEval.r_small_step e in
  print_endline "VELIKI KORAKI:";
  let bs = LazyEval.r_big_step e 
  in
    let ss_s = (Syntax.string_of_exp ss)
    and bs_s = (Syntax.string_of_exp bs)
    in
      if String.equal ss_s bs_s then
        ss
      else
        raise (
          TestFaild (
              "Failed at lazy evaluation : " ^
                "in_expr = '" ^ (Syntax.string_of_exp e) ^ "': " ^
                  "ss_s = '" ^ ss_s ^ "' " ^
                  "bs_s '" ^ bs_s ^ "'"));;

let run_eager_eval e =
  print_endline "-- EAGER EVAL --";
  print_endline "MALI KORAKI:";
  let ss = Eval.r_small_step e in
  print_endline "VELIKI KORAKI:";
  let bs = Eval.r_big_step e 
  in
    let ss_s = (Syntax.string_of_exp ss)
    and bs_s = (Syntax.string_of_exp bs)
    in
      if String.equal ss_s bs_s then
        ss
      else
        raise (
          TestFaild (
              "Failed at eager evaluation : " ^
                "in_expr = '" ^ (Syntax.string_of_exp e) ^ "': " ^
                  "ss_s = '" ^ ss_s ^ "' " ^
                  "bs_s '" ^ bs_s ^ "'"));;

let parse_and_run e_str = 
  print_endline "Parsing ...";
  let e = Parser.parse e_str
  in
    print_endline "Done.";
    let ee = run_eager_eval e
    and le = run_lazy_eval e
    in
      let ee_s = (Syntax.string_of_exp ee)
      and le_s = (Syntax.string_of_exp le) in
        if String.equal ee_s le_s then
          ee
        else
          raise (
            TestFaild (
              "Failed at evaluation: " ^
                "in_expr = '" ^ e_str ^ "': " ^
                  "ee = '" ^ ee_s ^ "' " ^
                  "le '" ^ le_s ^ "'"));;

let ensure_equal e_str expected_result =
  let
    res = (Syntax.string_of_exp (parse_and_run e_str))
  in
    if String.equal res expected_result
    then
      (print_endline ("> OK: " ^ e_str);
      print_endline "";)
    else
      raise (TestFaild ("Failed: in_expr = '" ^ e_str ^ "': result '" ^ res ^ "' != '" ^ expected_result ^ "'"));;

let ensure_equal_only_with_eager_eval e_str expected_result =
  print_endline ("> Running test (with eager eval) " ^ e_str ^ " ...");
  print_endline "Parsing ...";
  let e = Parser.parse e_str
  in
    print_endline "Done.";
    let
      res = (Syntax.string_of_exp (run_eager_eval e))
    in
      if String.equal res expected_result
      then
        (print_endline ("> OK: " ^ e_str);
        print_endline "";)
      else
        raise (TestFaild ("Failed: in_expr = '" ^ e_str ^ "': result '" ^ res ^ "' != '" ^ expected_result ^ "'"));;

let ensure_equal_only_with_lazy_eval e_str expected_result =
  print_endline ("> Running test (with lazy eval) " ^ e_str ^ " ...");
  print_endline "Parsing ...";
  let e = Parser.parse e_str
  in
    print_endline "Done.";
    let
      res = (Syntax.string_of_exp (run_lazy_eval e))
    in
      if String.equal res expected_result
      then
        (print_endline ("> OK: " ^ e_str);
        print_endline "";)
      else
        raise (TestFaild ("Failed: in_expr = '" ^ e_str ^ "': result '" ^ res ^ "' != '" ^ expected_result ^ "'"));;

