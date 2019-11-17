exception TestFaild of string

let parse_and_run e_str = 
  let e = Parser.parse e_str in
      print_endline "VELIKI KORAKI:";
    let rs_vk = Eval.big_step e in
      print_endline "MALI KORAKI:";
    let rs_mk = Eval.small_step e in
      let rs_vk_s = (Syntax.string_of_exp rs_vk)
      and rs_mk_s = (Syntax.string_of_exp rs_mk) in
        if String.equal rs_mk_s rs_vk_s then rs_mk else
          raise (
            TestFaild (
              "Failed: in_expr = '" ^ e_str ^ "': " ^
                "rs_vk = '" ^ rs_vk_s ^ "' " ^
                "rs_mk '" ^ rs_mk_s ^ "'"));;

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