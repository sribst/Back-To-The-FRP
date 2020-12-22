exception Not_same_size

module To_test = struct
  let continue refine_fct refine_list observe_fct time_list =
    let aux (time, value) time' = refine_fct time value ; observe_fct time' in
    List.map2 aux refine_list time_list

  let then_t refine_fct refine_list observe_fct time_list =
    List.iter (fun (t, v) -> refine_fct t v) refine_list ;
    List.map observe_fct time_list

  let timer_continue refine_fct refine_list observe_fct time_list =
    let aux (time, value) time' = refine_fct time value ; observe_fct time' in
    List.map2 aux refine_list time_list

  let timer_then refine_fct refine_list observe_fct time_list =
    List.iter (fun (t, v) -> refine_fct t v) refine_list ;
    let aux time = observe_fct time in
    List.map aux time_list
end

let rec test_list ol el ppv tv en =
  match (ol, el) with
  | ([], []) ->
      []
  | (h :: t, h' :: t') ->
      (ppv h h', `Slow, fun () -> Alcotest.(check tv) en h h')
      :: test_list t t' ppv tv en
  | _ ->
      raise Not_same_size

let continue_list ppv en tv rf rl o_f tl el =
  let ppvs o e = "observe : " ^ ppv o ^ " expected : " ^ ppv e in
  let ol = To_test.continue rf rl o_f tl in
  test_list ol el ppvs tv en

let then_list ppv en tv rf rl o_f tl el =
  let ppvs o e = "observe : " ^ ppv o ^ " expected : " ^ ppv e in
  let ol = To_test.then_t rf rl o_f tl in
  test_list ol el ppvs tv en

let timer_continue_list ppv en tv rf rl o_f tl el =
  let ppvs (o1, o2) (e1, e2) =
    ppv e1 ^ " <= " ^ ppv o1 ^ " && " ^ ppv o2 ^ " <= " ^ ppv e2
  in
  let ol = To_test.timer_continue rf rl o_f tl in
  test_list ol el ppvs tv en

let timer_then_list ppv en tv rf rl o_f tl el =
  let ppvs (o1, o2) (e1, e2) =
    ppv e1 ^ " <= " ^ ppv o1 ^ " && " ^ ppv o2 ^ " <= " ^ ppv e2
  in
  let ol = To_test.timer_then rf rl o_f tl in
  test_list ol el ppvs tv en

let run and_exit fl n ppv en tv rf rl o_f tl el =
  try Alcotest.run ~and_exit n [(n, fl ppv en tv rf rl o_f tl el)]
  with Not_same_size ->
    Printf.printf "expected list and observed_list are not of the same size"

let run_cont ?(and_exit = true) n = run and_exit continue_list (n ^ " continue")

let run_then ?(and_exit = true) n = run and_exit then_list (n ^ " then")

let run_timer_cont ?(and_exit = true) n =
  run and_exit timer_continue_list (n ^ " timer continue") string_of_float

let run_timer_then ?(and_exit = true) n =
  run and_exit timer_then_list (n ^ " timer then") string_of_float
