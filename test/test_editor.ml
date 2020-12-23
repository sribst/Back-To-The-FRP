open Editor

let text_t =
  let module M = struct
    type t = string * string

    let equal (l, r) (l', r') = l = l' && r = r'

    let pp fmt (l, r) = Format.fprintf fmt "@[@<20>%s|@<20>%s@]" l r
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let basic_refine_list =
  [(0, Insert "12345"); (1, Left 3); (2, Del 2); (3, Nothing); (4, Right 4)]

let basic_time_list = [0; 1; 2; 3; 4]

let basic_expected_list =
  [("12345", ""); ("12", "345"); ("", "345"); ("", "345"); ("345", "")]

let observe_2_refine_list =
  [ (0, Insert "12345");
    (0, Insert "ABCDEFGHIJ");
    (1, Left 2);
    (1, Left 4);
    (2, Del 2);
    (2, Del 4);
    (3, Nothing);
    (3, Nothing);
    (4, Right 2);
    (4, Right 4) ]

let observe_2_time_list = [0; 0; 1; 1; 2; 2; 3; 3; 4; 4]

let observe_2_expected_list =
  [ ("12345", "");
    ("ABCDEFGHIJ", "");
    ("ABCDEFGH", "IJ");
    ("ABCDEF", "GHIJ");
    ("ABCD", "GHIJ");
    ("AB", "GHIJ");
    ("AB", "GHIJ");
    ("AB", "GHIJ");
    ("ABGH", "IJ");
    ("ABGHIJ", "") ]

let not_inline_refine_list =
  [ (0, Insert "ABCD");
    (6, Insert "1234");
    (2, Left 2);
    (8, Right 5);
    (4, Del 1) ]

let not_inline_time_order_list = [1; 3; 5; 7; 9]

let not_inline_expected_order_continue_list =
  [("ABCD", ""); ("ABCD", ""); ("AB", "CD"); ("AB1234", "CD"); ("A1234CD", "")]

let not_inline_expected_order_then_list =
  [("ABCD", ""); ("AB", "CD"); ("A", "CD"); ("A1234", "CD"); ("A1234CD", "")]

let not_inline_time_same_order_list = [1; 7; 3; 9; 5]

let not_inline_expected_same_order_continue_list =
  [("ABCD", ""); ("ABCD1234", ""); ("AB", "CD"); ("AB1234CD", ""); ("A", "CD")]

let not_inline_expected_same_order_then_list =
  [("ABCD", ""); ("A1234", "CD"); ("AB", "CD"); ("A1234CD", ""); ("A", "CD")]

let not_inline_time_no_order_list = [7; 3; 9; 1; 5]

let not_inline_expected_no_order_continue_list =
  [ ("ABCD", "");
    (* 7 *)
    ("ABCD", "");
    (* 3 *)
    ("AB1234", "CD");
    (* 9 *)
    ("ABCD", "");
    (* 1 *)
    ("A", "CD")
    (* 5 *) ]

let not_inline_expected_no_order_then_list =
  [ ("A1234", "CD");
    (* 7 *)
    ("AB", "CD");
    (* 3 *)
    ("A1234CD", "");
    (* 9 *)
    ("ABCD", "");
    (* 1 *)
    ("A", "CD")
    (* 5 *) ]

let refine_fct t v = Bttfrp.S.refine user (Bttfrp.T.of_int t) v

let observe_fct t = Bttfrp.S.observe text (Bttfrp.T.of_int t)

let c n rl tl el =
  empty_all () ;
  Test.run_cont
    ~and_exit:false
    n
    (fun (l, r) -> " " ^ l ^ "|" ^ r ^ " ")
    "same text"
    text_t
    refine_fct
    rl
    observe_fct
    tl
    el

let t n rl tl el =
  empty_all () ;
  Test.run_then
    ~and_exit:false
    n
    (fun (l, r) -> " " ^ l ^ "|" ^ r ^ " ")
    "same text"
    text_t
    refine_fct
    rl
    observe_fct
    tl
    el

let () =
  c
    "observe 2"
    observe_2_refine_list
    observe_2_time_list
    observe_2_expected_list ;
  c "basic" basic_refine_list basic_time_list basic_expected_list ;
  t "basic" basic_refine_list basic_time_list basic_expected_list ;
  c
    "not inline time order"
    not_inline_refine_list
    not_inline_time_order_list
    not_inline_expected_order_continue_list ;
  t
    "not inline time order"
    not_inline_refine_list
    not_inline_time_order_list
    not_inline_expected_order_then_list ;
  c
    "not inline time same order"
    not_inline_refine_list
    not_inline_time_same_order_list
    not_inline_expected_same_order_continue_list ;
  t
    "not inline time same order"
    not_inline_refine_list
    not_inline_time_same_order_list
    not_inline_expected_same_order_then_list ;
  c
    "not inline time no order"
    not_inline_refine_list
    not_inline_time_no_order_list
    not_inline_expected_no_order_continue_list ;
  t
    "not inline time no order"
    not_inline_refine_list
    not_inline_time_no_order_list
    not_inline_expected_no_order_then_list
