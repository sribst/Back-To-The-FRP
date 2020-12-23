open Bttfrp

let empty refined_event = Event.empty refined_event

let print_time l =
  (List.fold_left
     (fun acc (t, b) ->
       " (" ^ string_of_int t ^ "," ^ string_of_bool b ^ ") " ^ acc)
     "")
    l

let refine refined_event t o = Event.refine refined_event (Time.of_int t) o

let observe refined_event name t =
  try Event.observe refined_event false (Time.of_int t)
  with Not_found -> Alcotest.failf "%s Not_found %d\n" name t

let check_refine_and_observe ~refined_event ~observed_name ~observed_event
    ~refine ~time_occ_list ~observe ~tested_time_list ~expected_list
    ~expected_interval_list =
  [ Test.test_refine_and_observe
      ~name:"occurence"
      ~occ_encoding:Alcotest.int
      ~refine:(refine refined_event)
      ~refine_list:time_occ_list
      ~observe:(observe observed_event observed_name)
      ~time_list:tested_time_list
      ~expected_list;
    Test.test_refine_and_observe
      ~name:"valid interval"
      ~occ_encoding:Alcotest.(list (pair int bool))
      ~refine:(refine refined_event)
      ~observe:(fun t ->
        ignore (observe observed_event observed_name t) ;
        List.rev (Event.get_interval_list observed_event))
      ~refine_list:time_occ_list
      ~time_list:tested_time_list
      ~expected_list:expected_interval_list ]

let check_refine_then_observe ~refined_event ~observed_name ~observed_event
    ~refine ~time_occ_list ~observe ~tested_time_list ~expected_list
    ~expected_interval_list =
  [ Test.test_refine_then_observe
      ~name:"occurence"
      ~occ_encoding:Alcotest.int
      ~refine:(refine refined_event)
      ~refine_list:time_occ_list
      ~observe:(observe observed_event observed_name)
      ~time_list:tested_time_list
      ~expected_list;
    Test.test_refine_then_observe
      ~name:"valid interval"
      ~occ_encoding:Alcotest.(list (pair int bool))
      ~refine:(refine refined_event)
      ~observe:(fun t ->
        ignore (observe observed_event observed_name t) ;
        List.rev (Event.get_interval_list observed_event))
      ~refine_list:time_occ_list
      ~time_list:tested_time_list
      ~expected_list:expected_interval_list ]

let check_both_test_case ~create_event ~observed_name ~refine ~time_occ_list
    ~observe ~tested_time_list ~expected_list ~expected_interval_list =
  (let (refined_event, observed_event) = create_event () in
   check_refine_and_observe
     ~refined_event
     ~observed_name
     ~observed_event
     ~refine
     ~time_occ_list
     ~observe
     ~tested_time_list
     ~expected_list
     ~expected_interval_list)
  @
  let (refined_event, observed_event) = create_event () in
  check_refine_then_observe
    ~refined_event
    ~observed_name
    ~observed_event
    ~refine
    ~time_occ_list
    ~observe
    ~tested_time_list
    ~expected_list
    ~expected_interval_list

let test_primitive_discrete () =
  let create_event () =
    let event = Event.Discrete.create () in
    (event, event)
  in
  let name = "primitive discrete event" in
  let time_occ_list = [(0, 0); (10, 10); (20, 20); (30, 30)] in
  let tested_time_list = [0; 10; 20; 30] in
  let expected_list = [0; 10; 20; 30] in
  let expected_interval_list = [[]; []; []; []] in
  check_both_test_case
    ~create_event
    ~observed_name:name
    ~refine
    ~time_occ_list
    ~observe
    ~tested_time_list
    ~expected_list
    ~expected_interval_list

let test_primitive_continuous () =
  let create_event () =
    let event = Event.Continuous.create () in
    (event, event)
  in
  let name = "primitive continuous event" in
  let time_occ_list = [(0, 0); (10, 10); (20, 20); (30, 30)] in
  let tested_time_list = [5; 15; 25; 35] in
  let expected_list = [0; 10; 20; 30] in
  let expected_interval_list = [[]; []; []; []] in
  check_both_test_case
    ~create_event
    ~observed_name:name
    ~refine
    ~time_occ_list
    ~observe
    ~tested_time_list
    ~expected_list
    ~expected_interval_list

let test_map_discrete () =
  let create_event () =
    let refined_event = Event.Discrete.create () in
    let observed_event = Event.Discrete.map (fun x -> x + 5) refined_event in
    (refined_event, observed_event)
  in
  let observed_name = "map discrete event" in
  let time_occ_list = [(0, 0); (10, 10); (20, 20); (30, 30)] in
  let tested_time_list = [0; 10; 20; 30] in
  let expected_list = [5; 15; 25; 35] in
  let expected_interval_list =
    [ [(0, true); (1, false)];
      [(0, true); (1, false); (10, true); (11, false)];
      [(0, true); (1, false); (10, true); (11, false); (20, true); (21, false)];
      [ (0, true);
        (1, false);
        (10, true);
        (11, false);
        (20, true);
        (21, false);
        (30, true);
        (31, false) ] ]
  in
  check_both_test_case
    ~create_event
    ~observed_name
    ~refine
    ~time_occ_list
    ~observe
    ~tested_time_list
    ~expected_list
    ~expected_interval_list

let test_map_continuous () =
  let create_event () =
    let refined_event = Event.Continuous.create () in
    let observed_event = Event.Continuous.map (fun x -> x + 5) refined_event in
    (refined_event, observed_event)
  in
  let observed_name = "map continous event" in
  let time_occ_list = [(0, 0); (10, 10); (20, 20); (30, 30)] in
  let tested_time_list = [5; 15; 25; 35] in
  let expected_list = [5; 15; 25; 35] in
  let expected_interval_list =
    [ [(0, true); (6, false)];
      [(0, true); (6, false); (10, true); (16, false)];
      [(0, true); (6, false); (10, true); (16, false); (20, true); (26, false)];
      [ (0, true);
        (6, false);
        (10, true);
        (16, false);
        (20, true);
        (26, false);
        (30, true);
        (36, false) ] ]
  in
  check_both_test_case
    ~create_event
    ~observed_name
    ~refine
    ~time_occ_list
    ~observe
    ~tested_time_list
    ~expected_list
    ~expected_interval_list

let test_complete () =
  let create_event () =
    let refined_event = Event.Discrete.create () in
    let observed_event = Event.Continuous.complete refined_event in
    (refined_event, observed_event)
  in
  let observed_name = "complete continuous event" in
  let time_occ_list = [(0, 0); (10, 10); (20, 20); (30, 30)] in
  let tested_time_list = [5; 15; 25; 35] in
  let expected_list = [0; 10; 20; 30] in
  let expected_interval_list =
    [ [(0, true); (6, false)];
      [(0, true); (6, false); (10, true); (16, false)];
      [(0, true); (6, false); (10, true); (16, false); (20, true); (26, false)];
      [ (0, true);
        (6, false);
        (10, true);
        (16, false);
        (20, true);
        (26, false);
        (30, true);
        (36, false) ] ]
  in
  check_both_test_case
    ~create_event
    ~observed_name
    ~refine
    ~time_occ_list
    ~observe
    ~tested_time_list
    ~expected_list
    ~expected_interval_list

let test_map2_continuous () =
  let create_event () =
    let refined_event = Event.Continuous.create () in
    let maped_event = Event.Continuous.map (fun x -> x * x) refined_event in
    let observed_event =
      Event.Continuous.map2 ( + ) refined_event maped_event
    in
    (refined_event, observed_event)
  in
  let observed_name = "map2 continuous event : x*x + x" in
  let time_occ_list = [(0, 0); (2, 2); (4, 4)] in
  let tested_time_list = [1; 3; 5] in
  let expected_list = [0; 6; 20] in
  let expected_interval_list =
    [[(0, true); (2, false)]; [(0, true); (4, false)]; [(0, true); (6, false)]]
  in
  check_both_test_case
    ~create_event
    ~observed_name
    ~refine
    ~time_occ_list
    ~observe
    ~tested_time_list
    ~expected_list
    ~expected_interval_list

let test_fix_continuous () =
  let create_event () =
    let refined_event = Event.Continuous.create () in
    let (observed_event, _) =
      Event.Continuous.fix (fun c ->
          let cf = Event.Continuous.map (fun x -> x + x) c in
          (cf, c))
    in
    (refined_event, observed_event)
  in
  let observed_name = "fix continuous event : x*x" in
  let time_occ_list = [(0, 0); (2, 2); (4, 4)] in
  let tested_time_list = [1; 3; 5] in
  let expected_list = [0; 4; 16] in
  let expected_interval_list =
    [[(0, true); (2, false)]; [(0, true); (4, false)]; [(0, true); (6, false)]]
  in
  check_both_test_case
    ~create_event
    ~observed_name
    ~refine
    ~time_occ_list
    ~observe
    ~tested_time_list
    ~expected_list
    ~expected_interval_list

let tests =
  [ ("discrete primitive", test_primitive_discrete ());
    ("continuous primitive", test_primitive_continuous ());
    ("discrete map", test_map_discrete ());
    ("continuous map", test_map_continuous ());
    ("continuous map2", test_map2_continuous ());
    ("continuous fix", test_fix_continuous ());
    ("continuous complete", test_complete ()) ]
