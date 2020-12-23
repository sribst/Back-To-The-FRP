module Time = Bttfrp.Time

let empty refined_event = Bttfrp.empty refined_event

let print_time l =
  (List.fold_left
     (fun acc (t, b) ->
       " (" ^ string_of_int t ^ "," ^ string_of_bool b ^ ") " ^ acc)
     "")
    l

let refine event time occ =
  Bttfrp.print_value event string_of_int "before refining event" ;
  Bttfrp.print_time event "before refining event" ;
  Bttfrp.refine event (Time.of_int time) occ ;
  Bttfrp.print_value event string_of_int "after refining event" ;
  Bttfrp.print_time event "after refining event"

let observe ~event_name event time =
  try
    Bttfrp.print_value event string_of_int "before observing event" ;
    Bttfrp.print_time event "before observing event" ;
    let occ = Bttfrp.observe ~produce:false event (Time.of_int time) in
    Bttfrp.print_value event string_of_int "after observing event" ;
    Bttfrp.print_time event "after observing event" ;
    occ
  with Not_found ->
    Alcotest.failf "event %s failed to be observed at time %d" event_name time

let check_refine_and_observe ~refined_event ~event_name ~observed_event ~refine
    ~time_occ_list ~observe ~tested_time_list ~expected_list
    ~expected_interval_list =
  [ Test.test_refine_and_observe
      ~name:"occurence"
      ~occ_encoding:Alcotest.int
      ~refine:(refine refined_event)
      ~refine_list:time_occ_list
      ~observe:(observe ~event_name observed_event)
      ~time_list:tested_time_list
      ~expected_list;
    Test.test_refine_and_observe
      ~name:"valid interval"
      ~occ_encoding:Alcotest.(list (pair int bool))
      ~refine:(refine refined_event)
      ~observe:(fun time ->
        ignore (observe ~event_name observed_event time) ;
        List.rev (Bttfrp.get_interval_list observed_event))
      ~refine_list:time_occ_list
      ~time_list:tested_time_list
      ~expected_list:expected_interval_list ]

let check_refine_then_observe ~refined_event ~event_name ~observed_event
    ~refine ~time_occ_list ~observe ~tested_time_list ~expected_list
    ~expected_interval_list =
  [ Test.test_refine_then_observe
      ~name:"occurence"
      ~occ_encoding:Alcotest.int
      ~refine:(refine refined_event)
      ~refine_list:time_occ_list
      ~observe:(observe ~event_name observed_event)
      ~time_list:tested_time_list
      ~expected_list;
    Test.test_refine_then_observe
      ~name:"valid interval"
      ~occ_encoding:Alcotest.(list (pair int bool))
      ~refine:(refine refined_event)
      ~observe:(fun time ->
        ignore (observe ~event_name observed_event time) ;
        List.rev (Bttfrp.get_interval_list observed_event))
      ~refine_list:time_occ_list
      ~time_list:tested_time_list
      ~expected_list:expected_interval_list ]

let check_both_test_case ~create_event ~refine ~time_occ_list ~event_name
    ~observe ~tested_time_list ~expected_list ~expected_interval_list =
  let (refined_event, observed_event) = create_event () in
  check_refine_and_observe
    ~refined_event
    ~event_name
    ~observed_event
    ~refine
    ~time_occ_list
    ~observe
    ~tested_time_list
    ~expected_list
    ~expected_interval_list
  @
  let (refined_event, observed_event) = create_event () in
  check_refine_then_observe
    ~refined_event
    ~event_name
    ~observed_event
    ~refine
    ~time_occ_list
    ~observe
    ~tested_time_list
    ~expected_list
    ~expected_interval_list

let test_primitive_discrete () =
  let create_event () =
    let event = Bttfrp.Discrete.create () in
    (event, event)
  in
  let name = "primitive discrete" in
  let time_occ_list = [(0, 0); (10, 10); (20, 20); (30, 30)] in
  let tested_time_list = [0; 10; 20; 30] in
  let expected_list = [0; 10; 20; 30] in
  let expected_interval_list = [[]; []; []; []] in
  check_both_test_case
    ~create_event
    ~event_name:name
    ~refine
    ~time_occ_list
    ~observe
    ~tested_time_list
    ~expected_list
    ~expected_interval_list

let test_primitive_continuous () =
  let create_event () =
    let event = Bttfrp.Continuous.create () in
    (event, event)
  in
  let name = "primitive continuous" in
  let time_occ_list = [(0, 0); (10, 10); (20, 20); (30, 30)] in
  let tested_time_list = [5; 15; 25; 35] in
  let expected_list = [0; 10; 20; 30] in
  let expected_interval_list = [[]; []; []; []] in
  check_both_test_case
    ~create_event
    ~event_name:name
    ~refine
    ~time_occ_list
    ~observe
    ~tested_time_list
    ~expected_list
    ~expected_interval_list

let test_map_discrete () =
  let create_event () =
    let refined_event = Bttfrp.Discrete.create () in
    let observed_event =
      Bttfrp.Discrete.map ~f:(fun x -> x + 5) refined_event
    in
    (refined_event, observed_event)
  in
  let event_name = "map discrete" in
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
    ~event_name
    ~refine
    ~time_occ_list
    ~observe
    ~tested_time_list
    ~expected_list
    ~expected_interval_list

let test_map_continuous () =
  let create_event () =
    let refined_event = Bttfrp.Continuous.create () in
    let observed_event =
      Bttfrp.Continuous.map ~f:(fun x -> x + 5) refined_event
    in
    (refined_event, observed_event)
  in
  let event_name = "map continous" in
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
    ~event_name
    ~refine
    ~time_occ_list
    ~observe
    ~tested_time_list
    ~expected_list
    ~expected_interval_list

let test_complete () =
  let create_event () =
    let refined_event = Bttfrp.Discrete.create () in
    let observed_event = Bttfrp.Continuous.complete refined_event in
    (refined_event, observed_event)
  in
  let event_name = "complete continuous" in
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
    ~event_name
    ~refine
    ~time_occ_list
    ~observe
    ~tested_time_list
    ~expected_list
    ~expected_interval_list

let test_map2_continuous () =
  let create_event () =
    let refined_event = Bttfrp.Continuous.create () in
    let maped_event =
      Bttfrp.Continuous.map ~f:(fun x -> x * x) refined_event
    in
    let observed_event =
      Bttfrp.Continuous.map2 ~f:( + ) refined_event maped_event
    in
    (refined_event, observed_event)
  in
  let event_name = "map2 continuous" in
  let time_occ_list = [(0, 0); (2, 2); (4, 4)] in
  let tested_time_list = [1; 3; 5] in
  let expected_list = [0; 6; 20] in
  let expected_interval_list =
    [[(0, true); (2, false)]; [(0, true); (4, false)]; [(0, true); (6, false)]]
  in
  check_both_test_case
    ~create_event
    ~event_name
    ~refine
    ~time_occ_list
    ~observe
    ~tested_time_list
    ~expected_list
    ~expected_interval_list

let test_fix_continuous () =
  let create_event () =
    let refined_event = Bttfrp.Continuous.create () in
    let (observed_event, _other_event) =
      Bttfrp.Continuous.fix ~fix_f:(fun event ->
          let maped_event = Bttfrp.Continuous.map ~f:(fun x -> x + x) event in
          (maped_event, event))
    in
    (refined_event, observed_event)
  in
  let event_name = "fix continuous" in
  let time_occ_list = [(0, 0); (2, 2); (4, 4)] in
  let tested_time_list = [1; 3; 5] in
  let expected_list = [0; 4; 16] in
  let expected_interval_list =
    [[(0, true); (2, false)]; [(0, true); (4, false)]; [(0, true); (6, false)]]
  in
  check_both_test_case
    ~create_event
    ~event_name
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
    ("continuous complete", test_complete ());
    ("continuous fix", test_fix_continuous ()) ]
