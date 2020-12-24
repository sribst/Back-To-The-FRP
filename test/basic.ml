module Time = Bttfrp.Time

let refine event time occ = Bttfrp.refine event (Time.of_int time) occ

let observe ~event_name event time =
  try Bttfrp.observe ~produce:false event (Time.of_int time)
  with Not_found ->
    Alcotest.failf "event %s failed to be observed at time %d" event_name time

let check_refine_and_observe ~refined_event ~event_name ~observed_event ~refine
    ~observe ~time_occ_list ~tested_time_list ~expected_list
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
    ~refine ~observe ~time_occ_list ~tested_time_list ~expected_list
    ~expected_interval_list =
  [ Test.test_refine_then_observe
      ~name:"occurence"
      ~occ_encoding:Alcotest.int
      ~refine_list:time_occ_list
      ~refine:(refine refined_event)
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

let check_both_test_case ?(refine = refine) ?(observe = observe) ~create_event
    ~time_occ_list ~event_name ~tested_time_list ~expected_list
    ~expected_interval_list () =
  let (refined_event, observed_event) = create_event () in
  check_refine_and_observe
    ~refine
    ~observe
    ~refined_event
    ~event_name
    ~observed_event
    ~time_occ_list
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
    ~time_occ_list
    ~tested_time_list
    ~expected_list
    ~expected_interval_list
    ()

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
    ~time_occ_list
    ~tested_time_list
    ~expected_list
    ~expected_interval_list
    ()

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
    ()

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
    ()

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
    ()

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
    ()

let test_fix_continuous () =
  let create_event () =
    let refined_event = Bttfrp.Continuous.create () in
    let (fibo, _) =
      Bttfrp.Continuous.fix ~fix_f:(fun event ->
          let previous_event = Bttfrp.Continuous.previous ~origin:1 event in
          let previous_event2 =
            Bttfrp.Continuous.previous ~origin:0 previous_event
          in
          let fibo_event =
            Bttfrp.Continuous.map2
              ~f:(fun x y -> x + y)
              previous_event
              previous_event2
          in
          (fibo_event, ()))
    in
    (refined_event, fibo)
  in
  let event_name = "fix continuous" in
  let time_occ_list =
    (* empty list needed because we need the refined_list to be the same size as expected_list *)
    [ (0, 0);
      (0, 0);
      (0, 0);
      (0, 0);
      (0, 0);
      (0, 0);
      (0, 0);
      (0, 0);
      (0, 0);
      (0, 0);
      (0, 0);
      (0, 0) ]
  in
  let tested_time_list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11] in
  let expected_list = [1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144; 233] in
  let expected_interval_list =
    [ [(0, false)];
      [(0, true); (1, false)];
      [(0, true); (2, false)];
      [(0, true); (3, false)];
      [(0, true); (4, false)];
      [(0, true); (5, false)];
      [(0, true); (6, false)];
      [(0, true); (7, false)];
      [(0, true); (8, false)];
      [(0, true); (9, false)];
      [(0, true); (10, false)];
      [(0, true); (11, false)] ]
  in
  check_both_test_case
    ~create_event
    ~event_name
    ~refine:(fun _event _time _occ -> ())
    ~time_occ_list
    ~observe
    ~tested_time_list
    ~expected_list
    ~expected_interval_list
    ()

let tests =
  [ ("discrete primitive", test_primitive_discrete ());
    ("continuous primitive", test_primitive_continuous ());
    ("discrete map", test_map_discrete ());
    ("continuous map", test_map_continuous ());
    ("continuous map2", test_map2_continuous ());
    ("continuous complete", test_complete ());
    ("continuous fix", test_fix_continuous ()) ]
