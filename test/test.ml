let refine_and_observe ~refine ~observe ~refine_list ~time_list =
  List.map2
    (fun (time, occ) observed_time -> refine time occ ; observe observed_time)
    refine_list
    time_list

let refine_then_observe ~refine ~observe ~refine_list ~time_list =
  List.iter (fun (time, occ) -> refine time occ) refine_list ;
  List.map observe time_list

let test_observed_list ~case_name ~given_list ~expected_list ~occ_encoding =
  Alcotest.test_case case_name `Quick (fun () ->
      Alcotest.((check (list occ_encoding)) case_name expected_list given_list))

let refine_and_observe_list ~name ~occ_encoding ~refine ~refine_list ~observe
    ~time_list ~expected_list =
  let given_list =
    refine_and_observe ~refine ~observe ~refine_list ~time_list
  in
  test_observed_list
    ~case_name:(name ^ " refine and observe")
    ~given_list
    ~expected_list
    ~occ_encoding

let refine_then_observe_list ~name ~occ_encoding ~refine ~refine_list ~observe
    ~time_list ~expected_list =
  let given_list =
    refine_then_observe ~refine ~observe ~refine_list ~time_list
  in
  test_observed_list
    ~case_name:(name ^ " refine then observe")
    ~given_list
    ~expected_list
    ~occ_encoding

let test ~test_fct ~name ~occ_encoding ~refine ~refine_list ~observe ~time_list
    ~expected_list =
  test_fct
    ~name
    ~occ_encoding
    ~refine
    ~refine_list
    ~observe
    ~time_list
    ~expected_list

let test_refine_and_observe ~name ~occ_encoding ~refine ~refine_list ~observe
    ~time_list ~expected_list =
  test
    ~test_fct:refine_and_observe_list
    ~name
    ~occ_encoding
    ~refine
    ~refine_list
    ~observe
    ~time_list
    ~expected_list

let test_refine_then_observe ~name ~occ_encoding ~refine ~refine_list ~observe
    ~time_list ~expected_list =
  test
    ~test_fct:refine_then_observe_list
    ~name
    ~occ_encoding
    ~refine
    ~refine_list
    ~observe
    ~time_list
    ~expected_list
