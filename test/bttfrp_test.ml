open Bttfrp

let empty s=
  S.empty s
	  
let print_time l =
  (List.fold_left (fun acc (t,b) ->
		   " ("  ^ string_of_int t
		   ^ "," ^ string_of_bool b
		   ^ ") " ^ acc) "" ) l 
				      
let rf s t o =
  S.refine s (T.of_int t) o

let obs s name t =
  try S.observe s (T.of_int t) with
    Not_found -> Printf.printf "%s Not_found %d\n" name t;
		 raise Not_found

let check_c n s n' s' rf_f rf_l obs_f time_l exp_l exp_interval_l =
  Test.run_cont
    ~and_exit:false
    (n ^ " val")
    (string_of_int)
    "same int"
    (Alcotest.int)
    (rf_f s)
    rf_l
    (obs_f s' n')
    time_l
    exp_l;
  S.empty s';
  Test.run_cont
    ~and_exit:false
    (n ^ " time")
    (print_time)
    "same valid interval"
    (Alcotest.list (Alcotest.pair Alcotest.int Alcotest.bool))
    (rf_f s)
    rf_l
    (fun t -> ignore(obs_f s' n' t); List.rev (S.get_vl_list s'))
    time_l
    exp_interval_l

let check_t n s n' s' rf_f rf_l obs_f time_l exp_l exp_interval_l = 
  Test.run_then
    ~and_exit:false
    (n ^ " val")
    (string_of_int)
    "same int"
    (Alcotest.int)
    (rf_f s)
    rf_l
    (obs_f s' n')
    time_l
    exp_l;
  S.empty s';
  Test.run_then
    ~and_exit:false
    (n ^ " time")
    (print_time)
    "same valid interval"
    (Alcotest.list (Alcotest.pair Alcotest.int Alcotest.bool))
    (rf_f s)
    rf_l
    (fun t -> ignore(obs_f s' n' t); List.rev (S.get_vl_list s'))
    time_l
    exp_interval_l


let bsc_d _ =
  let d = D.create () in
  let n = "basic discrete" in 
  let rf_l =
    [(0,0);(10,10);(20,20);(30,30)]
  in
  let time_l =
    [0;10;20;30]
  in
  let exp_l=
    [0;10;20;30]
  in
  let exp_interval_l =
    [[];
     [];
     [];
     [];
    ]
  in

  check_c n d n d rf rf_l obs time_l exp_l exp_interval_l;
  check_t n d n d rf rf_l obs time_l exp_l exp_interval_l
	  
let bsc_c _ =
  let c = C.create () in
  let n = "basic continuous" in
  let rf_l = 
    [(0,0);(10,10);(20,20);(30,30)]
  in
  let time_l =
    [5;15;25;35]
  in
  let exp_l=
    [0;10;20;30]
  in
  let exp_interval_l =
    [[];
     [];
     [];
     [];
    ]
  in

  check_c n c n c rf rf_l obs time_l exp_l exp_interval_l;
  check_t n c n c rf rf_l obs time_l exp_l exp_interval_l

	  
let map_d _ =
  let d = D.create ()  in
  let n = "dmap discrete"  in
  let m = D.map (fun x -> x + 5) d in
  let mn = "map discrete"  in
  let rf_l =
    [(0,0);(10,10);(20,20);(30,30)]
  in
  let time_l =
    [0;10;20;30]
  in
  let exp_l=
    [5;15;25;35]
  in
  let exp_interval_l =
    [
      [(0,true);(1,false)];
      [(0,true);(1,false);(10,true);(11,false)];
      [(0,true);(1,false);(10,true);(11,false);(20,true);(21,false)];
      [(0,true);(1,false);(10,true);(11,false);(20,true);(21,false);(30,true);(31,false)];
    ]
  in

  check_c n d mn m rf rf_l obs time_l exp_l exp_interval_l;
  check_t n d mn m rf rf_l obs time_l exp_l exp_interval_l

let map_c _ =
  let c = C.create () in
  let n = "cmap continous" in
  let m = C.map (fun x -> x+5) c in
  let mn = "map continous" in
  let rf_l = 
    [(0,0);(10,10);(20,20);(30,30)]
  in
  let time_l =
    [5;15;25;35]
  in
  let exp_l=
    [5;15;25;35]
  in
  let exp_interval_l =
    [
      [(0,true);(6,false)];
      [(0,true);(6,false);(10,true);(16,false)];
      [(0,true);(6,false);(10,true);(16,false);(20,true);(26,false)];
      [(0,true);(6,false);(10,true);(16,false);(20,true);(26,false);(30,true);(36,false)];
    ]
  in

  check_c n c mn m rf rf_l obs time_l exp_l exp_interval_l;
  check_t n c mn m rf rf_l obs time_l exp_l exp_interval_l
	  
let complete _ =
  let d   = D.create () in
  let n   = "complete" in
  let cd  = C.complete d in
  let cdn = "complete" in
  let rf_l = 
    [(0,0);(10,10);(20,20);(30,30)]
  in
  let time_l =
    [5;15;25;35]
  in
  let exp_l=
    [0;10;20;30]
  in
  let exp_interval_l =
    [
      [(0,true);(6,false)];
      [(0,true);(6,false);(10,true);(16,false)];
      [(0,true);(6,false);(10,true);(16,false);(20,true);(26,false)];
      [(0,true);(6,false);(10,true);(16,false);(20,true);(26,false);(30,true);(36,false)];
    ]
  in

  check_c n d cdn cd rf rf_l obs time_l exp_l exp_interval_l;
  check_t n d cdn cd rf rf_l obs time_l exp_l exp_interval_l
	  
let map2 _ =
  let c1 = C.create () in
  let n  = "map2" in
  let c2 = C.map (fun x -> x*x) c1 in
  let m2 = C.map2 (+) c1 c2 in
  let nm2= "x*x + x" in
  let rf_l =
    [(0,0);(2,2);(4,4)]
  in
  let time_l =
    [1;3;5]
  in
  let exp_l=
    [0;6;20]
  in
  let exp_interval_l =
    [
      [(0,true);(2,false)];
      [(0,true);(4,false)];
      [(0,true);(6,false)];
    ]
  in

  check_c n c1 nm2 m2 rf rf_l obs time_l exp_l exp_interval_l;
  check_t n c1 nm2 m2 rf rf_l obs time_l exp_l exp_interval_l

let fix _ =
  let c = C.create () in
  let cf,_ =
    C.fix (
	fun c ->
	let cf =
	  C.map (fun x -> x+x) c
	in
	(cf,c)
      )
  in
  let n = "c" in
  let nf = "fix x*x" in
  let rf_l =
    [(0,0);(2,2);(4,4)]
  in
  let time_l =
    [1;3;5]
  in
  let exp_l=
    [0;4;16]
  in
  let exp_interval_l =
    [
      [(0,true);(2,false)];
      [(0,true);(4,false)];
      [(0,true);(6,false)];
    ]
  in
  
  check_c n c nf cf rf rf_l obs time_l exp_l exp_interval_l;
  check_t n c nf cf rf rf_l obs time_l exp_l exp_interval_l
    
let _ =
  bsc_d ();
  bsc_c ();
  map_d ();
  map_c ();
  complete ();
  map2 ();
  fix ()
       
