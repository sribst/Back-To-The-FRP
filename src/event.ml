type signal_type = [`C | `D | `N]

module type Event = sig
  type t

  val primitive : bool

  val definition : signal_type -> Time.t -> (Time.t * t) option

  val cache : t Cache.t ref

  val interval_time : Interval.t ref

  val push : Time.t -> unit

  val refine : Time.t -> Time.t -> t -> unit

  val observe : produce:bool -> Time.t -> t

  val invalidate : Time.t -> unit
end

module type Sig = sig
  include Event

  val production : (module Event) list ref
end

type ('t, 'a) event = (module Sig with type t = 'a) ref

let create (type t) signal_type =
  ref
    ( module struct
      type nonrec t = t

      let cache = ref Cache.empty

      let interval_time = ref Interval.empty

      let production = ref []

      let primitive = true

      let rec search time = function
        | `D ->
            Cache.occurence time !cache
        | `C ->
            Cache.last_occurence time !cache
        | _ ->
            search time signal_type

      let definition search_signal_type time = search time search_signal_type

      let invalidate_production time =
        List.iter (fun (module E : Event) -> E.invalidate time) !production

      let refine time _time' occurence =
        cache := Cache.add !cache time occurence ;
        invalidate_production time

      let push _ = ()

      let invalidate _ = ()

      let propagate time =
        List.iter (fun (module E : Event) -> E.push time) !production

      let observe ~produce time =
        match definition signal_type time with
        | None ->
            raise Not_found
        | Some (def_time, occurence) ->
            if produce then propagate def_time ;
            occurence
    end : Sig
      with type t = t )

let make (type t t') ~signal_type ~event ~definition:new_definition
    ~push:new_push =
  let module Event = (val !event : Sig with type t = t) in
  let new_event =
    ref
      ( module struct
        type nonrec t = t'

        let cache = ref Cache.empty

        let interval_time = ref Interval.empty

        let production = ref []

        let primitive = false

        let refine time1 time2 occ =
          Printf.printf
            "ref at time <%d,%d>\n"
            (Time.to_int time1)
            (Time.to_int time2) ;
          if time1 < time2 then
            cache := Cache.filter (fun t _ -> t < time1 || t > time2) !cache ;
          cache := Cache.add !cache time1 occ ;
          interval_time := Interval.validate !interval_time time1 time2

        (* TODO if `C && b then return good value (possibly > at t' with previous_value  *)

        let rec search time = function
          | `D ->
              Cache.occurence time !cache
          | `C ->
              Cache.last_occurence time !cache
          | _ ->
              search time signal_type

        let definition search_signal_type time =
          Printf.printf "definition at time %d\n" (Time.to_int time) ;
          if Time.before_origin time then (
            Printf.printf "definition at time %d = None\n" (Time.to_int time) ;
            None )
          else
            let (_time, is_valid) = Interval.valid time !interval_time in
            Printf.printf
              "definition at time %d -> time is valid %b\n"
              (Time.to_int time)
              is_valid ;
            if is_valid then (
              Printf.printf
                "definition time %d -> searching \n"
                (Time.to_int time) ;
              search time search_signal_type )
            else (
              Printf.printf
                "definition time %d -> matching new_def\n"
                (Time.to_int time) ;
              match new_definition time with
              | None ->
                  Printf.printf
                    "definition time %d -> new_def = None \n"
                    (Time.to_int time) ;
                  None
              | Some (def_time, occ) as time_occ ->
                  Printf.printf
                    "definition time %d -> new_def = Some def_time %d \n"
                    (Time.to_int time)
                    (Time.to_int def_time) ;
                  refine def_time time occ ;
                  time_occ )

        let invalidate_production time =
          List.iter (fun (module E : Event) -> E.invalidate time) !production

        let invalidate time =
          if not (Interval.already_invalidate !interval_time time) then (
            interval_time := Interval.invalidate !interval_time time ;
            invalidate_production time )

        let propagate time =
          let aux (module E : Event) = E.push time in
          List.iter aux !production

        let push time =
          match Event.definition `N time with
          | None ->
              invalidate time
          | Some (_def_time, occurence) -> (
            match new_push time occurence with
            | None ->
                invalidate time
            | Some (push_time, push_occ) ->
                refine push_time push_time push_occ ;
                propagate push_time )

        let observe ~produce time =
          let (_time, is_valid) = Interval.valid time !interval_time in
          match definition signal_type time with
          | None ->
              raise Not_found
          | Some (def_time, occurence) ->
              Printf.printf
                "observing time %d; def_time %d"
                (Time.to_int time)
                (Time.to_int def_time) ;
              if produce then propagate def_time
              else if not is_valid then invalidate_production time ;
              occurence
      end : Sig
        with type t = t' )
  in
  let module New_Event = (val !new_event : Sig with type t = t') in
  Event.production := (module New_Event) :: !Event.production ;
  new_event

let make2 (type t1 t2 t') ~signal_type ~event1 ~event2 ~definition ~push =
  let module Event1 = (val !event1 : Sig with type t = t1) in
  let module Event2 = (val !event2 : Sig with type t = t2) in
  let new_event =
    ref
      ( module struct
        type nonrec t = t'

        let cache = ref Cache.empty

        let interval_time = ref Interval.empty

        let production = ref []

        let primitive = false

        let refine time1 time2 occ =
          if time1 < time2 then
            cache := Cache.filter (fun t _ -> t < time1 || t > time2) !cache ;
          cache := Cache.add !cache time1 occ ;
          interval_time := Interval.validate !interval_time time1 time2

        let rec search time = function
          | `D ->
              Cache.occurence time !cache
          | `C ->
              Cache.last_occurence time !cache
          | `N ->
              search time signal_type

        (* HERE TODO new definition with `D & `C; del pull *)

        (* HERE TODO, if `C && b then return good value (possibly > at t' with previous_value  *)
        let definition search_signal_type time =
          if Time.before_origin time then None
          else
            let (_time, is_valid) = Interval.valid time !interval_time in
            if is_valid then search time search_signal_type
            else
              match definition time with
              | None ->
                  None
              | Some (found_time, occ) as time_occ ->
                  refine found_time time occ ; time_occ

        let invalidate_production time =
          let invalidate (module E : Event) = E.invalidate time in
          List.iter invalidate !production

        let invalidate t =
          if not (Interval.already_invalidate !interval_time t) then (
            interval_time := Interval.invalidate !interval_time t ;
            invalidate_production t )

        let propagate time =
          let aux (module E : Event) = E.push time in
          List.iter aux !production

        let push t =
          match (Event1.definition `N t, Event2.definition `N t) with
          | (None, None) ->
              invalidate t (* TODO not possible as push is call when new occ*)
          | (o1, o2) -> (
            match push o1 o2 with
            | None ->
                invalidate t
            | Some (t, o) ->
                refine t t o ; propagate t )

        let observe ~produce time =
          let (_, is_valid) = Interval.valid time !interval_time in
          match definition signal_type time with
          | None ->
              raise Not_found
          | Some (def_time, occurence) ->
              if produce then propagate def_time
              else if not is_valid then invalidate_production def_time ;
              occurence
      end : Sig
        with type t = t' )
  in
  let module New_Event = (val !new_event : Sig with type t = t') in
  Event1.production := (module New_Event) :: !Event1.production ;
  Event2.production := (module New_Event) :: !Event2.production ;
  new_event

let event_definition (type t) event =
  let module Event = (val !event : Sig with type t = t) in
  Event.definition

let map signal_type ~f event =
  let definition time =
    match event_definition event `C time with
    | None ->
        None
    | Some (def_time, occ) ->
        Some (def_time, f occ)
  in
  let push time occ = Some (time, f occ) in
  make ~signal_type ~event ~definition ~push

let map2 signal_type ~f event1 event2 =
  let definition time =
    match
      ( event_definition event1 signal_type time,
        event_definition event2 signal_type time )
    with
    | (None, _) | (_, None) ->
        None
    | (Some (found_time1, occ1), Some (found_time2, occ2)) ->
        Some (max found_time1 found_time2, f occ1 occ2)
  in
  let push occ1 occ2 =
    match (occ1, occ2) with
    | (None, _) | (_, None) ->
        None
    | (Some (time1, occ1), Some (time2, occ2)) ->
        Some (max time1 time2, f occ1 occ2)
  in
  make2 ~signal_type ~event1 ~event2 ~definition ~push

let complete signal_type event =
  let definition time = event_definition event `C time in
  let push time occ = Some (time, occ) in
  make ~signal_type ~event ~definition ~push

let complete_default signal_type ~default event =
  let definition time =
    match event_definition event `C time with
    | None ->
        Some (Time.origin, default)
    | Some (def_time, occ) ->
        if def_time = time then Some (def_time, occ)
        else Some (Time.next def_time, default)
  in
  let push time occ = Some (time, occ) in
  make ~signal_type ~event ~definition ~push

let previous signal_type ~origin event =
  let definition time =
    if Time.before_origin (Time.prev time) then Some (Time.origin, origin)
    else
      match event_definition event `C (Time.prev time) with
      | None ->
          Some (Time.origin, origin)
      | Some (_time, o) ->
          Some (time, o)
  in
  let push _time _occ = None in
  make ~signal_type ~event ~definition ~push

let fix (type t) signal_type ~fix_f =
  let event = create signal_type in
  let (event', value) = fix_f event in
  let module Event' = (val !event' : Sig with type t = t) in
  let module Event = (val !event : Sig with type t = t) in
  Event'.production := !Event.production ;
  event := !event' ;
  (event, value)

type discrete

type continuous

module Discrete = struct
  let create () = create `D

  let map ~f event = map `D ~f event

  let map2 ~f e1 e2 = map2 `D ~f e1 e2
end

module Continuous = struct
  let create () = create `C

  let map ~f event = map `C ~f event

  let map2 ~f e1 e2 = map2 `C ~f e1 e2

  let complete event = complete `C event

  let complete_default ~default event = complete_default `C ~default event

  let previous ~origin event = previous `C ~origin event

  let fix ~fix_f = fix `C ~fix_f
end

exception Not_primitive

exception Before_origin

let refine (type t) event time occurence =
  if Time.before_origin time then raise Before_origin
  else
    let module E = (val !event : Sig with type t = t) in
    if E.primitive then E.refine time time occurence else raise Not_primitive

let observe (type t) ?(produce = true) event time =
  if Time.before_origin time then raise Before_origin
  else
    let module E = (val !event : Sig with type t = t) in
    E.observe ~produce time

let empty (type t) event =
  let module E = (val !event : Sig with type t = t) in
  let _ = E.cache := Cache.empty in
  E.interval_time := Interval.empty

(** {2 Debug function} *)
let print_value (type t) event fct =
  let module Sig = (val !event : Sig with type t = t) in
  Cache.print !Sig.cache fct

let print_time (type t) event =
  let module Sig = (val !event : Sig with type t = t) in
  Interval.print !Sig.interval_time

let get_interval_list (type t) event =
  let module Sig = (val !event : Sig with type t = t) in
  let to_list time is_valid acc = (Time.to_int time, is_valid) :: acc in
  Interval.fold to_list !Sig.interval_time []
