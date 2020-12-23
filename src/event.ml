module type Event = sig
  type t

  val push : Time.t -> unit

  val signal : ('t, t) Signal.t

  val refine : Time.t -> Time.t -> t -> unit

  val observe : bool -> Time.t -> t

  val invalidate : Time.t -> unit
end

module type Sig = sig
  include Event

  val production : (module Event) list ref
end

type ('t, 'a) event = (module Sig with type t = 'a) ref

let create (type t) signal_type =
  let signal = Signal.create signal_type in
  ref
    ( module struct
      type nonrec t = t

      let signal = signal

      let production = ref []

      let propagate time =
        List.iter (fun (module E : Event) -> E.push time) !production

      let invalidate_production time =
        List.iter (fun (module E : Event) -> E.invalidate time) !production

      let observe produce time =
        let module S = (val !signal : Signal.Sig with type t = t) in
        let (def_time, occ) = S.observe time in
        if produce then propagate def_time ;
        occ

      let push _time = ()

      let invalidate _time = ()

      let refine time1 time2 occurence =
        let module S = (val !signal : Signal.Sig with type t = t) in
        S.refine time1 time2 occurence ;
        invalidate_production time1
    end : Sig
      with type t = t )

let make (type t t') ~signal_type ~event ~definition ~push =
  let module Event = (val !event : Sig with type t = t) in
  let signal =
    Signal.make ~signal_type ~signal:Event.signal ~definition ~push
  in
  let event =
    ref
      ( module struct
        type nonrec t = t'

        let signal = signal

        let production = ref []

        let refine =
          let module S = (val !signal : Signal.Sig with type t = t) in
          S.refine

        let invalidate_production time =
          List.iter (fun (module E : Event) -> E.invalidate time) !production

        let invalidate time =
          let module S = (val !signal : Signal.Sig with type t = t) in
          if S.invalidate time then invalidate_production time

        let propagate time =
          List.iter
            (fun (module E : Event) -> ignore (E.push time))
            !production

        let push time =
          let module S = (val !signal : Signal.Sig with type t = t) in
          match S.push time with
          | Some push_time ->
              propagate push_time
          | None ->
              ()

        let observe produce time =
          let module S = (val !signal : Signal.Sig with type t = t) in
          let (_time, is_valid) = Interval.valid time !S.interval_time in
          let (def_time, occ) = S.observe time in
          if produce then propagate def_time
          else if not is_valid then invalidate_production time ;
          occ
      end : Sig
        with type t = t' )
  in
  let module Event = (val !event : Sig with type t = t') in
  Event.production := (module Event) :: !Event.production ;
  event

let make2 (type t1 t2 t') ~signal_type ~event1 ~event2 ~definition ~push =
  let module Event1 = (val !event1 : Sig with type t = t1) in
  let module Event2 = (val !event2 : Sig with type t = t2) in
  let signal =
    Signal.make2
      ~signal_type
      ~signal1:Event1.signal
      ~signal2:Event2.signal
      ~definition
      ~push
  in
  let event =
    ref
      ( module struct
        type nonrec t = t'

        let signal = signal

        let production = ref []

        let refine =
          let module S = (val !signal : Signal.Sig with type t = t) in
          S.refine

        let invalidate_production time =
          List.iter (fun (module E : Event) -> E.invalidate time) !production

        let invalidate time =
          let module S = (val !signal : Signal.Sig with type t = t) in
          if S.invalidate time then invalidate_production time

        let propagate time =
          List.iter (fun (module E : Event) -> E.push time) !production

        let push time =
          let module S = (val !signal : Signal.Sig with type t = t) in
          match S.push time with
          | Some push_time ->
              propagate push_time
          | None ->
              ()

        let observe produce time =
          let module S = (val !signal : Signal.Sig with type t = t) in
          let (_time, is_valid) = Interval.valid time !S.interval_time in
          let (def_time, occ) = S.observe time in
          if produce then propagate def_time
          else if not is_valid then invalidate_production time ;
          occ
      end : Sig
        with type t = t' )
  in
  let module Event = (val !event : Sig with type t = t') in
  Event.production := (module Event) :: !Event.production ;
  event

let event_definition (type t) event =
  let module Event = (val !event : Sig with type t = t) in
  let module S = (val !Event.signal : Signal.Sig with type t = t) in
  S.definition

let map signal_type f event =
  let definition time =
    match event_definition event `C time with
    | None ->
        None
    | Some (def_time, occ) ->
        Some (def_time, f occ)
  in
  let push time occ = Some (time, f occ) in
  make ~signal_type ~event ~definition ~push

let map2 signal_type f event1 event2 =
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
  let push _time _occ = None (* Some (Time.next t, o) *) in
  make ~signal_type ~event ~definition ~push

let fix (type t) signal_type ~fix_f =
  let event = create signal_type in
  let (event', fixpoint) = fix_f event in
  let module Event = (val !event : Sig with type t = t) in
  let module Event' = (val !event' : Sig with type t = t) in
  Event'.production := !Event.production ;
  event := !event' ;
  (event, fixpoint)

type discrete

type continuous

module Discrete = struct
  let create () = create `D

  let map f event = map `D f event

  let map2 f e1 e2 = map2 `D f e1 e2
end

module Continuous = struct
  let create () = create `C

  let map f event = map `C f event

  let complete event = complete `C event

  let complete_default ~default event = complete_default `C ~default event

  let map2 f e1 e2 = map2 `C f e1 e2

  let previous ~origin event = previous `C ~origin event

  let fix fix_f = fix `C ~fix_f
end

let refine (type t) event time occurence =
  let module E = (val !event : Sig with type t = t) in
  E.refine time time occurence

let observe (type t) event produce time =
  let module E = (val !event : Sig with type t = t) in
  E.observe produce time

let empty (type t) event =
  let module E = (val !event : Sig with type t = t) in
  Signal.empty E.signal

(** {2 Debug function} *)
let print_value (type t) event f =
  let module E = (val !event : Sig with type t = t) in
  Signal.print_value E.signal f

let print_time (type t) event =
  let module E = (val !event : Sig with type t = t) in
  Signal.print_time E.signal

let get_interval_list (type t) event =
  let module E = (val !event : Sig with type t = t) in
  Signal.get_interval_list E.signal
