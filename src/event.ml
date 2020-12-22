let event_definition (type occ_t) event =
  let module Event = (val !event : Sig.Event with type t = occ_t) in
  Event.definition

let map signal_type f event =
  let definition time =
    match event_definition event `C time with
    | None ->
        None
    | Some (def_time, occ) ->
        Some (def_time, f occ)
  in
  let push time occ = Some (time, f occ) in
  Signal.make ~signal_type ~event ~definition ~push

let map2 signal_type f event1 event2 =
  let definition time =
    match
      ( event_definition event1 signal_type time,
        event_definition event2 signal_type time )
    with
    | (None, _) | (_, None) ->
        None
    | (Some (def_time1, occ1), Some (def_time2, occ2)) ->
        Some (max def_time1 def_time2, f occ1 occ2)
  in
  let push occ1 occ2 =
    match (occ1, occ2) with
    | (None, _) | (_, None) ->
        None
    | (Some (time1, occ1), Some (time2, occ2)) ->
        Some (max time1 time2, f occ1 occ2)
  in
  Signal.make2 ~signal_type ~event1 ~event2 ~definition ~push

let complete signal_type event =
  let definition time = event_definition event `C time in
  let push time occ = Some (time, occ) in
  Signal.make ~signal_type ~event ~definition ~push

let complete_default signal_type event default =
  let definition time =
    match event_definition event `C time with
    | None ->
        Some (Time.origin, default)
    | Some (def_time, occ) ->
        if def_time = time then Some (def_time, occ)
        else Some (Time.next def_time, default)
  in
  let push time occ = Some (time, occ) in
  Signal.make ~signal_type ~event ~definition ~push

let previous signal_type origin event =
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
  Signal.make ~signal_type ~event ~definition ~push

let fix (type o) sig_t f =
  let e = Signal.create sig_t in
  let (e', v) = f e in
  let module E' = (val !e' : Sig.Event with type t = o) in
  let module E = (val !e : Sig.Event with type t = o) in
  E'.production := !E.production ;
  e := !e' ;
  (e, v)

type discrete

type continuous

type ('t, 'a) event = ('t, 'a) Sig.event

module Discrete = struct
  let create () = Signal.create `D

  let map fct e' = map `D fct e'

  let map2 fct e1 e2 = map2 `D fct e1 e2
end

module Continuous = struct
  let create () = Signal.create `C

  let map f event = map `C f event

  let complete e' = complete `C e'

  let complete_default e' d_o = complete_default `C e' d_o

  let map2 f e1 e2 = map2 `C f e1 e2

  let previous d_o e' = previous `C d_o e'

  let fix f = fix `C f
end
