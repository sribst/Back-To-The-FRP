module type Event = sig
  type t

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

let make_event (type t) (signal : ('t, t) Signal.t) =
  ref
    ( module struct
      type nonrec t = t

      let signal = signal

      let production = ref []

      let propagate time =
        let module S = (val !signal : Signal.Sig with type t = t) in
        let aux (module E : Event) = S.push time in
        List.iter aux !production

      let observe produce t =
        let module S = (val !signal : Signal.Sig with type t = t) in
        let occ = S.observe t |> snd in
        if produce then propagate t ;
        occ

      let invalidate_production time =
        List.iter (fun (module E : Event) -> E.invalidate time) !production

      let invalidate time =
        let module S = (val !signal : Signal.Sig with type t = t) in
        S.invalidate time ; invalidate_production time

      let refine time1 time2 occurence =
        let module S = (val !signal : Signal.Sig with type t = t) in
        S.refine time1 time2 occurence ;
        invalidate_production time1
    end : Sig
      with type t = t )

let create signal_type = make_event @@ Signal.create signal_type

let make (type t t') ~signal_type ~event ~definition ~push =
  let module Event = (val !event : Sig with type t = t) in
  let event =
    make_event
    @@ Signal.make ~signal_type ~signal:Event.signal ~definition ~push
  in
  let module Event = (val !event : Sig with type t = t') in
  Event.production := (module Event) :: !Event.production ;
  event

let make2 (type t1 t2 t') ~signal_type ~event1 ~event2 ~definition ~push =
  let module Event1 = (val !event1 : Sig with type t = t1) in
  let module Event2 = (val !event2 : Sig with type t = t2) in
  let event =
    make_event
    @@ Signal.make2
         ~signal_type
         ~signal1:Event1.signal
         ~signal2:Event2.signal
         ~definition
         ~push
  in
  let module Event = (val !event : Sig with type t = t') in
  Event1.production := (module Event) :: !Event1.production ;
  Event2.production := (module Event) :: !Event2.production ;
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
  make ~signal_type ~event ~definition ~push

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
  make ~signal_type ~event ~definition ~push

let fix (type o) sig_t f =
  let e = create sig_t in
  let (e', v) = f e in
  let module E' = (val !e' : Sig with type t = o) in
  let module E = (val !e : Sig with type t = o) in
  E'.production := !E.production ;
  e := !e' ;
  (e, v)

type discrete

type continuous

module Discrete = struct
  let create () = create `D

  let map fct e' = map `D fct e'

  let map2 fct e1 e2 = map2 `D fct e1 e2
end

module Continuous = struct
  let create () = create `C

  let map f event = map `C f event

  let complete e' = complete `C e'

  let complete_default e' d_o = complete_default `C e' d_o

  let map2 f e1 e2 = map2 `C f e1 e2

  let previous d_o e' = previous `C d_o e'

  let fix f = fix `C f
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
let print_value (type t) event fct =
  let module E = (val !event : Sig with type t = t) in
  Signal.print_value E.signal fct

let print_time (type t) event =
  let module E = (val !event : Sig with type t = t) in
  Signal.print_time E.signal

let get_interval_list (type t) event =
  let module E = (val !event : Sig with type t = t) in
  Signal.get_interval_list E.signal
