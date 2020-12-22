type signal_type = [`C | `D | `N]

(* module type Signal = sig
 *   type t
 *
 *   val primitive : bool
 *
 *   val cache : t Cache.t ref
 *
 *   val interval_time : Interval.t ref
 *
 *   val observe : bool -> Time.t -> t
 *
 *   val invalidate : Time.t -> unit
 *
 *   val definition : signal_type -> Time.t -> (Time.t * t) option
 *
 *   val refine : Time.t -> Time.t -> t -> unit
 *
 *   val push : Time.t -> unit
 * end *)

(* type ('t, 'a) signal = (module Signal with type t = 'a) ref *)

let create (type occ) sig_type =
  ref
    ( module struct
      type t = occ

      let cache = ref Cache.empty

      let interval_time = ref Interval.empty

      let production = ref []

      let primitive = true

      let definition sig_type time =
        let rec search = function
          | `D ->
              Cache.occurence time !cache
          | `C ->
              Cache.last_occurence time !cache
          | _ ->
              search sig_type
        in
        search sig_type

      let invalidate_production time =
        List.iter
          (fun (module S : Sig.Signal) -> S.invalidate time)
          !production

      let refine time _ occurence =
        cache := Cache.add !cache time occurence ;
        invalidate_production time

      let push _ = ()

      let invalidate _ = ()

      let propagate t =
        let aux (module S : Sig.Signal) = S.push t in
        List.iter aux !production

      let observe produce t =
        match definition sig_type t with
        | None ->
            raise Not_found
        | Some (t', o) ->
            if produce then propagate t' ;
            o
    end : Sig.Event
      with type t = occ )

let make (type occ_t new_occ_t) ~signal_type ~event ~definition ~push =
  let module Event = (val !event : Sig.Event with type t = occ_t) in
  let new_event =
    ref
      ( module struct
        type t = new_occ_t

        let cache = ref Cache.empty

        let interval_time = ref Interval.empty

        let production = ref []

        let primitive = false

        let refine t' t o =
          if t' < t then
            cache := Cache.filter (fun t'' _ -> t'' < t' || t'' > t) !cache ;
          cache := Cache.add !cache t' o ;
          interval_time := Interval.validate !interval_time t' t

        (* TODO if `C && b then return good value (possibly > at t' with
           previous_value *)

        let rec search time = function
          | `D ->
              Cache.occurence time !cache
          | `C ->
              Cache.last_occurence time !cache
          | `N ->
              search time signal_type

        let definition search_signal_type time =
          if Time.before_origin time then None
          else
            let (_time, is_valid) = Interval.valid time !interval_time in
            if is_valid then search time search_signal_type
            else
              match definition time with
              | None ->
                  None
              | Some (t'', o) as t_o ->
                  refine t'' time o ; t_o

        let invalidate_production t =
          let invalidate (module S : Sig.Signal) = S.invalidate t in
          List.iter invalidate !production

        let invalidate t =
          if not (Interval.already_invalidate !interval_time t) then (
            interval_time := Interval.invalidate !interval_time t ;
            invalidate_production t )

        let propagate t =
          let aux (module S : Sig.Signal) = S.push t in
          List.iter aux !production

        let push t =
          match Event.definition `N t with
          | None ->
              invalidate t (* cannot happend TODO clean that*)
          | Some (_, o) -> (
            match push t o with
            | None ->
                invalidate t
            | Some (t, o) ->
                refine t t o ; propagate t )

        let observe produce t =
          let (_, b) = Interval.valid t !interval_time in
          let not_valid = not b in
          match definition signal_type t with
          | None ->
              raise Not_found
          | Some (t', o) ->
              if produce then propagate t'
              else if not_valid then invalidate_production t ;
              o
      end : Sig.Event
        with type t = new_occ_t )
  in
  let module New_event = (val !new_event) in
  Event.production := (module New_event : Sig.Signal) :: !Event.production ;
  new_event

let make2 (type occ_t1 occ_t2 new_occ_t) ~signal_type ~event1 ~event2
    ~definition ~push =
  let module Event1 = (val !event1 : Sig.Event with type t = occ_t1) in
  let module Event2 = (val !event2 : Sig.Event with type t = occ_t2) in
  let e =
    ref
      ( module struct
        type t = new_occ_t

        let cache = ref Cache.empty

        let interval_time = ref Interval.empty

        let production = ref []

        let primitive = false

        let refine t' t o =
          if t' < t then
            cache := Cache.filter (fun t'' _ -> t'' < t' || t'' > t) !cache ;
          cache := Cache.add !cache t' o ;
          interval_time := Interval.validate !interval_time t' t

        (* HERE TODO new definition with `D & `C; del pull *)

        (* HERE TODO, if `C && b then return good value (possibly > at t' with previous_value  *)
        let definition search_t t =
          let rec search = function
            | `D ->
                Cache.occurence t !cache
            | `C ->
                Cache.last_occurence t !cache
            | _ ->
                search signal_type
          in
          if Time.before_origin t then None
          else
            let (_, b) = Interval.valid t !interval_time in
            if b then search search_t
            else
              match definition t with
              | None ->
                  None
              | Some (t'', o) as t_o ->
                  refine t'' t o ; t_o

        let invalidate_production t =
          let invalidate (module S : Sig.Signal) = S.invalidate t in
          List.iter invalidate !production

        let invalidate t =
          if not (Interval.already_invalidate !interval_time t) then (
            interval_time := Interval.invalidate !interval_time t ;
            invalidate_production t )

        let propagate t =
          let aux (module S : Sig.Signal) = S.push t in
          List.iter aux !production

        let push t =
          match (Event1.definition `N t, Event2.definition `N t) with
          | (None, None) ->
              invalidate t
          (* TODO not possible as push is call when new occ*)
          | (o1, o2) -> (
            match push o1 o2 with
            | None ->
                invalidate t
            | Some (t, o) ->
                refine t t o ; propagate t )

        let observe produce t =
          let (_, b) = Interval.valid t !interval_time in
          let not_valid = not b in
          match definition signal_type t with
          | None ->
              raise Not_found
          | Some (t', o) ->
              if produce then propagate t'
              else if not_valid then invalidate_production t ;
              o
      end : Sig.Event
        with type t = new_occ_t )
  in
  let module E = (val !e) in
  Event1.production := (module E : Sig.Signal) :: !Event1.production ;
  Event2.production := (module E : Sig.Signal) :: !Event2.production ;
  e

exception Not_primitive

exception Before_origin

(* type signal_type = [`C | `D | `N] *)

type ('t, 'a) event = ('t, 'a) Sig.event

type ('t, 'a) signal = ('t, 'a) Sig.signal

let refine (type occ) event time (occurence : occ) =
  if Time.before_origin time then raise Before_origin
  else
    let module Event = (val !event : Sig.Event with type t = occ) in
    if Event.primitive then Event.refine time time occurence
    else raise Not_primitive

let observe (type occ) ?(produce = true) event time =
  if Time.before_origin time then raise Before_origin
  else
    let module Event = (val !event : Sig.Event with type t = occ) in
    Event.observe produce time

let empty (type o) e =
  let module E = (val !e : Sig.Event with type t = o) in
  let _ = E.cache := Cache.empty in
  E.interval_time := Interval.empty

(** {2 Debug function} *)
let print_value (type o) e fct =
  let module E = (val !e : Sig.Event with type t = o) in
  Cache.print !E.cache fct

let print_time (type o) e =
  let module E = (val !e : Sig.Event with type t = o) in
  Interval.print !E.interval_time

let get_vl_list (type o) e =
  let module E = (val !e : Sig.Event with type t = o) in
  let to_list t b acc = (Time.to_int t, b) :: acc in
  Interval.fold to_list !E.interval_time []
