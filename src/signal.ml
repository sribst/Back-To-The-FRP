type signal_type = [`C | `D | `N]

module type Sig = sig
  type t

  val observe : Time.t -> Time.t * t

  val push : Time.t -> Time.t option

  val refine : Time.t -> Time.t -> t -> unit

  val definition : signal_type -> Time.t -> (Time.t * t) option

  val primitive : bool

  val cache : t Cache.t ref

  val interval_time : Interval.t ref

  val invalidate : Time.t -> bool
end

type ('t, 'a) signal = (module Sig with type t = 'a) ref

type ('t, 'a) t = ('t, 'a) signal

exception Not_primitive

exception Before_origin

let create (type t) signal_type =
  ref
    ( module struct
      type nonrec t = t

      let cache = ref Cache.empty

      let interval_time = ref Interval.empty

      let primitive = true

      let push _time = None

      let invalidate _time = false

      let rec search time = function
        | `D ->
            Cache.occurence time !cache
        | `C ->
            Cache.last_occurence time !cache
        | `N ->
            search time signal_type

      let definition search_signal_type time = search time search_signal_type

      let refine time _time occurence =
        cache := Cache.add !cache time occurence

      let observe time =
        match definition signal_type time with
        | None ->
            raise Not_found
        | Some (time, occurence) ->
            (time, occurence)
    end : Sig
      with type t = t )

let make (type t t') ~signal_type ~signal ~definition ~push =
  let module S = (val !signal : Sig with type t = t) in
  ref
    ( module struct
      type nonrec t = t'

      let cache = ref Cache.empty

      let interval_time = ref Interval.empty

      let primitive = false

      let invalidate t =
        if not (Interval.already_invalidate !interval_time t) then (
          interval_time := Interval.invalidate !interval_time t ;
          true )
        else false

      let refine time1 time2 occ =
        if time1 < time2 then
          cache := Cache.filter (fun t _ -> t < time1 || t > time2) !cache ;
        cache := Cache.add !cache time1 occ ;
        interval_time := Interval.validate !interval_time time1 time2

      let push time =
        match S.definition `N time with
        | None ->
            ignore (invalidate time) (* cannot happend TODO clean that*) ;
            None
        | Some (_found_time, occ) -> (
          match push time occ with
          | None ->
              ignore (invalidate time) ;
              None
          | Some (push_time, occ) ->
              refine push_time push_time occ ;
              Some push_time )

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
            | Some (time, occ) as time_occ ->
                refine time time occ ; time_occ

      let observe time =
        match definition signal_type time with
        | None ->
            raise Not_found
        | Some (time, occurence) ->
            (time, occurence)
    end : Sig
      with type t = t' )

let make2 (type t1 t2 t') ~signal_type ~signal1 ~signal2 ~definition ~push =
  let module Signal1 = (val !signal1 : Sig with type t = t1) in
  let module Signal2 = (val !signal2 : Sig with type t = t2) in
  ref
    ( module struct
      type t = t'

      let cache = ref Cache.empty

      let interval_time = ref Interval.empty

      let primitive = false

      let invalidate t =
        if not (Interval.already_invalidate !interval_time t) then (
          interval_time := Interval.invalidate !interval_time t ;
          true )
        else false

      let refine time1 time2 occ =
        if time1 < time2 then
          cache := Cache.filter (fun t _ -> t < time1 || t > time2) !cache ;
        cache := Cache.add !cache time1 occ ;
        interval_time := Interval.validate !interval_time time1 time2

      let push time =
        match (Signal1.definition `N time, Signal2.definition `N time) with
        | (None, None) ->
            (* TODO not possible as push is call when new occ*)
            ignore (invalidate time) ;
            None
        | (time_occ1, time_occ2) -> (
          match push time_occ1 time_occ2 with
          | None ->
              ignore (invalidate time) ;
              None
          | Some (push_time, occ) ->
              refine push_time push_time occ ;
              Some push_time )

      (* HERE TODO new definition with `D & `C; del pull *)

      let rec search time = function
        | `D ->
            Cache.occurence time !cache
        | `C ->
            Cache.last_occurence time !cache
        | `N ->
            search time signal_type

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

      let observe time =
        match definition signal_type time with
        | None ->
            raise Not_found
        | Some (time, occurence) ->
            (time, occurence)
    end : Sig
      with type t = t' )

let refine (type t) signal time occurence =
  if Time.before_origin time then raise Before_origin
  else
    let module Sig = (val !signal : Sig with type t = t) in
    if Sig.primitive then Sig.refine time time occurence
    else raise Not_primitive

let observe (type t) signal time =
  if Time.before_origin time then raise Before_origin
  else
    let module Sig = (val !signal : Sig with type t = t) in
    Sig.observe time |> snd

let empty (type t) signal =
  let module Sig = (val !signal : Sig with type t = t) in
  let _ = Sig.cache := Cache.empty in
  Sig.interval_time := Interval.empty

(** {2 Debug function} *)
let print_value (type t) signal fct =
  let module Sig = (val !signal : Sig with type t = t) in
  Cache.print !Sig.cache fct

let print_time (type t) signal =
  let module Sig = (val !signal : Sig with type t = t) in
  Interval.print !Sig.interval_time

let get_interval_list (type t) signal =
  let module Sig = (val !signal : Sig with type t = t) in
  let to_list time is_valid acc = (Time.to_int time, is_valid) :: acc in
  Interval.fold to_list !Sig.interval_time []
