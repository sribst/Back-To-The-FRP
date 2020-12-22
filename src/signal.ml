(**
    Module Signal contain all the function of creation and interaction with
    signal
 *)

exception Not_primitive

exception Before_origin

let refine (type o) s t (o : o) =
  if Time.before_origin t then raise Before_origin
  else
    let module S = (val !s : Sig.Event with type t = o) in
    if S.primitive then S.refine t t o else raise Not_primitive

let observe (type o) ?(produce = true) e t =
  if Time.before_origin t then raise Before_origin
  else
    let module E = (val !e : Sig.Event with type t = o) in
    E.observe produce t

let create (type o) sig_type =
  ref
    ( module struct
      type t = o

      let cache = ref Time.Timemap.empty

      let interval_time = ref Time.Timemap.empty

      let production = ref []

      let primitive = true

      let definition search_t t =
        let rec search = function
          | `D ->
              Ca.occ !cache t
          | `C ->
              Ca.last_occ !cache t
          | _ ->
              search sig_type
        in
        search search_t

      let invalidate_production t =
        let invalidate (module S : Sig.Signal) = S.invalidate t in
        List.iter invalidate !production

      let refine t _ o =
        cache := Ca.add !cache t o ;
        invalidate_production t

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
      with type t = o )

let make (type o' o) sig_t e' definition_fct push_fct =
  let module E' = (val !e' : Sig.Event with type t = o') in
  let e =
    ref
      ( module struct
        type t = o

        let cache = ref Time.Timemap.empty

        let interval_time = ref Time.Timemap.empty

        let production = ref []

        let primitive = false

        let refine t' t o =
          if t' < t then
            cache :=
              Time.Timemap.filter (fun t'' _ -> t'' < t' || t'' > t) !cache ;
          cache := Ca.add !cache t' o ;
          interval_time := Interval.validate !interval_time t' t

        (* TODO if `C && b then return good value (possibly > at t' with previous_value  *)

        let definition search_t t =
          let rec search = function
            | `D ->
                Ca.occ !cache t
            | `C ->
                Ca.last_occ !cache t
            | _ ->
                search sig_t
          in
          if Time.before_origin t then None
          else
            let (_, b) = Interval.valid !interval_time t in
            (* no need of time value, as we just wanna know if t is valid, and we don't need to know from when it's valid *)
            if b then search search_t
            else
              match definition_fct t with
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
          match E'.definition `N t with
          | None ->
              invalidate t (* cannot happend TODO clean that*)
          | Some (_, o) -> (
            match push_fct t o with
            | None ->
                invalidate t
            | Some (t, o) ->
                refine t t o ; propagate t )

        let observe produce t =
          let (_, b) = Interval.valid !interval_time t in
          let not_valid = not b in
          match definition sig_t t with
          | None ->
              raise Not_found
          | Some (t', o) ->
              if produce then propagate t'
              else if not_valid then invalidate_production t ;
              o
      end : Sig.Event
        with type t = o )
  in
  let module E = (val !e) in
  E'.production := (module E : Sig.Signal) :: !E'.production ;
  e

let make2 (type o1 o2 o) sig_t e1 e2 definition_fct push_fct =
  let module E1 = (val !e1 : Sig.Event with type t = o1) in
  let module E2 = (val !e2 : Sig.Event with type t = o2) in
  let e =
    ref
      ( module struct
        type t = o

        let cache = ref Time.Timemap.empty

        let interval_time = ref Time.Timemap.empty

        let production = ref []

        let primitive = false

        let refine t' t o =
          if t' < t then
            cache :=
              Time.Timemap.filter (fun t'' _ -> t'' < t' || t'' > t) !cache ;
          cache := Ca.add !cache t' o ;
          interval_time := Interval.validate !interval_time t' t

        (* HERE TODO new definition with `D & `C; del pull *)

        (* HERE TODO, if `C && b then return good value (possibly > at t' with previous_value  *)
        let definition search_t t =
          let rec search = function
            | `D ->
                Ca.occ !cache t
            | `C ->
                Ca.last_occ !cache t
            | _ ->
                search sig_t
          in
          if Time.before_origin t then None
          else
            let (_, b) = Interval.valid !interval_time t in
            if b then search search_t
            else
              match definition_fct t with
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
          match (E1.definition `N t, E2.definition `N t) with
          | (None, None) ->
              invalidate t
          (* TODO not possible as push is call when new occ*)
          | (o1, o2) -> (
            match push_fct o1 o2 with
            | None ->
                invalidate t
            | Some (t, o) ->
                refine t t o ; propagate t )

        let observe produce t =
          let (_, b) = Interval.valid !interval_time t in
          let not_valid = not b in
          match definition sig_t t with
          | None ->
              raise Not_found
          | Some (t', o) ->
              if produce then propagate t'
              else if not_valid then invalidate_production t ;
              o
      end : Sig.Event
        with type t = o )
  in
  let module E = (val !e) in
  E1.production := (module E : Sig.Signal) :: !E1.production ;
  E2.production := (module E : Sig.Signal) :: !E2.production ;
  e

let empty (type o) e =
  let module E = (val !e : Sig.Event with type t = o) in
  let _ = E.cache := Time.Timemap.empty in
  E.interval_time := Time.Timemap.empty

(** {2 Debug function} *)
let print_value (type o) e fct =
  let module E = (val !e : Sig.Event with type t = o) in
  Ca.print !E.cache fct

let print_time (type o) e =
  let module E = (val !e : Sig.Event with type t = o) in
  Interval.print !E.interval_time

let get_vl_list (type o) e =
  let module E = (val !e : Sig.Event with type t = o) in
  let to_list t b acc = (Time.to_int t, b) :: acc in
  Time.Timemap.fold to_list !E.interval_time []
