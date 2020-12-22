(*
  /!\ TODO comment all the code
  Comment not enough explicit and easy to understand !
 *)
exception Not_primitive
exception Before_origin

type time_t = int
module T = struct
  (**
      [module T] contain all the function to interact with the time
   *)
  type t = time_t
  let origin = 0
  (* let infinity = max_int *)
  let next = succ
  let prev = pred
  (* let (>)  = (>) *)
  let (<)  = (<)
  let (=)  = (=)
  let before_origin x = x < origin
  (* let is_origin x = x = origin *)
  let of_string = int_of_string
  let to_string = string_of_int
  let of_int  t = (t:time_t)
  let to_int  t = (t:int)
                    (* let min       = min *)
end

(**
      [module Timemap]
      Timemap is a simple map struct using time as key
      A Timemap is use to save occurence of a signal (module Ca)
      and another one to save valid time interval (module IT)
 *)
module Timemap =
  Map.Make(struct
      type t = T.t
      let compare x y = Stdlib.compare x y
    end)

(**
    [module Ca] save all the occurences of a signal
 *)
module Ca = struct
  (* let empty = Timemap.empty *)

  let add c t v =
    Timemap.add t v c

  (**
      [occ c t : 'a Timemap.t -> time_t -> (time_t,'a) option] is the
      occurence (if it exist) of the signal associated to the cache
      [c] at the key [t]
   *)
  let occ c t =
    try
      Some (t,Timemap.find t c)
    with Not_found ->
      None

  (**
     [last_occ c t : 'a Timemap.t -> time_t -> (time_t,'a) option] is
     the occurence (if it exist) of the signal associated to the cache
     [c] at the key (max {t'| t'<=[t]})
   *)
  let last_occ c t =
    let l,v_o,_r = Timemap.split t c in
    match v_o with
    | Some v -> Some (t,v)
    | None   ->
       if Timemap.is_empty l then
         None
       else
         Some (Timemap.max_binding l)

  (** {2 DEBUG FUNCTION} *)

  (**
     [print c f n : 'a Timemap.t -> ('a -> string) -> string -> unit]
     print all the value of [c] using the function [f]
     [n] is a name given by the user (mostly use to debug and quickly
     identify the Signal)
   *)
  let print c f n =
    Printf.printf "\nValue %s :\n%!" n;
    Timemap.iter (fun i v -> Printf.printf "| %d : %s %!" i (f v)) c;
    Printf.printf "|\n\n%!"
end

(**
   [module IT](Interval Time) is the set of function to modify the set
   of valid time of a signal

   if a time is valid in the associated Timemap then the value
   associated in the cache of the signal is valid
   [itl] : iterval time list
 *)
module IT = struct
  (* type t = bool Timemap.t *)

  (** [valid itl t : bool Timemap.t -> time_t -> bool] is the validity
  if the time [t].
  if true then [t] is a valid time so the occurence at time [t] of the
  associated signal is still valid and can be use
   *)
  let valid itl t =
    let l,b_o,_r = Timemap.split t itl in
    match b_o with
    | Some b -> (t,b)
    | None ->
       if Timemap.is_empty l then
         (T.of_int (-1) , false)
       else
         Timemap.max_binding l

  (**
      [clean itl] will prevent the list to have 2 consecutive equal
      boolean value

      TODO : Not the best way to do
   *)
  let clean itl =
    let f t b =
      let t_prev, b_prev = valid itl (T.prev t) in
      if t_prev < T.origin || b_prev != b then true
      else false
    in
    Timemap.filter f itl

  (**
      [already_invalidate itl t : bool Timemap.t -> time_t -> bool]
      prevent to invalidate an already invalidate [itl]

      it is use to stop the loop in [S.fix] and reduce number of
      calcul
   *)
  let already_invalidate itl t =
    if Timemap.is_empty itl then false
    else
      let (t',b) = Timemap.max_binding itl in
      not b && t' <= t

  (**
      [invalidate itl t : bool Timemap.t -> time_t -> bool Timemap.t]
      is [itl] truncate at [t-1] and adding [(t,false)]
   *)
  let invalidate itl t =
    if not (already_invalidate itl t) then
      let itl = Timemap.filter (fun t' _ -> t' < t ) itl in
      let itl = Timemap.add t false itl in
      clean itl
    else
      itl

  (**
      [validate itl t t'] is [itl] modified so that (at least)
      [t] to [t'] is valid

      3 steps :
      delete all value between t t'
      add (t,true)
      if t' was false then add (t'+1,false)
   *)
  let validate itl t t' =
    let _tb', b' = valid itl t' in
    let itl = Timemap.filter (fun t'' _ -> t'' < t || t'' > t' ) itl in
    let itl = Timemap.add t true itl in
    let itl = if not b' && not (Timemap.mem (T.next t') itl) then Timemap.add (T.next t') false itl else itl in
    clean itl

  (**
     [printf itl name] print all valid time of the signal with name 'name'
   *)
  let print itl name =
    let to_str t b acc = (T.to_string t) ^ " : " ^ (string_of_bool b) ^ " | " ^ acc in
    let str = Timemap.fold to_str itl "" in
    Printf.printf "time %s : \n| %s\n" name str

end

(**
   module type occurence signal and event are signature for first-class
   module
 *)

(**
   Module type of occurence, this is where the differential part has
   to be implemented

   TODO differential occurence
 *)
module type Occurence = sig
  type t
end

(**
    Module Type of a Signal
    function to interact with the signal

 *)
module type Signal = sig
  include Occurence

  (**
    [primitive] indicate if that signal will receive it occurence
    from the user/client or transforme occurence from another signal
   *)
  val primitive     : bool

  (**
     cache holds all the value of the signal
   *)
  val cache         : t Timemap.t ref

  (**
     interval_time hold boolean to indicate if the time (key here)
     is not valid anymore and need to be check
   *)
  val interval_time : bool Timemap.t ref

  (**

   *)
  val observe       : bool -> time_t -> t
  val invalidate    : time_t -> unit
  val definition    : [ `N | `D | `C] -> time_t -> (time_t * t) option
  val refine        : time_t -> time_t -> t -> unit
  val push          : time_t -> unit
end

(**
 Type of Event
 *)
module type Event = sig
  include Signal
  val production    : (module Signal) list ref
end

type 'a event = (module Event with type t= 'a)
type d
type c
type ('t,'a) t = 'a event ref


(**
    Module S contain all the function of creation and interaction with
    signal
 *)
module S = struct

  (** [refine s t o] call the refine fonction of [S]

      If [s] is a primitive signal then call [S.refine]
      else @raise Not_primitive
   *)
  let refine (type o) s t (o:o) =
    if T.before_origin t then raise Before_origin
    else
      let module S = (val !s:Event with type t = o) in
      if S.primitive then S.refine t t o
      else raise Not_primitive

  (** [observe s t] call the observe function of [S]*)
  let observe (type o) ?(produce=true) e t =
    if T.before_origin t then raise Before_origin
    else
      let module E = (val !e:Event with type t = o) in
      E.observe produce t

  (**
      [create : (time_t -> o) -> o event ref]

      Create a event producing a primitive signal.
      This is the only signal that the user can refine.
      That signal differs from other, it doesn't use the interval list
      as all the value that are refine are always valid

      It don't use also the push function as push is used when a new
      value is occured by a signal it depend on search_fct is
      different for discrete and continuous event

      to see comment of function, CF function [make] where it is
      describe
   *)
  let create (type o) sig_t =
    ref
      (module struct
         type t = o

         let cache         = ref Timemap.empty
         let interval_time = ref Timemap.empty
         let production    = ref []
         let primitive     = true

         let definition search_t t =
           let rec search = function
             | `D -> Ca.occ      !cache t
             | `C -> Ca.last_occ !cache t
             | _  -> search sig_t
           in
           search search_t

         let invalidate_production t =
           let invalidate (module S:Signal) =
             S.invalidate t
           in
           List.iter invalidate !production

         let refine t _ o =
           cache := Ca.add !cache t o;
           invalidate_production t

         let push _ = ()
         let invalidate _ = ()

         let propagate t =
           let aux (module S:Signal) =
             S.push t
           in
           List.iter aux !production

         let observe produce t =
           match definition sig_t t with
           | None -> raise Not_found
           | Some (t',o) ->
              if produce then propagate t';
              o
       end:Event with type t = o)

  (**
      [make :(time_t -> (time_t * o) option) -> ('o event ref) ->
      (time_t -> (time_t * o) option -> (time_t -> unit) -> 'o event
      ref]

      [make search_fct e' definition push] is a new signal created by
      an event transforming the signal produce by the event e'

      [definition_fct] define what to do when there is no occurence or
      occurence is outdated

      [push_fct] define what to do when a new occurence is push to
      that event

      TODO comment all function
   *)
  let make (type o') (type o) sig_t e' definition_fct push_fct=
    let module E' = (val !e':Event with type t = o') in
    let e =
      ref
        (module struct
           type t = o

           let cache         = ref Timemap.empty
           let interval_time = ref Timemap.empty
           let production    = ref []
           let primitive     = false

           let refine t' t o =
             if t' < t then cache := Timemap.filter (fun t'' _ -> t'' < t' || t'' > t) !cache;
             cache := Ca.add !cache t' o;
             interval_time := IT.validate !interval_time t' t

           (* TODO if `C && b then return good value (possibly > at t' with previous_value  *)

           let definition search_t t =
             let rec search = function
               | `D -> Ca.occ !cache t
               | `C -> Ca.last_occ !cache t
               | _  -> search sig_t
             in
             if T.before_origin t then None
             else
               let _, b = IT.valid !interval_time t in (* no need of time value, as we just wanna know if t is valid, and we don't need to know from when it's valid *)
               if b then
                 search search_t
               else
                 match definition_fct t with
                 | None -> None
                 | Some (t'',o) as t_o ->
                    refine t'' t o;
                    t_o

           let invalidate_production t =
             let invalidate (module S:Signal) =
               S.invalidate t
             in
             List.iter invalidate !production

           let invalidate t =
             if not (IT.already_invalidate !interval_time t)
             then begin
                 interval_time := IT.invalidate !interval_time t;
                 invalidate_production t
               end

           let propagate t =
             let aux (module S:Signal) =
               S.push t
             in
             List.iter aux !production

           let push t =
             match E'.definition (`N) t with
             | None -> invalidate t (* cannot happend TODO clean that*)
             | Some (_,o) ->
                match push_fct t o with
                | None       -> invalidate t
                | Some (t,o) -> refine t t o;
                                propagate t

           let observe produce t =
             let _, b  = IT.valid !interval_time t in
             let not_valid = not b in
             match definition sig_t t with
             | None        -> raise Not_found
             | Some (t',o) ->
                if produce then propagate t'
                else if not_valid then invalidate_production t;
                o

         end:Event with type t=o)
    in
    let module E = (val !e) in
    E'.production := (module E:Signal)::!(E'.production);
    e

  (**
      [make2 sig_t e1 e2 def_fct push_fct] is a new signal created by
      an event transforming two signals
   *)
  let make2 (type o1) (type o2) (type o) sig_t e1 e2 definition_fct push_fct =
    let module E1 = (val !e1:Event with type t = o1) in
    let module E2 = (val !e2:Event with type t = o2) in
    let e =
      ref
        (module struct
           type t = o

           let cache         = ref Timemap.empty
           let interval_time = ref Timemap.empty
           let production    = ref []
           let primitive     = false

           let refine t' t o =
             if t' < t then cache := Timemap.filter (fun t'' _ -> t'' < t' || t'' > t) !cache;
             cache := Ca.add !cache t' o;
             interval_time := IT.validate !interval_time t' t

           (* HERE TODO new definition with `D & `C; del pull *)

           (* HERE TODO, if `C && b then return good value (possibly > at t' with previous_value  *)
           let definition search_t t =
             let rec search = function
               | `D -> Ca.occ !cache t
               | `C -> Ca.last_occ !cache t
               | _  -> search sig_t
             in
             if T.before_origin t then None
             else
               let _, b = IT.valid !interval_time t in
               if b then
                 search search_t
               else
                 match definition_fct t with
                 | None -> None
                 | Some (t'',o) as t_o ->
                    refine t'' t o;
                    t_o

           let invalidate_production t =
             let invalidate (module S:Signal) =
               S.invalidate t
             in
             List.iter invalidate !production

           let invalidate t =
             if not (IT.already_invalidate !interval_time t)
             then begin
                 interval_time := IT.invalidate !interval_time t;
                 invalidate_production t
               end

           let propagate t =
             let aux (module S:Signal) =
               S.push t
             in
             List.iter aux !production

           let push t =
             match E1.definition `N t, E2.definition `N t  with
             |None, None -> invalidate t (* TODO not possible as push is call when new occ*)
             | o1, o2    ->
                match push_fct o1 o2 with
                | None       -> invalidate t
                | Some (t,o) -> refine t t o;
                                propagate t

           let observe produce t =
             let _, b  = IT.valid !interval_time t in
             let not_valid = not b in
             match definition sig_t t with
             | None        -> raise Not_found
             | Some (t',o) ->
                if produce then
                  propagate t'
                else if not_valid then
                  invalidate_production t;
                o
         end:Event with type t=o)
    in
    let module E = (val !e) in
    E1.production := (module E:Signal)::!(E1.production);
    E2.production := (module E:Signal)::!(E2.production);
    e

  (** DEBUG FUNCTION *)

  (**
      [empty: 'a Signal -> unit] empty the cache of the signal
   *)
  let empty (type o ) e =
    let module E = (val !e:Event with type t = o) in
    let _ = E.cache := Timemap.empty in
    E.interval_time := Timemap.empty

  (**
      [print_value: 'a Signal -> ('a -> string) -> unit] print all
      value of the signal usgin the function given
   *)
  let print_value  (type o) e fct =
    let module E = (val !e:Event with type t = o) in
    Ca.print !(E.cache) fct

  (**
     [print_time: signal -> unit] print the time valid list of the signal
   *)
  let print_time (type o) e =
    let module E = (val !e:Event with type t = o) in
    IT.print !(E.interval_time)

  (**
      [get_vl_list: Signal -> (time_t, bool) list] is the time valid list
   *)
  let get_vl_list (type o) e =
    let module E = (val !e:Event with type t = o) in
    let to_list t b acc = (T.to_int t,b)::acc in
    Timemap.fold to_list !(E.interval_time) []
end


(**
   Module E contain all the function to create Events

   A common value to all the function is [sig_t].
   [sig_t = `D | `C] correspond to the output signal type (Discrete or
   Continue)

   All those functions are calling [make] or [make2] of [module S]

   to call [make(2)] two functions has to be define

   [definition: time_t -> option (time_t, 'a)] where 'a
   is the type of the occurences of the output signal.

   [definition t] is the new occurence of the output signal at the
   time [t]. If [None] then there is no new value,
   else if [Some(t', o)] then o is the new occurence at time [t']
   ([t'<=t]).
   [t'] can be inferior at [t] in the case of a continuous signal.

   [push: time_t -> 'a -> option(time_t, 'b)] where the occurences of
   the output signal are of type 'b and the occurences of the input
   signal are of type 'a

   [push t o] is called when a new occurence is define in the input
   signal and the propagation is asked by the client/user
   if [None] then no new occurence appear in the output signal
   else if [Some(t',o)] a new occurence appear in the output signal

   if wont define sig_t in all the function because it is always the
   same idea

 *)
module E = struct

  (**
      [map: sig_t -> (o' -> 'b) -> (o' Event) -> 'b Event)]

      [map sig_t f e'] map the function [f] to all occurences of the
      signal produce by [e']
   *)
  let map (type o') sig_t fct e' =
    let definition t =
      let module E' = (val !e':Event with type t = o') in
      match E'.definition `C t with
      | None -> None
      | Some (t,o) -> Some (t, fct o)
    in
    let push t o = Some (t,fct o) in
    S.make sig_t e' definition push

  (**
     [complete: sig_t -> o' `D Event] the event with the same
     occurence of the input Discrete Signal producing an output
     Continuous Signal
   *)
  let complete (type o') sig_t e' =
    let definition t =
      let module E' = (val !e':Event with type t = o') in
      E'.definition `C t
    in
    let push t o = Some (t, o)
    in
    S.make sig_t e' definition push

  (**
     [complete_default: sig_t -> o' `D Event -> o'] the event with the
     same occurence of the input Discrete Signal producing an output
     Continuous Signal.
     If the input signal don't have an occurence at time [t] the the
     default occurence is used [d_o]
   *)

  let complete_default (type o') sig_t e' d_o   =
    let definition t =
      let module E' = (val !e':Event with type t = o') in
      match E'.definition `C t with
      | None        -> Some (T.origin,d_o)
      | Some (t',o) -> if t' = t then Some (t',o)
                       else Some (T.next t',d_o)
    in
    let push t o = Some (t, o)
    in
    S.make sig_t e' definition push

  (**
      Same as map but the event take 2 input signal
   *)
  let map2 (type o1) (type o2) (type o) sig_t  (fct: o1 -> o2 -> o) e1 e2 =
    let definition t =
      let module E1 = (val !e1:Event with type t = o1) in
      let module E2 = (val !e2:Event with type t = o2) in
      match E1.definition sig_t t, E2.definition sig_t t with
      |None, _| _, None -> None
      |Some (t1, o1), Some (t2, o2) ->
        Some (max t1 t2, fct o1 o2) (* t1 = t2 if `D else t1 <=> t2 *)
    in
    let push o1 o2 =
      match o1, o2 with
      |None,_|_,None -> None
      |Some (t1,o1),Some (t2,o2) ->
        Some (max t1 t2, fct o1 o2)
    in
    S.make2 sig_t e1 e2 definition push

  (**
      [previous: sig_t -> o' -> 'o Event] is the previous occurence of
      the input signal.
      if no occurence has occur yet, the default occurence is used [d_o]
   *)
  let previous (type o') sig_t  d_o e'  =
    let definition t =
      if T.before_origin (T.prev t) then Some (T.origin, d_o)
      else
        let module E' = (val !e':Event with type t = o') in
        match E'.definition `C (T.prev t) with
        | None        -> Some (T.origin , d_o)
        | Some (_t',o) -> Some (t, o)
    in
    let push _t _o = None
                       (* Some (T.next t, o) *)
    in
    S.make sig_t  e' definition push

  (**
     [fix: sig_t -> ((c,'a) Event -> (c,'a) Event * 'b) -> (c,'a)
     Event * 'b] create a fix point using the function define in argument
   *)
  let fix (type o) sig_t f =
    let e = S.create sig_t in
    let e', v = f e in
    let module E' = (val !e':Event with type t = o) in
    let module E = (val !e:Event with type t = o) in
    E'.production := !(E.production);
    e := !e';
    e,v

end


(**
    module D is one of the two module for the client interface.
    It contain all the function to create Event producing Dicrete
    Signal

    that module doesn't define anything interesting.
    It's just calling the function of [module E] with [`D]

    Not all function of the [module E] are define here,
    for exemple [E.complete] and [E.complete_default] can only be call
    to create an event producing a Continuous Signal
 *)
module D = struct
  let create () = S.create `D

  let map fct e' =
    E.map `D  fct e'

(* let map2 _name fct e1 e2 =
 *   E.map2 `D fct e1 e2 *)
end

(**
    module C is one of the two module for the client interface.
    It contain all the function to create Event producing Continuous
    Signal

    that module doesn't define anything interesting.
    It's just calling the function of [module E] with [`C]

    [complete] and [complete_default] can only be call with an event
    producing a discrete Signal.
    That verification is done by the signature with polymorph type
 *)
module C = struct
  let create ()  = S.create `C
  let map (type o') (type o) (fct:o' -> o) e' =
    E.map `C fct e'

  let complete  (* (type o')  *) e' =
    E.complete `C e'

  let complete_default (* (type o')  *)e' d_o =
    E.complete_default `C e' d_o

  let map2 (type o1) (type o2) (type o) (fct: o1 -> o2 -> o) e1 e2 =
    E.map2  `C fct e1 e2

  let previous (* (type o')  *)d_o e' =
    E.previous `C d_o e'

  let fix  f =
    E.fix `C f
end
