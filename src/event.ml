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

(**
      [map: sig_t -> (o' -> 'b) -> (o' Event) -> 'b Event)]

      [map sig_t f e'] map the function [f] to all occurences of the
      signal produce by [e']
 *)
let map (type o') sig_t fct e' =
  let definition t =
    let module E' = (val !e' : Sig.Event with type t = o') in
    match E'.definition `C t with
    | None ->
        None
    | Some (t, o) ->
        Some (t, fct o)
  in
  let push t o = Some (t, fct o) in
  Signal.make sig_t e' definition push

(**
     [complete: sig_t -> o' `D Event] the event with the same
     occurence of the input Discrete Signal producing an output
     Continuous Signal
 *)
let complete (type o') sig_t e' =
  let definition t =
    let module E' = (val !e' : Sig.Event with type t = o') in
    E'.definition `C t
  in
  let push t o = Some (t, o) in
  Signal.make sig_t e' definition push

(**
     [complete_default: sig_t -> o' `D Sig.Event -> o'] the event with the
     same occurence of the input Discrete Signal producing an output
     Continuous Signal.
     If the input signal don't have an occurence at time [t] the the
     default occurence is used [d_o]
 *)

let complete_default (type o') sig_t e' d_o =
  let definition t =
    let module E' = (val !e' : Sig.Event with type t = o') in
    match E'.definition `C t with
    | None ->
        Some (Time.origin, d_o)
    | Some (t', o) ->
        if t' = t then Some (t', o) else Some (Time.next t', d_o)
  in
  let push t o = Some (t, o) in
  Signal.make sig_t e' definition push

(**
      Same as map but the event take 2 input signal
 *)
let map2 (type o1 o2 o) sig_t (fct : o1 -> o2 -> o) e1 e2 =
  let definition t =
    let module E1 = (val !e1 : Sig.Event with type t = o1) in
    let module E2 = (val !e2 : Sig.Event with type t = o2) in
    match (E1.definition sig_t t, E2.definition sig_t t) with
    | (None, _) | (_, None) ->
        None
    | (Some (t1, o1), Some (t2, o2)) ->
        Some (max t1 t2, fct o1 o2)
    (* t1 = t2 if `D else t1 <=> t2 *)
  in
  let push o1 o2 =
    match (o1, o2) with
    | (None, _) | (_, None) ->
        None
    | (Some (t1, o1), Some (t2, o2)) ->
        Some (max t1 t2, fct o1 o2)
  in
  Signal.make2 sig_t e1 e2 definition push

(**
      [previous: sig_t -> o' -> 'o Sig.Event] is the previous occurence of
      the input signal.
      if no occurence has occur yet, the default occurence is used [d_o]
 *)
let previous (type o') sig_t d_o e' =
  let definition t =
    if Time.before_origin (Time.prev t) then Some (Time.origin, d_o)
    else
      let module E' = (val !e' : Sig.Event with type t = o') in
      match E'.definition `C (Time.prev t) with
      | None ->
          Some (Time.origin, d_o)
      | Some (_t', o) ->
          Some (t, o)
  in
  let push _t _o = None (* Some (Time.next t, o) *) in
  Signal.make sig_t e' definition push

(**
     [fix: sig_t -> ((c,'a) Sig.Event -> (c,'a) Sig.Event * 'b) -> (c,'a)
     Sig.Event * 'b] create a fix point using the function define in argument
 *)
let fix (type o) sig_t f =
  let e = Signal.create sig_t in
  let (e', v) = f e in
  let module E' = (val !e' : Sig.Event with type t = o) in
  let module E = (val !e : Sig.Event with type t = o) in
  E'.production := !E.production ;
  e := !e' ;
  (e, v)
