(**
   [module IT](Interval Time) is the set of function to modify the set
   of valid time of a signal

   if a time is valid in the associated Timemap then the value
   associated in the cache of the signal is valid
   [itl] : iterval time list
 *)

module Timemap = Time.Timemap

(* type t = bool Timemap.t *)

(** [valid itl t : bool Timemap.t -> time_t -> bool] is the validity
  if the time [t].
  if true then [t] is a valid time so the occurence at time [t] of the
  associated signal is still valid and can be use
 *)
let valid itl t =
  let (l, b_o, _r) = Timemap.split t itl in
  match b_o with
  | Some b ->
      (t, b)
  | None ->
      if Timemap.is_empty l then (Time.of_int (-1), false)
      else Timemap.max_binding l

(**
      [clean itl] will prevent the list to have 2 consecutive equal
      boolean value

      TODO : Not the best way to do
 *)
let clean itl =
  let f t b =
    let (t_prev, b_prev) = valid itl (Time.prev t) in
    if t_prev < Time.origin || b_prev != b then true else false
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
    let (t', b) = Timemap.max_binding itl in
    (not b) && t' <= t

(**
      [invalidate itl t : bool Timemap.t -> time_t -> bool Timemap.t]
      is [itl] truncate at [t-1] and adding [(t,false)]
 *)
let invalidate itl t =
  if not (already_invalidate itl t) then
    let itl = Timemap.filter (fun t' _ -> t' < t) itl in
    let itl = Timemap.add t false itl in
    clean itl
  else itl

(**
      [validate itl t t'] is [itl] modified so that (at least)
      [t] to [t'] is valid

      3 steps :
      delete all value between t t'
      add (t,true)
      if t' was false then add (t'+1,false)
 *)
let validate itl t t' =
  let (_tb', b') = valid itl t' in
  let itl = Timemap.filter (fun t'' _ -> t'' < t || t'' > t') itl in
  let itl = Timemap.add t true itl in
  let itl =
    if (not b') && not (Timemap.mem (Time.next t') itl) then
      Timemap.add (Time.next t') false itl
    else itl
  in
  clean itl

(**
     [printf itl name] print all valid time of the signal with name 'name'
 *)
let print itl name =
  let to_str t b acc =
    Time.to_string t ^ " : " ^ string_of_bool b ^ " | " ^ acc
  in
  let str = Timemap.fold to_str itl "" in
  Printf.printf "time %s : \n| %s\n" name str
