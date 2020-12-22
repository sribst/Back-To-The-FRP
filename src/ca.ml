(**
    [module Ca] save all the occurences of a signal
 *)

module Timemap = Time.Timemap

type 'a t = 'a Timemap.t

(* let empty = Timemap.empty *)

let add c t v = Timemap.add t v c

(**
      [occ c t : 'a Timemap.t -> time_t -> (time_t,'a) option] is the
      occurence (if it exist) of the signal associated to the cache
      [c] at the key [t]
 *)
let occ c t = try Some (t, Timemap.find t c) with Not_found -> None

(**
     [last_occ c t : 'a Timemap.t -> time_t -> (time_t,'a) option] is
     the occurence (if it exist) of the signal associated to the cache
     [c] at the key (max {t'| t'<=[t]})
 *)
let last_occ c t =
  let (l, v_o, _r) = Timemap.split t c in
  match v_o with
  | Some v ->
      Some (t, v)
  | None ->
      if Timemap.is_empty l then None else Some (Timemap.max_binding l)

(** {2 DEBUG FUNCTION} *)

(** [print c f n : 'a Timemap.t -> ('a -> string) -> string -> unit] print all
   the value of [c] using the function [f] [n] is a name given by the user
   (mostly use to debug and quickly identify the Signal) *)

let print c f n =
  Printf.printf "\nValue %s :\n%!" n ;
  Timemap.iter
    (fun i v -> Printf.printf "| %d : %s %!" (Time.to_int i) (f v))
    c ;
  Printf.printf "|\n\n%!"
