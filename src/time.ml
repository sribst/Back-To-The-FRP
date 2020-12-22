(** [module T] contain all the function to interact with the time *)

type time = int

type t = time

let origin = 0

(* let infinity = max_int *)
let next = succ

let prev = pred

(* let (>)  = (>) *)
let ( < ) = ( < )

let ( = ) = ( = )

let before_origin x = x < origin

(* let is_origin x = x = origin *)
let of_string = int_of_string

let to_string = string_of_int

let of_int t = (t : time)

let to_int t = (t : int)

(* let min = min *)

(**
      [module Timemap]
      Timemap is a simple map struct using time as key
      A Timemap is use to save occurence of a signal (module Ca)
      and another one to save valid time interval (module IT)
 *)
module Timemap = Map.Make (struct
  type t = time

  let compare x y = Stdlib.compare x y
end)
