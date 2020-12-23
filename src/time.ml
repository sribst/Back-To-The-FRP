type time = int

type t = time

let origin = 0

(* let infinity = max_int *)
let next = succ

let prev = pred

let ( = ) = ( = )

let before_origin x = x < origin

let of_string = int_of_string

let to_string = string_of_int

let of_int t = (t : time)

let to_int t = (t : int)

module Timemap = Map.Make (struct
  type t = time

  let compare x y = Int.compare x y
end)
