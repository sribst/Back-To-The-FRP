type time

type t = time

val origin : time

val next : time -> time

val prev : time -> time

val ( = ) : time -> time -> bool

val before_origin : time -> bool

val of_string : string -> time

val to_string : time -> string

val of_int : int -> time

val to_int : time -> int

module Timemap : sig
  include Map.S with type key = time
end
