type 'a t = 'a Time.Timemap.t

val add : 'a t -> Time.t -> 'a -> 'a t

val occ : 'a t -> Time.t -> (Time.t * 'a) option

val last_occ : 'a t -> Time.t -> (Time.t * 'a) option

val print : 'a t -> ('a -> string) -> string -> unit
