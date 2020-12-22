type 'a t

val empty : 'a t

val add : 'a t -> Time.t -> 'a -> 'a t

val occurence : Time.t -> 'a t -> (Time.t * 'a) option

val last_occurence : Time.t -> 'a t -> (Time.t * 'a) option

val filter : (Time.time -> 'a -> bool) -> 'a t -> 'a t

val print : 'a t -> ('a -> string) -> string -> unit
