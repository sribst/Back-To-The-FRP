type t

val empty : t

val valid : Time.t -> t -> Time.t * bool

val clean : t -> t

val invalidate : t -> Time.t -> t

val already_invalidate : t -> Time.t -> bool

val validate : t -> Time.t -> Time.t -> t

val print : t -> string -> unit

val fold : (Time.time -> bool -> 'b -> 'b) -> t -> 'b -> 'b
