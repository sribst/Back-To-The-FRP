type ('t, 'a) event = ('t, 'a) Sig.event

type signal_type = Sig.signal_type

val make :
  signal_type:signal_type ->
  event:('t, 'a) event ->
  definition:(Time.time -> (Time.time * 'b) option) ->
  push:(Time.time -> 'a -> (Time.time * 'b) option) ->
  ('t, 'b) event

val make2 :
  signal_type:signal_type ->
  event1:('t, 'a) event ->
  event2:('t, 'b) event ->
  definition:(Time.time -> (Time.time * 'c) option) ->
  push:((Time.time * 'a) option ->
       (Time.time * 'b) option ->
       (Time.time * 'c) option) ->
  ('t, 'c) event

type ('t, 'a) signal = ('t, 'a) Sig.signal

val refine : ('t, 'a) event -> Time.t -> 'a -> unit

val observe : ?produce:bool -> ('t, 'a) event -> Time.t -> 'a

val create : signal_type -> ('t, 'a) event

val empty : ('t, 'a) event -> unit

val print_value : ('t, 'a) event -> ('a -> string) -> string -> unit

val print_time : ('t, 'a) event -> string -> unit

val get_vl_list : ('t, 'a) event -> (int * bool) list
