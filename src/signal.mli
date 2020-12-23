type signal_type = [`C | `D | `N]

module type Sig = sig
  type t

  val observe : Time.t -> Time.t * t

  val push : Time.t -> unit

  val refine : Time.t -> Time.t -> t -> unit

  val definition : signal_type -> Time.t -> (Time.t * t) option

  val primitive : bool

  val cache : t Cache.t ref

  val interval_time : Interval.t ref

  val invalidate : Time.t -> unit
end

type ('t, 'a) signal = (module Sig with type t = 'a) ref

type ('t, 'a) t = ('t, 'a) signal

val create : signal_type -> ('t, 'a) signal

val make :
  signal_type:signal_type ->
  signal:('t, 'a) signal ->
  definition:(Time.time -> (Time.time * 'b) option) ->
  push:(Time.time -> 'a -> (Time.time * 'b) option) ->
  ('t, 'b) signal

val make2 :
  signal_type:signal_type ->
  signal1:('t, 'a) signal ->
  signal2:('t, 'b) signal ->
  definition:(Time.time -> (Time.time * 'c) option) ->
  push:((Time.time * 'a) option ->
       (Time.time * 'b) option ->
       (Time.time * 'c) option) ->
  ('t, 'c) t

val refine : ('t, 'a) signal -> Time.t -> 'a -> unit

val observe : ('t, 'a) signal -> Time.t -> 'a

val empty : ('t, 'a) signal -> unit

val print_value : ('t, 'a) signal -> ('a -> string) -> string -> unit

val print_time : ('t, 'a) signal -> string -> unit

val get_interval_list : ('t, 'a) signal -> (int * bool) list
