type signal_type = [`C | `D | `N]

module type Signal = sig
  type t

  val primitive : bool

  val cache : t Cache.t ref

  val interval_time : Interval.t ref

  val observe : bool -> Time.t -> t

  val invalidate : Time.t -> unit

  val definition : signal_type -> Time.t -> (Time.t * t) option

  val refine : Time.t -> Time.t -> t -> unit

  val push : Time.t -> unit
end

type ('t, 'a) signal = (module Signal with type t = 'a) ref

module type Event = sig
  include Signal

  val production : (module Signal) list ref
end

type ('t, 'a) event = (module Event with type t = 'a) ref
