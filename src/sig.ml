(**
   module type occurence signal and event are signature for first-class
   module
 *)

(**
   Module type of occurence, this is where the differential part has
   to be implemented

   TODO differential occurence
 *)
module type Occurence = sig
  type t
end

(**
    Module Type of a Signal
    function to interact with the signal

 *)
module type Signal = sig
  include Occurence

  (**
    [primitive] indicate if that signal will receive it occurence
    from the user/client or transforme occurence from another signal
   *)
  val primitive : bool

  (**
     cache holds all the value of the signal
   *)
  val cache : t Time.Timemap.t ref

  (**
     interval_time hold boolean to indicate if the time (key here)
     is not valid anymore and need to be check
   *)
  val interval_time : bool Time.Timemap.t ref

  (**

   *)
  val observe : bool -> Time.t -> t

  val invalidate : Time.t -> unit

  val definition : [`N | `D | `C] -> Time.t -> (Time.t * t) option

  val refine : Time.t -> Time.t -> t -> unit

  val push : Time.t -> unit
end

(**
 Type of Event
 *)
module type Event = sig
  include Signal

  val production : (module Signal) list ref
end

type 'a event = (module Event with type t = 'a)

(* type sig_t = Discrete | Continuous *)

type discrete

type continuous

type ('t, 'a) t = 'a event ref
