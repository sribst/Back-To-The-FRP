(*
  /!\ TODO comment all the code
  Comment not enough explicit and easy to understand !
 *)

module Time = Time
include Sig

(**
    module D is one of the two module for the client interface.
    It contain all the function to create Event producing Dicrete
    Signal

    that module doesn't define anything interesting.
    It's just calling the function of [module E] with [`D]

    Not all function of the [module E] are define here,
    for exemple [E.complete] and [E.complete_default] can only be call
    to create an event producing a Continuous Signal
 *)
module Discrete = struct
  let create () = Signal.create `D

  let map fct e' = Event.map `D fct e'

  (* let map2 _name fct e1 e2 =
   *   Event.map2 `D fct e1 e2 *)
end

(**
    module C is one of the two module for the client interface.
    It contain all the function to create Event producing Continuous
    Signal

    that module doesn't define anything interesting.
    It's just calling the function of [module E] with [`C]

    [complete] and [complete_default] can only be call with an event
    producing a discrete Signal.
    That verification is done by the signature with polymorph type
 *)
module Continuous = struct
  let create () = Signal.create `C

  let map (type o' o) (fct : o' -> o) e' = Event.map `C fct e'

  let complete (* (type o')  *) e' = Event.complete `C e'

  let complete_default (* (type o')  *) e' d_o =
    Event.complete_default `C e' d_o

  let map2 (type o1 o2 o) (fct : o1 -> o2 -> o) e1 e2 = Event.map2 `C fct e1 e2

  let previous (* (type o')  *) d_o e' = Event.previous `C d_o e'

  let fix f = Event.fix `C f
end
