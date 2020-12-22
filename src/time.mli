type time

type t = time

(** {1 création}*)

(** Origine du temps ( <=> 0).*)
val origin : time

(**[next t] le temps suivant.*)
val next : time -> time

(**[previous t] le temps precedent.*)
val prev : time -> time

(** {1 égalité}*)

(**[t = t']: égalité de t et t'*)
val ( = ) : time -> time -> bool

(**[before_origin t] t est en dehors du temps ([t < origin]) *)
val before_origin : time -> bool

(** {1 cast}*)

(**[of_string s] : temps égal à la valeur de l'entier dans le string
  [s]*)
val of_string : string -> time

(**[to_string t] : string contenant un entier égal au temps
  renseigné *)
val to_string : time -> string

(**[of_int t]: temps égal à la valeur de [t] *)
val of_int : int -> time

(**[to_int t]: int égal à la valeur de [t] *)
val to_int : time -> int

module Timemap : sig
  include Map.S with type key = time
end
