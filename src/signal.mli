(** Signal

Lien : {{!sigsem}semantics} de BTTFRP *)

(** {1 Valeur du signal selon le temps} *)

(**
      [refine e t o]
      raffine le signal produit par l'évènement [e] par une occurence
      de valeur [o] au temps [t]

      [refine] ne peut etre utiliser (pour le moment) que sur des
      évènements primitifs (cf lien pour primitif dans semantique)

      si [e] n'est pas une primitive raise [Not_primitive]

      [refine t o] call the refine fonction of [S]

      If [s] is a primitive signal then call [S.refine]
      else @raise Not_primitive
 *)
val refine : ('t, 'a) Sig.t -> Time.t -> 'a -> unit

(**
      [observe ~produce e t]: valeur de l'occurence du signal produit
  par [e] au temps [t]
      {ul
      	  {- Si le signal est inexistant au temps [t] raise [Not_found] }
	  {- [observe] s'utilise sur les évènements discrets et
	     continues}
	  {- [observe e] quand [e] n'est pas un évènement primitive va
	     engendrer l'évaluation de tous les signaux dont [e] dépent}
  	  {- [produce=true] : [observe] propage l'observation du signal
	  produit par [e] dans l'ensemble des évènements dépendant de
  [e]}
      }
 *)
val observe : ?produce:bool -> ('t, 'a) Sig.t -> Time.t -> 'a

(**
      [create : (time_t -> o) -> o event ref]

      Create a event producing a primitive signal.
      This is the only signal that the user can refine.
      That signal differs from other, it doesn't use the interval list
      as all the value that are refine are always valid

      It don't use also the push function as push is used when a new
      value is occured by a signal it depend on search_fct is
      different for discrete and continuous event

      to see comment of function, CF function [make] where it is
      describe
 *)
val create : [`C | `D | `N] -> (module Sig.Event with type t = 'o) ref

(** [empty signal] empty the cache of a signal *)
val empty : ('t, 'a) Sig.t -> unit

(** [make :(time_t -> (time_t * o) option) -> ('o event ref) -> (time_t ->
   (time_t * o) option -> (time_t -> unit) -> 'o event ref]

      [make search_fct e' definition push] is a new signal created by an event
   transforming the signal produce by the event e'

      [definition_fct] define what to do when there is no occurence or occurence
   is outdated

      [push_fct] define what to do when a new occurence is push to that event

      TODO comment all function *)
val make :
  [`C | `D | `N] ->
  (module Sig.Event with type t = ' o') ref ->
  (Time.time -> (Time.time * 'o) option) ->
  (Time.time -> ' o' -> (Time.time * 'o) option) ->
  (module Sig.Event with type t = 'o) ref

(** [make2 sig_t e1 e2 def_fct push_fct] is a new signal created by an event
   transforming two signals *)
val make2 :
  [`C | `D | `N] ->
  (module Sig.Event with type t = 'o1) ref ->
  (module Sig.Event with type t = 'o2) ref ->
  (Time.time -> (Time.time * 'o) option) ->
  ((Time.time * 'o1) option ->
  (Time.time * 'o2) option ->
  (Time.time * 'o) option) ->
  (module Sig.Event with type t = 'o) ref

(** {1 debug function}*)

(** [print_value: 'a Signal -> ('a -> string) -> unit] print all value of the
   signal usgin the function given *)
val print_value : ('t, 'a) Sig.t -> ('a -> string) -> string -> unit

(** [print_time: signal -> unit] print the time valid list of the signal *)
val print_time : ('t, 'a) Sig.t -> string -> unit

(** [get_vl_list: Signal -> (time_t, bool) list] is the time valid list *)
val get_vl_list : ('t, 'a) Sig.t -> (int * bool) list
