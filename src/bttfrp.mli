(*---------------------------------------------------------------------------
   Copyright (c) 2016 Sylvain Ribstein. All rights reserved.
   Distributed under a BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(*
/!\ DESCRIBE how BTTFRP works
/!\ COMMMENT the interface
 *)

(**
    Back To The FRP

    Travail de Recherche Encadré Master 1 MPRI 2015/2016
    Sylvain Ribstein
    Université Paris 7 - Diderot
 **)

(** Back To The FRP

    BTTFRP est un module créé pour la programation reactive
    fonctionel.
    Il supporte les signaux discrets et continues, et se combine via
    des évènements.
    L'ensemble des évènements classiques y sont définit (map, ...).

    BTTFRP se distingue en laissant la gestion du temps au client.
    Ainsi il est possible de modifier la valeur d'un signal à un temps
    passé sans avoir à reenvoyer l'ensemble du flow, ou prévoir sa
    valeur à un temps futur.

    Veuillez consulter la sémantique, et les exemples basiques.

    Pour utiliser BTTFRP il suffit d'ouvrir le module qui définie 4 types
    -S : Signal (signal discret ou continue produit par un évènement)
    -D : Discrete (évènement produisant un signal Discret)
    -C : Continuous (évènement produisant un signal Continue)
    -T : Time (Temps de la timeline des signaux)

    Pour voir l'ensemble des possibilités veuillez vous referer au
    sous-parties si dessus.

 **)

(** {1 Interface} *)

(** type du temps ( <=> Int) *)
type time_t

(** Interface du type T (Temps)  *)
module T : sig
  (** {1 création}*)

  val origin : time_t
  (** Origine du temps ( <=> 0).*)

  val next : time_t -> time_t
  (**[next t] le temps suivant.*)

  val prev : time_t -> time_t
  (**[previous t] le temps precedent.*)

  (** {1 égalité}*)

  val (=)  : time_t -> time_t -> bool
  (**[t = t']: égalité de t et t'*)

  val before_origin : time_t -> bool
  (**[before_origin t] t est en dehors du temps ([t < origin]) *)

  (** {1 cast}*)

  val of_string : string -> time_t
  (**[of_string s] : temps égal à la valeur de l'entier dans le string
  [s]*)

  val to_string : time_t -> string
  (**[to_string t] : string contenant un entier égal au temps
  renseigné *)

  val of_int   : int    -> time_t
  (**[of_int t]: temps égal à la valeur de [t] *)

  val to_int   : time_t -> int
                             (**[to_int t]: int égal à la valeur de [t] *)
end

type ('t,'a) t
(** type of event *)

type d
(** phantom type of discretes signals *)

type c
(** phantom type of continuous signals *)

(** Signal

Lien : {{!sigsem}semantics} de BTTFRP *)
module S : sig
  (** {1 Valeur du signal selon le temps} *)

  val refine  : ('t,'a) t -> time_t -> 'a -> unit
  (**
      [refine e t o]
      raffine le signal produit par l'évènement [e] par une occurence
      de valeur [o] au temps [t]

      [refine] ne peut etre utiliser (pour le moment) que sur des
      évènements primitifs (cf lien pour primitif dans semantique)

      si [e] n'est pas une primitive raise [Not_primitive]
   *)

  val observe : ?produce:bool -> ('t,'a) t -> time_t -> 'a
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

  val empty    : ('t,'a) t -> unit
  (** [empty e] supprime toutes les occurences de e*)

  (** {1 debug function}*)

  val print_value      : ('t,'a) t -> ('a -> string ) -> string -> unit

  val print_time       : ('t,'a) t -> string -> unit

  val get_vl_list      : ('t,'a) t -> (int * bool) list

end

(** discrete event *)
module D : sig

  val create  : unit -> (d,'a) t
  (**
      [create ()] primitive event producing a signal of discrete occurence.

      the occurence are refine by the client using {{!S}S.refine}
   *)

  val map     :  ('a -> 'b ) -> (d,'a) t -> (d,'b) t
(**
      [map fct e] apply [fct] to [e] occurences
 *)

end


(**
    continuous event
 *)
module C : sig

  val create   : unit -> (c,'a) t
  (**
     [create ()] is a primitive event producing continuous occurence
   *)

  val map     :  ('a -> 'b ) -> (c,'a) t -> (c,'b) t
  (**
      [map fct e] apply [fct] to [e] occurences
   *)

  val map2     : ('a -> 'b -> 'c ) -> (c,'a) t -> (c,'b) t -> (c,'c) t
  (**
      [map2 fct e1 e2] apply [fct] to [e1] [e2] occurences
   *)

  val complete : (d,'a) t -> (c,'a) t
  (** [complete e] transforme [e] discrete occurence to continuous occurence

[e] must be a discrete combinator*)

  val complete_default : (d,'a) t -> 'a -> (c,'a) t
  (** [complete_default e d_o] is [e] occurence or d_o when [e] don't produce an occurence*)

  val previous : 'a -> (c,'a) t -> (c,'a) t
  (** [previous d_o e] the previous occurence of [e] or d_o if [e] hasn't produce an occurence yet*)

  val fix      : ((c,'a) t -> (c,'a) t * 'b) -> (c,'a) t * 'b
                                                             (** [fix f] an event with a fix point, and a additional value*)
end

             (** {1:sem Semantics}
    The following notations are used to give a precise meaning
    to events and signals.

    The time of a timeline can be seen as int, where the origin is 0.
    Any negative int (before the origin) won't be consider.

    {2:sigsem Signal}

    A signal is a discrete or continue time varying value
    generate by an {{!evsem} event}.

    An occurrence is the value of e signal at a a precise time.

    A signal is the function \[\] [: 'a event -> time -> 'a occurrence].
    Litteraly an event [e] mapped to a time [t]
    produce an occurrence [o].
    If [e] don't produce an occurrence at [t] then an error is raised.

    A primitive event produce occurrence defined by the user,
    any other event transform an occurrence of an other event.

    The timeline of a signal is the timeline of occurence produce by
    a primitive event (letting the client define his timeline)

    To modifies occurrence the client is
    given the control of time, letting him define occurrence of primitive event
    at any time.

    That control is given by two notions, refine and observe.

    Refine generate an occurence and observe evaluate an occurence.
    Refine can be use only on primitive event

    {3 Refine}

    [refine] [: 'a event -> time -> 'a -> unit]
    that generates the occurence [o] of a primitive
    event [e] at time [t].

    We write \[[e]\]{_t}[<-o] the refining of [e]
    at time [t] of occurence [o].

    {3 Observe}

    A function [observe] [:'a event -> time -> 'a]
    that evaluate the occurrence [o] produced by [e] at time [t].

    We write \[[e]\]{_t}[->o] the occurence [o]
    produced by [e] at time [t] for a discrete occurrence.

    and \[[e]\]{_<=t}[->o] repectively for a continuous occurrence.


{1:basics Basics}

{1:ex Examples}

              *)
