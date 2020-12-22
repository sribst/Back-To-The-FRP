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

(** Interface du type T (Temps)  *)
module Time : sig
  (** type du temps ( <=> Int) *)
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
end

(** type of event *)
type ('t, 'a) t

(** phantom type of discretes signals *)
type discrete

(** phantom type of continuous signals *)
type continuous

(** discrete event *)
module Discrete : sig
  (**
      [create ()] primitive event producing a signal of discrete occurence.

      the occurence are refine by the client using {{!S}S.refine}
   *)
  val create : unit -> (discrete, 'a) t

  (**
      [map fct e] apply [fct] to [e] occurences
   *)
  val map : ('a -> 'b) -> (discrete, 'a) t -> (discrete, 'b) t
end

(**
    continuous event
 *)
module Continuous : sig
  (**
     [create ()] is a primitive event producing continuous occurence
   *)
  val create : unit -> (continuous, 'a) t

  (**
      [map fct e] apply [fct] to [e] occurences
   *)
  val map : ('a -> 'b) -> (continuous, 'a) t -> (continuous, 'b) t

  (**
      [map2 fct e1 e2] apply [fct] to [e1] [e2] occurences
   *)
  val map2 :
    ('a -> 'b -> 'c) ->
    (continuous, 'a) t ->
    (continuous, 'b) t ->
    (continuous, 'c) t

  (** [complete e] transforme [e] discrete occurence to continuous occurence

[e] must be a discrete combinator*)
  val complete : (discrete, 'a) t -> (continuous, 'a) t

  (** [complete_default e d_o] is [e] occurence or d_o when [e] don't produce an occurence*)
  val complete_default : (discrete, 'a) t -> 'a -> (continuous, 'a) t

  (** [previous d_o e] the previous occurence of [e] or d_o if [e] hasn't produce an occurence yet*)
  val previous : 'a -> (continuous, 'a) t -> (continuous, 'a) t

  (** [fix f] an event with a fix point, and a additional value*)
  val fix :
    ((continuous, 'a) t -> (continuous, 'a) t * 'b) -> (continuous, 'a) t * 'b
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
