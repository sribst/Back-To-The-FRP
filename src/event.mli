type discrete

type continuous

type ('t, 'a) event = ('t, 'a) Sig.event

module Discrete : sig
  val create : unit -> (discrete, 'a) event

  val map : ('a -> 'b) -> (discrete, 'a) event -> (discrete, 'b) event

  val map2 :
    ('a -> 'b -> 'c) ->
    (discrete, 'a) event ->
    (discrete, 'b) event ->
    (discrete, 'c) event
end

module Continuous : sig
  val create : unit -> (continuous, 'a) event

  val map : ('a -> 'b) -> (continuous, 'a) event -> (continuous, 'b) event

  val map2 :
    ('a -> 'b -> 'c) ->
    (continuous, 'a) event ->
    (continuous, 'b) event ->
    (continuous, 'c) event

  val complete : (discrete, 'a) event -> (continuous, 'a) event

  val complete_default : (discrete, 'a) event -> 'a -> (continuous, 'a) event

  val previous : 'a -> (continuous, 'a) event -> (continuous, 'a) event

  val fix :
    ((continuous, 'a) event -> (continuous, 'a) event * 'b) ->
    (continuous, 'a) event * 'b
end
