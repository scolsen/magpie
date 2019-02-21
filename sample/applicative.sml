(* Attempt at an alternative applicative implementation of packrats. *)

infix 4 <*> 

signature APPLICATIVE =
  sig
    type 'a f

    val pure  : 'a -> 'a f
    val apply : ('a -> 'b) f -> 'a f -> 'b f
    val <*>   : ('a -> 'b) f -> 'a f -> 'b f
  end
