(* State monad inspired implementation. *)

datatype 'a result = Success of 'a 
                   | Failure

signature PARSER =
  sig
    type grammar 
    type 'a result  where type 'a = ('a, grammar)
    
    val parse    : grammar -> ('a, grammar) result
    val raw      : grammar -> char result
    val position : grammar -> position
  end

functor Parser(structure Grammar : GRAMMAR) =
  struct
    structure G = Grammar
    
    datatype 'a result = Success of 'a * G.grammar
                       | Failure
    val raw      : grammar -> 'a result
    val position : grammar -> position
  end
