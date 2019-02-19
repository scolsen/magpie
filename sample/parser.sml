(* A sample packrat parser for a simple arithmetic grammar. *)

(* We're attempting to use the packrat methodology to parse the following
* PEG grammar: 
* Additive  <- Multitive + Additive | Multitive 
* Multitive <- Primary * Multitive | Primary 
* Primary   <- ( Additive ) | Decimal 
* Decimal   <- 0 | .. | 9 *)



(* The start of the recursive knot -- mutually recursive datatypes 
* derivations of the grammar and a parser result. A derivation must contain a
* result and a result must point to further derivations if it succeeds. 
* I have chosen to use Success and Failure constructors here instead of Parse
* and NoParse, which Ford uses. *)
datatype derivation = Derivation of  { additive  : int result
                                     , multitive : int result
                                     , primary   : int result
                                     , decimal   : int result
                                     , raw       : char result
                                     }
     and 'a result = Success of 'a * derivation
                   | Failure

(* Definitions of parsing functions. These must all take a derivation and return
* some kind of result. The number of parsing functions is equivalent to the
* number of fields in the derivations record, sans the raw field which
* represents raw input. *)
fun pAdditive (Derivation d : derivation)
  : int result
  = (case #multitive d of
         Failure => Failure
       | Success (x, Derivation d) => 
           (case #raw d of
                Failure => Failure
              | Success (#"+", Derivation d') =>
                  (case #additive d' of 
                       Failure => Failure
                     | Success (y, rest) => Success ((x + y), rest))
              | Success (_, _) => Failure))
