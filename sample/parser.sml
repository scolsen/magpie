(* A sample packrat parser for a simple arithmetic grammar. *)

(* The lazy structure. Evaluate something lazily. *)

structure Lazy =
  struct
    (* Thunks are the minimal requirement for the doubly recursive definition in
     * the parse function.*)
    fun thunk (x : 'a) 
      : (unit -> 'a)
      = fn () => x
  end

open Lazy

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
* represents raw input. 
*
* The functions below obviate the need for an organizing formalism such as the
* monad equipped with alternative. In the majority of cases we can abstract over
* the idea of returning failure on failure. *)
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

fun pMultitive (Derivation d : derivation) 
  : int result
  = (case #primary d of 
          Failure => Failure 
        | Success (x, Derivation d) => 
            (case #raw d of 
                  Failure => Failure
                | Success (#"*", Derivation d') => 
                    (case #multitive d of 
                          Failure => Failure
                        | Success (y, rest) => Success ((x * y), rest))
                | Success (_, _) => Failure))

fun pPrimary (Derivation d : derivation) 
  : int result
  = (case #raw d of 
          Failure => 
            (case #decimal d of 
                  Failure => Failure
                | Success (x, d') => Success (x, d'))
        | Success (#"(", Derivation d') => 
            (case #additive d' of 
                  Failure => Failure
                | Success (x, Derivation d'') => 
                    (case #raw d'' of 
                          Failure => Failure
                        | Success (#")", rest) => Success (x, rest)
                        | Success (_, _) => Failure))
        | Success (_, _) => Failure)

fun pDecimal (Derivation d : derivation) 
  : int result
  = case #raw d of 
         Failure => Failure
       | Success (#"0", d') => Success (0, d')
       | Success (#"1", d') => Success (1, d')  
       | Success (#"2", d') => Success (2, d')
       | Success (#"3", d') => Success (3, d')
       | Success (#"4", d') => Success (4, d')
       | Success (#"5", d') => Success (5, d')
       | Success (#"6", d') => Success (6, d')
       | Success (#"7", d') => Success (7, d')
       | Success (#"8", d') => Success (8, d')
       | Success (#"9", d') => Success (9, d')
       | Success (_, _)     => Failure

(* The parse function ties the recursive knot. 
 * Given our input string, it returns the resulting derivation. 
 * We must construct an initial derivation from the input, following this
 * we must pass the derivation along to our parsing functions. 
 * The definition in SML is a bit more involved than that of Haskell. 
 * In SML, we must define a function in order to handle recursion. *)
fun parse (input : char list)
  : derivation
  = let 
      fun derive () = let
                        val chr = case input of 
                                     nil => Failure
                                   | (c :: s) => Success (c, (parse s))
                        in
                        Derivation { additive  = pAdditive (derive())
                                   , multitive = pMultitive (derive()) 
                                   , primary   = pPrimary (derive())  
                                   , decimal   = pDecimal (derive()) 
                                   , raw       = chr
                                   }
                        end
    in
      derive ()
    end

