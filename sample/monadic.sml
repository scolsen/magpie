(* A monadic implementation of the packrat parser presented in parser.sml. *)

(* While we can make do with case statements, their repeated use soon 
* becomes tedious. Many of the case matchings we need to perform can be
* abstracted into common patterns. Monads with alternation provide combinators
* that effectively abstract the notion of choice from our computations and
* enable us to combine parsers. *)

(* First, we define a minimal monad structure. *)
signature MONAD =
  sig
    type 'a m
    val return : 'a -> 'a m
    val bind : 'a m * ('a -> 'b m) -> 'b m
  end

(* In order to implement choice in our monadic computations we provide an
 * 'alternative' signature, named Choice. Given a monadic structure, a choice
 * functor defines selection over its computations. 
 * Note that this definition of choice is general over applicative or monad. *)
signature CHOICE =
  sig
    type 'a c
    val empty  : 'a c
    val choose : 'a c -> 'a c -> 'a c
  end 

(* Monad plus just a monad with choice. *)
signature MONADPLUS = 
  sig
    structure M : MONAD
    structure C : CHOICE
    sharing type M.m = C.c
  end

(* The parser data type will wrap a function that takes some concept of a
 * grammar, such as Ford's derivations, and returns a result. So, we need to
 * define the notion of a parser result. *)
datatype ('a, 'b) result = Success of 'a * 'b
                         | Failure

(* We need a parser data type, which we'll make monadic.
 *  It wraps a function which, given a representation of the grammar, returns a
 *  result of attempting to parse that grammar.*)
datatype ('a, 'b) parser = Parser of ('a -> ('a, 'b) result)

(* Because of the way type parameters work in SML we need an additional
 * signature for grammars. We are forced to implement the monadic parser as a
 * functor over the provided grammar structure. *)
signature GRAMMAR =
  sig
    type g
    val raw : g -> (char, g) result
  end


functor ParserPlus(structure G : GRAMMAR) : MONADPLUS =
  struct
    structure G = G 
       
    structure M : MONAD =
      struct
        type 'a m = ('a, G.g) parser
        
        fun return (x) = Parser (fn (y) => Success (x, y))
        fun bind (Parser f, g) 
          = let
              fun parse (x) 
                : ('a -> ('a, 'b) result) 
                = (case f x of 
                        Success (x', y') => fn (y) => Success (x', g y')  
                      | Failure => fn (y) => Failure)
            in
              Parser parse 
            end
      end

    structure C : CHOICE =
      struct
        type 'a c = ('a, G.g) parser
        
        val empty = Parser (fn (x) => Failure)
      end
  end
