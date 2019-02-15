(* Position module to track line numbers and columns. *)

fun tab (x : int) 
  : int = 
  (div (x + 8 - 1) 8) * 8 + 1
  
signature Position =
  sig
    type t
    val next : t * char -> t
    val show : t -> string
    val showRelative : t * t -> string

    val eq : t * t -> bool (* check if positions are equal. *)
  end

structure Position =
  struct
    type position = { file   : string
                    , line   : int
                    , column : int}
    
    fun next ({file=file,line=line,column=column} : position, c : char) =
        case c of 
             #"\n" = {file = file, line = (line + 1), column = 1}
           | #"\t" = {file = file, line = line, column = tab column}
           | _     = {file = file, line = line, column = column + 1} 

    fun show ({file=file,line=line,column=column} : position) 
      : string =
      file ^ (toString line) ^ (toString column)

    fun showRelative (pos : position, pos' :position)
      : string =
      let
        val {file=file,line=line,column=column} = pos
        val {file=file',line=line',column=column'} = pos'
      in
        if file == file'
        then if line == line'
             then show {file=file,line=line,column=column'}
             else show {file=file,line=line',column=column'}
        else show pos
      end
  end
