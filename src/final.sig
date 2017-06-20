structure FppTypes = 
struct
  (* Strings or horizontal space to be displayed *)
  datatype 'w chunk = 
     (* An atomic string. Should not contain formatting spaces or newlines.
       Semantic/object-level spaces OK, but not newlines. *)
     TEXT of string
     (* An amount of horizontal space to insert. *)
   | SPACE of 'w

  (* Atomic pieces of output from the pretty printer *)
  datatype 'w atom = 
     (* Inclusion of chunks *)
     CHUNK of 'w chunk
     (* Newlines to be displayed *)
   | NEWLINE

  (* Pretty printer output represents a single annotated string *)
  datatype ('w, 'ann) output = 
     (* The empty output *)
     NULL
     (* Atomic output *)
   | ATOM of 'w atom
     (* An annotated region of output *)
   | ANN of 'ann * ('w, 'ann) output
     (* The concatenation of two outputs *)
   | SEQ of ('w, 'ann) output * ('w, 'ann) output

  type ('w, 'fmt) line = ('w chunk * 'fmt) list
  type ('w, 'fmt) state = {curLine : ('w, 'fmt) line}

  datatype layout = FLAT | BREAK
  datatype failure = CAN_FAIL | CANT_FAIL

  type ('w, 'ann, 'fmt) env = 
    {maxWidth : 'w,
     maxRibbon : 'w,
     nesting : 'w,
     layout : layout,
     failure : failure,
     formatting : 'fmt,
     formatAnn : 'ann -> 'fmt}
end

signature FPP = 
sig
  type space
  type ann
  type 'a m

  type chunk = space FppTypes.chunk
  type atom = space FppTypes.atom
  type output = (space, ann) FppTypes.output

  structure Output : 
  sig
    val map : ('a -> 'b) -> ('w, 'a) FppTypes.output -> ('w, 'b) FppTypes.output
    val zer : output
    val mul : output * output -> output
  end

  val grouped : 'a m -> 'a m
  val text : string -> unit m
  val char : char -> unit m
  val space : space -> unit m
  val hardLine : unit m
  val newline : unit m
  val nest : space -> 'a m -> 'a m

  val ifFlat : 'a m -> 'a m -> 'a m
  val makeFlat : 'a m -> 'a m
  val allowFail : 'a m -> 'a m
  val align : 'a m -> 'a m
  val annotate : ann -> 'a m -> 'a m

  val hsep : unit m list -> unit m
  val vsep : unit m list -> unit m
  val measureText : string -> space m
  val spaceWidth : space m
  val emWidth : space m

  val hvsep : unit m list -> unit m
  val hsepTight : unit m list -> unit m
  val hvsepTight : unit m list -> unit m
  val collection : unit m -> unit m -> unit m -> unit m list -> unit m

  val expr : 'a m -> 'a m
end

signature FPP_BASIS =
sig
  type space
  type ann
  type fmt

  structure Space :
  sig
    val compare : space * space -> order
    val sum : space * space -> space
    val neg : space -> space
  end

  structure Fmt :
  sig
    val unit : fmt
    val mul : fmt * fmt -> fmt
  end

  structure Monad : 
  sig
    type 'a m

    val ret : 'a -> 'a m
    val bind : ('a -> 'b m) -> 'a m -> 'b m

    val alt : 'a m * 'a m -> 'a m
    val fail : unit -> 'a m


    type output = (space, ann) FppTypes.output
    type state = (space, fmt) FppTypes.state
    type env = (space, ann, fmt) FppTypes.env
    type line = (space, fmt) FppTypes.line

    val measure : line -> space m

    val output : output -> unit m
    val censor : (output -> output) -> 'a m -> 'a m

    val askEnv : env m
    val localEnv : (env -> env) -> 'a m -> 'a m

    val getState : state m
    val modifyState : (state -> state) -> unit m
  end
end