signature FPP_BASIS_TYPES = 
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
end

signature FPP_BASIS =
sig
  include FPP_BASIS_TYPES

  structure Monad : 
  sig
    type 'a m

    val ret : 'a -> 'a m
    val bind : 'a m -> ('a -> 'b m) -> 'b m

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
