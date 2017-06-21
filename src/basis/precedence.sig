signature FPP_PRECEDENCE_BASIS = 
sig
  include FPP_BASIS

  type expr =
    {opr : unit Monad.m,
     arg1 : unit Monad.m,
     arg2 : unit Monad.m}

  type level = int 
  datatype assoc = LEFT | RIGHT | NON_ASSOC

  type prec_env =
    {level : level, 
     bumped : bool,
     lparen : string * ann option,
     rparen : string * ann option}

  val emptyPrecEnv : prec_env

  val inf : level -> assoc -> expr -> unit Monad.m

  val atLevel : int -> unit Monad.m -> unit Monad.m
  val botLevel : unit Monad.m -> unit Monad.m
  val app : unit Monad.m -> unit Monad.m list -> unit Monad.m
end
