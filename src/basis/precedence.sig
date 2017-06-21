signature FPP_PRECEDENCE_BASIS = 
sig
  include FPP_BASIS

  type 'a expr =
    {opr : 'a Monad.m,
     arg1 : 'a Monad.m,
     arg2 : 'a Monad.m}

  type level = int 
  datatype assoc = LEFT | RIGHT | NON_ASSOC

  val inf : level -> assoc -> 'a expr -> 'a Monad.m

  val atLevel : int -> 'a Monad.m -> 'a Monad.m
  val botLevel : 'a Monad.m -> 'a Monad.m
  val app : unit Monad.m -> unit Monad.m list -> unit Monad.m
end
