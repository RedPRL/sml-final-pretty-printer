signature FPP_PRECEDENCE_BASIS = 
sig
  include FPP_BASIS

  type 'a infixer = int -> 'a Monad.m -> 'a Monad.m -> 'a Monad.m -> 'a Monad.m

  val inf : 'a infixer
  val infl : 'a infixer
  val infr : 'a infixer

  val atLevel : int -> 'a Monad.m -> 'a Monad.m
  val botLevel : 'a Monad.m -> 'a Monad.m
  val app : unit Monad.m -> unit Monad.m list -> unit Monad.m
end
