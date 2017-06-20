signature FPP = 
sig
  type space
  type ann
  type 'a m

  type chunk = space FppTypes.chunk
  type atom = space FppTypes.atom
  type output = (space, ann) FppTypes.output

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
