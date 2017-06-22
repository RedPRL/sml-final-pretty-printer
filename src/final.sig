signature FPP = 
sig
  type space
  type ann
  type 'a m

  val ret : 'a -> 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m

  val grouped : 'a m -> 'a m
  val text : string -> unit m
  val char : char -> unit m
  val space : space -> unit m
  val hardLine : unit m
  val newline : unit m
  val nest : space -> 'a m -> 'a m

  val ifFlat : 'a m -> 'a m -> 'a m
  val align : 'a m -> 'a m
  val annotate : ann -> 'a m -> 'a m

  val empty : unit m
  val seq : unit m list -> unit m

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


  (* convenience *)
  structure Atomic :
  sig
    val equals : unit m
    val parens : unit m -> unit m
    val braces : unit m -> unit m
    val squares : unit m -> unit m
    val colon : unit m
    val comma : unit m
  end
end