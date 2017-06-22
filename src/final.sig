signature FPP = 
sig
  type space
  type ann
  type 'a m
  type doc = unit m

  val ret : 'a -> 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m

  val grouped : 'a m -> 'a m
  val text : string -> doc
  val char : char -> doc
  val space : space -> doc
  val hardLine : doc
  val newline : doc
  val nest : space -> 'a m -> 'a m

  val ifFlat : 'a m -> 'a m -> 'a m
  val align : 'a m -> 'a m
  val annotate : ann -> 'a m -> 'a m

  val empty : doc
  val seq : doc list -> doc

  val hsep : doc list -> doc
  val vsep : doc list -> doc
  val measureText : string -> space m
  val spaceWidth : space m
  val emWidth : space m

  val hvsep : doc list -> doc
  val hsepTight : doc list -> doc
  val hvsepTight : doc list -> doc
  val collection : doc -> doc -> doc -> doc list -> doc

  val expr : 'a m -> 'a m


  (* convenience *)
  structure Atomic :
  sig
    val equals : doc
    val parens : doc -> doc
    val braces : doc -> doc
    val squares : doc -> doc
    val colon : doc
    val comma : doc
  end
end