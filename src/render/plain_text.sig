signature FPP_RENDER_PLAIN_TEXT = 
sig
  type 'ann output = (int, 'ann) FppTypes.output
  val render : TextIO.outstream -> 'ann output -> unit
end