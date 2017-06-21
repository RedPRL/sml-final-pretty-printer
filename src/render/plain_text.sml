structure FppRenderPlainText : FPP_RENDER_PLAIN_TEXT = 
struct
  structure T = FppTypes

  type 'ann output = (int, 'ann) T.output

  fun renderChunk h = 
    fn T.TEXT txt => TextIO.output (h, txt)
     | T.SPACE w => List.app (fn x => TextIO.output (h, x)) (List.tabulate (w, fn _ => " "))

  fun renderAtom h = 
    fn T.CHUNK c => renderChunk h c
     | T.NEWLINE => TextIO.output (h, "\n")

  fun render h = 
    fn T.NULL => ()
     | T.ATOM a =>
         renderAtom h a
     | T.SEQ (out1, out2) => 
         (render h out1;
          render h out2)
     | T.ANN (_, out) => 
         render h out

  val rec toString = 
    fn T.NULL => ""
     | T.ATOM a => toStringAtom a
     | T.SEQ (out1, out2) => toString out1 ^ toString out2
     | T.ANN (_, out) => toString out

  and toStringAtom = 
    fn T.CHUNK c => toStringChunk c
     | T.NEWLINE => "\n"

  and toStringChunk = 
    fn T.TEXT txt => txt
     | T.SPACE w => String.implode (List.tabulate (w, fn _ => #" "))
end