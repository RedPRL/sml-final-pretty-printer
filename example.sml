structure FppBasis = FppPrecedenceBasis (FppInitialBasis (FppPlainBasisTypes))
structure Fpp = FinalPrettyPrinter (FppBasis)
structure Render = FppRenderPlainText

structure Example = 
struct
  datatype tm = VAR of string | LAM of string * tm | APP of tm * tm

  open FppBasis Fpp

  fun @@ (f, x) = f x
  infixr 0 @@

  local 
    val initialEnv =
      {maxWidth = 80,
       maxRibbon = 60,
       layout = FppTypes.BREAK,
       failure = FppTypes.CANT_FAIL,
       nesting = 0,
       formatting = (),
       formatAnn = fn _ => ()}
  in
    fun execPP (m : unit m)  = 
      #output @@ m emptyPrecEnv initialEnv {curLine = []}
  end

  val rec ppTm : tm -> unit m = 
    fn VAR x => text x
     | LAM (x, t) =>
         grouped o atLevel 10 o nest 2 o hvsep @@ 
           [hsep [text "lam", text "x", char #"."],
            ppTm t]
     | APP (t1, t2) => 
         app (ppTm t1) [ppTm t2]

  val example = LAM ("x", APP (VAR "x", APP (VAR "x", VAR "x"))) 
  val () = Render.render TextIO.stdOut o execPP @@ ppTm example
end