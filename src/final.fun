functor FinalPrettyPrinter (Kit : FPP_BASIS) : FPP = 
struct
  open Kit
  type 'a m = 'a Kit.Monad.m

  open FppTypes

  type chunk = space FppTypes.chunk
  type atom = space FppTypes.atom
  type output = (space, ann) FppTypes.output

  structure Output = 
  struct
    fun map f = 
      fn NULL => NULL
       | ATOM a => ATOM a
       | ANN (ann, out) => ANN (f ann, map f out)
       | SEQ (l, r) => SEQ (map f l, map f r)

    val zer = NULL
    val mul = SEQ
  end

  open Kit
  open Kit.Monad

  fun >>= (m, f) = 
    bind f m
  infix 2 >>=

  fun >> (m, n) = 
    m >>= (fn _ => n)
  infix 2 >>

  val <|> = alt
  infixr <|>

  fun <$> (f, m) = 
    m >>= (ret o f)

  infix <$> 

  fun <*> (mf, m) = 
    mf >>= (fn f => f <$> m)

  infix <*>

  fun sequence ms = 
    case ms of 
       [] => ret []
     | m::ms => m >>= (fn x => (fn xs => x :: xs) <$> sequence ms)

  fun sequence_ ms = 
    case ms of 
       [] => ret ()
     | m::ms => m >> sequence_ ms

  fun @@ (f, x) = f x
  infixr 0 @@

  fun when b m = 
    if b then m else ret ()

  fun ifFlat flatAction breakAction = 
    askEnv >>= (fn {layout,...} =>
      case layout of 
         FLAT => flatAction
       | BREAK => breakAction)

  fun makeFlat m = 
    localEnv
      (fn {maxWidth, maxRibbon, nesting, layout, failure, formatting, formatAnn} =>
         {maxWidth = maxWidth,
          maxRibbon = maxRibbon,
          nesting = nesting,
          layout = FLAT,
          failure = failure,
          formatting = formatting,
          formatAnn = formatAnn})
      m

  fun allowFail m = 
    localEnv
      (fn {maxWidth, maxRibbon, nesting, layout, failure, formatting, formatAnn} =>
         {maxWidth = maxWidth,
          maxRibbon = maxRibbon,
          nesting = nesting,
          layout = layout,
          failure = CAN_FAIL,
          formatting = formatting,
          formatAnn = formatAnn})
      m

  fun localNesting f m = 
    localEnv
      (fn {maxWidth, maxRibbon, nesting, layout, failure, formatting, formatAnn} =>
         {maxWidth = maxWidth,
          maxRibbon = maxRibbon,
          nesting = f nesting,
          layout = layout,
          failure = failure,
          formatting = formatting,
          formatAnn = formatAnn})
      m

  fun localFormatting f m = 
    localEnv
      (fn {maxWidth, maxRibbon, nesting, layout, failure, formatting, formatAnn} =>
         {maxWidth = maxWidth,
          maxRibbon = maxRibbon,
          nesting = nesting,
          layout = layout,
          failure = failure,
          formatting = f formatting,
          formatAnn = formatAnn})
      m


  fun modifyLine f = 
    modifyState 
      (fn {curLine} => {curLine = f curLine})


  fun putCurrentLine l = 
    modifyLine (fn _ => l)

  val measureCurrentLine =
    getState >>= 
    (fn {curLine} => measure curLine)

  fun chunk (c : chunk) : unit m = 
    output (ATOM (CHUNK c)) >> askEnv >>= 
    (fn {formatting,...} => 
      modifyLine (fn line => line @ [(c, formatting)]) >>
      askEnv) >>=
    (fn {failure, maxWidth, maxRibbon, nesting, ...} => 
      when (failure = CAN_FAIL) @@
        measureCurrentLine >>=
        (fn w =>
           let
             val width = Space.sum (nesting, w)
             val shouldFail = 
               Space.compare (width, maxWidth) = GREATER
                 orelse Space.compare (w, maxRibbon) = GREATER
           in
             when shouldFail (fail ())
           end))

  fun grouped m = ifFlat m (makeFlat (allowFail m) <|> m)
  val text = chunk o TEXT 
  val char = text o Char.toString
  val space = chunk o SPACE

  val hardLine = 
    output (ATOM NEWLINE) >>
    putCurrentLine []

  val newline = 
    askEnv >>=
    (fn {nesting, ...} => 
      hardLine >>
      space nesting)

  fun nest w = 
    localNesting
      (fn w' => Space.sum (w, w'))

  fun align m =
    askEnv >>= 
    (fn {nesting,...} => 
      measureCurrentLine >>= 
      (fn w => 
        nest (Space.sum (w, Space.neg nesting)) m))

  fun annotate ann m =
    askEnv >>= 
    (fn {formatAnn,...} =>
      localFormatting
        (fn fmt => Fmt.mul (formatAnn ann, fmt))
        (censor (fn out => ANN (ann, out)) m))

  fun intersperse s xs = 
    case xs of 
      [] => []
     | [x] => [x]
     | x::xs => x :: s :: intersperse s xs

  val hsep = sequence_ o intersperse (text " ")
  val vsep = sequence_ o intersperse newline

  fun measureText txt =
    askEnv >>=
    (fn {formatting,...} => 
      measure [(TEXT txt, formatting)])

  val spaceWidth = measureText " "
  val emWidth = measureText "M"

  fun hvsep ms =
    spaceWidth >>= 
    (fn i => 
      grouped
        @@ sequence_
        @@ intersperse (ifFlat (space i) newline) 
        @@ ms)

  fun hsepTight ms =
    spaceWidth >>= 
    (fn i => 
      sequence_
        @@ intersperse (ifFlat (ret ()) (space i))
        @@ ms)

  val hvsepTight =
    grouped
      o sequence_
      o intersperse (ifFlat (ret ()) newline)

  fun collection ldelim rdelim sep =
    fn [] => ldelim >> rdelim
     | m::ms =>
       grouped o hvsepTight o List.concat @@
         [[hsepTight [ldelim, align m]],
          List.map (fn m' => hsep [sep, align m']) ms,
          [rdelim]]

  fun expr m =
    align (grouped m)
end