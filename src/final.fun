functor FinalPrettyPrinter (Kit : FPP_BASIS) : FPP = 
struct
  open Kit
  type 'a m = 'a Kit.Monad.m
  type doc = unit m
  
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
    bind m f
  infix 2 >>=

  fun >> (m, n) = 
    m >>= (fn _ => n)
  infix 2 >>

  val <|> = alt
  infixr <|>

  fun <$> (f, m) = 
    m >>= (ret o f)

  infix <$> 

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

  fun makeFlatAndAllowFail m = 
    localEnv
      (fn {maxWidth, maxRibbon, nesting, layout, failure, formatting, formatAnn} =>
        {maxWidth = maxWidth, maxRibbon = maxRibbon, nesting = nesting, layout = FLAT, failure = CAN_FAIL, formatting = formatting, formatAnn = formatAnn})
      m

  fun localNesting f = 
    localEnv
      (fn {maxWidth, maxRibbon, nesting, layout, failure, formatting, formatAnn} =>
        {maxWidth = maxWidth, maxRibbon = maxRibbon, nesting = f nesting, layout = layout, failure = failure, formatting = formatting, formatAnn = formatAnn})

  fun localFormatting f = 
    localEnv
      (fn {maxWidth, maxRibbon, nesting, layout, failure, formatting, formatAnn} =>
        {maxWidth = maxWidth,maxRibbon = maxRibbon, nesting = nesting, layout = layout, failure = failure, formatting = f formatting, formatAnn = formatAnn})

  fun spaceMax (w, w') = 
    if Space.compare (w, w') = GREATER then w else w'
    
  (* TODO: aquaMax ;-) *)

  fun putCurrentLine l : unit m = 
    measure l >>= (fn w => 
      modifyState (fn {maxWidthSeen = w', ...} => {maxWidthSeen = spaceMax (w, w'), curLine = l}))

  fun modifyLine f =
    getState >>= (fn {curLine, ...} => 
      putCurrentLine (f curLine))

  val measureCurrentLine =
    getState >>= 
    (fn {curLine, ...} => measure curLine)

  fun chunk (c : chunk) : unit m = 
    output (ATOM (CHUNK c)) >> askEnv >>= 
    (fn {formatting, failure, maxWidth, maxRibbon, nesting, ...} => 
      modifyLine (fn line => line @ [(c, formatting)]) >>
      (when (failure = CAN_FAIL) @@
        measureCurrentLine >>=
        (fn w =>
          let
            val shouldFail = 
              Space.compare (w, maxWidth) = GREATER
                orelse Space.compare (Space.sum (w, Space.neg nesting), maxRibbon) = GREATER
          in
            when shouldFail (fail ())
          end)))

  fun grouped m = ifFlat m (makeFlatAndAllowFail m <|> m)
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

  val empty = ret ()
  val seq = sequence_
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

  structure Atomic =
  struct
    val equals = char #"="
    fun parens m = char #"(" >> m >> char #")"
    fun braces m = char #"{" >> m >> char #"}"
    fun squares m = char #"[" >> m >> char #"]"
    val colon = char #":"
    val comma  = char #","
  end
end
