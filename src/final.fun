functor FinalPrettyPrinter (Kit : FPP_BASIS) : FPP = 
struct
  type space = Kit.Space.t
  type format = Kit.Fmt.t
  type ann = Kit.Ann.t
  type 'a m = 'a Kit.Monad.m

  open FppTypes

  structure Chunk =
  struct
    type t = space chunk

    val eq = 
      fn (TEXT s1, TEXT s2) => s1 = s2
       | (SPACE w1, SPACE w2) => Kit.Space.eq (w1, w2)
       | _ => false
  end

  structure Atom = 
  struct
    type t = space atom
    val eq = 
      fn (CHUNK c1, CHUNK c2) => Chunk.eq (c1, c2)
       | (NEWLINE, NEWLINE) => true
       | _ => false
  end

  structure Out = 
  struct
    type t = (space, ann) output

    val rec eq = 
      fn (NULL, NULL) => true
       | (ATOM a1, ATOM a2) => Atom.eq (a1, a2)
       | (ANN (ann1, o1), ANN (ann2, o2)) => Kit.Ann.eq (ann1, ann2) andalso eq (o1, o2)
       | (SEQ (l1, r1), SEQ (l2, r2)) => eq (l1, l2) andalso eq (r1, r2)
       | _ => false

    fun map f = 
      fn NULL => NULL
       | ATOM a => ATOM a
       | ANN (ann, out) => ANN (f ann, map f out)
       | SEQ (l, r) => SEQ (map f l, map f r)

    val zer = NULL
    val mul = SEQ
  end

  structure Layout = 
  struct
    datatype t = Flat | Break
    val eq : t * t -> bool = op=
  end

  structure Failure =
  struct
    datatype t = CanFail | CantFail
    val eq : t * t -> bool = op=
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

  fun chunk (c : space chunk) : unit m = 
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