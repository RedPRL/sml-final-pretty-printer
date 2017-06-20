functor FppPrecedenceBasis (B : FPP_BASIS where type space = int) : FPP_PRECEDENCE_BASIS = 
struct
  type space = B.space
  type ann = B.ann
  type fmt = B.fmt

  structure Space = B.Space and Fmt = B.Fmt and M = B.Monad

  type prec_env = 
    {level : int, 
     bumped : bool,
     lparen : string * ann option,
     rparen : string * ann option}
  
  val epsilon : prec_env = 
    {level = 0,
     bumped = false,
     lparen = ("(", NONE),
     rparen = (")", NONE)}

  structure Monad =
  struct
    type 'a m = prec_env -> 'a M.m
    type output = M.output
    type state = M.state
    type env = M.env
    type line = M.line

    fun ret a _ = M.ret a
    fun bind m f p = 
      M.bind (m p) (fn a => f a p)

    fun alt (m1, m2) p = 
      M.alt (m1 p, m2 p)
    
    fun fail () _ = 
      M.fail ()

    fun measure x _ =
      M.measure x

    fun output out _ = 
      M.output out

    fun censor f m = 
      M.censor f o m

    fun askEnv _ = 
      M.askEnv

    fun localEnv f m = 
      M.localEnv f o m

    fun getState _ = 
      M.getState

    fun modifyState f _ = 
      M.modifyState f

    fun localLevel f m {level, bumped, lparen, rparen} = 
      m {level = f level, bumped = bumped, lparen = lparen, rparen = rparen}

    fun localBumped f m {level, bumped, lparen, rparen} = 
      m {level = level, bumped = f bumped, lparen = lparen, rparen = rparen}

    fun askPrec p : prec_env M.m = 
      M.ret p

  end

  type 'a infixer = int -> 'a Monad.m -> 'a Monad.m -> 'a Monad.m -> 'a Monad.m

  local
    open Monad
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

    fun @@ (f, x) = f x
    infixr 0 @@

    fun lift m _ = m

    structure Fpp = FinalPrettyPrinter (B)
  in
    fun botLevel m =
      localLevel (fn _ => 0) (localBumped (fn _ => false) m)
    
    fun bump m = localBumped (fn _ => true) m

    fun closed lm m rm : 'a m= 
      lm >> botLevel m >> rm

    fun parens (m : 'a m) : 'a m =
      closed
        (fn {lparen = (lp, SOME ann),...} => Fpp.annotate ann (Fpp.text lp) | {lparen = (lp, NONE),...} => Fpp.text lp)
        (fn {rparen = (rp, SOME ann),...} => Fpp.annotate ann (Fpp.text rp) | {rparen = (rp, NONE),...} => Fpp.text rp)
        m

    fun atLevel i' (m : 'a m) : 'a m = 
      askPrec >>=
      (fn {level = i, bumped, ...} => 
        let
          val m' = localLevel (fn _ => i') (localBumped (fn _ => false) m)
        in
          if i < i' orelse (i = i' andalso not bumped) then 
            m'
          else
            parens m'
        end)

    fun inf i mOp m1 m2 = 
      atLevel i @@ 
        bump m1 >>
        lift (Fpp.space 1) >>
        mOp >>
        lift (Fpp.space 1) >>
        bump m2

    fun infl i mOp m1 m2 = 
      atLevel i @@
        m1 >> 
        lift (Fpp.space 1) >>
        mOp >> 
        lift (Fpp.space 1) >>
        bump m2

    fun infr i mOp m1 m2 = 
      atLevel i @@
        bump m1 >> 
        lift (Fpp.space 1) >>
        mOp >> 
        lift (Fpp.space 1) >>
        m2

    fun app m ms = 
      atLevel 100 (fn p => Fpp.hvsep (m p :: List.map (Fpp.align o (fn m => bump m p)) ms))
  end
end