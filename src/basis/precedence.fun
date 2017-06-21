functor FppPrecedenceBasis (B : FPP_BASIS) : FPP_PRECEDENCE_BASIS = 
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

    fun lift m _ = m

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

    fun askPrec p : prec_env M.m = 
      M.ret p

  end

  type 'a expr =
    {opr : 'a Monad.m,
     arg1 : 'a Monad.m,
     arg2 : 'a Monad.m}

  type level = int 
  datatype assoc = LEFT | RIGHT | NON_ASSOC

  local
    open Monad

    fun >>= (m, f) = bind m f
    fun >> (m, n) = bind m (fn _ => n)
    infix 2 >>= >>

    fun @@ (f, x) = f x
    infixr 0 @@

    structure Fpp = FinalPrettyPrinter (B)
  in
    fun botLevel m {level, bumped, lparen, rparen} =
      m {level = 0, bumped = false, lparen = lparen, rparen = rparen}
    
    fun bump m {level, bumped, lparen, rparen} =
      m {level = level, bumped = true, lparen = lparen, rparen = rparen}

    fun closed lm m rm : 'a m= 
      lm >> botLevel m >> rm

    fun parens m =
      closed
        (fn {lparen = (lp, SOME ann),...} => Fpp.annotate ann (Fpp.text lp) | {lparen = (lp, NONE),...} => Fpp.text lp)
        (fn {rparen = (rp, SOME ann),...} => Fpp.annotate ann (Fpp.text rp) | {rparen = (rp, NONE),...} => Fpp.text rp)
        m

    fun atLevel i' m = 
      askPrec >>=
      (fn {level = i, bumped, ...} => 
        let
          fun m' {level, bumped, lparen, rparen} = m {level = i', bumped = false, lparen = lparen, rparen = rparen}
        in
          if i < i' orelse (i = i' andalso not bumped) then 
            m'
          else
            parens m'
        end)

    val oneSpace : unit m = 
      lift Fpp.spaceWidth >>= 
        lift o Fpp.space

    fun inf lvl assoc {opr, arg1, arg2} = 
      let
        val (arg1', arg2') = 
          case assoc of 
             LEFT => (arg1, bump arg2)
           | RIGHT => (bump arg1, arg2)
           | NON_ASSOC => (arg1, arg2)
      in
        atLevel lvl @@
          arg1' >>
          oneSpace >>
          opr >> 
          oneSpace >>
          arg2'
      end

    fun app m ms = 
      atLevel 100 (fn p => Fpp.hvsep (m p :: List.map (Fpp.align o (fn m => bump m p)) ms))
  end
end