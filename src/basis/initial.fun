(* based on https://github.com/purescript/purescript-transformers/blob/master/src/Control/Monad/RWS/Trans.purs *)

(* TODO: turn this into a monad transformer... *)
functor FppInitialBasis (Types : FPP_BASIS_TYPES where type space = int) : FPP_BASIS = 
struct
  open FppTypes
  open Types

  structure Monad = 
  struct
    type output = (space, ann) output
    type state = (space, fmt) state
    type env = (space, ann, fmt) env
    type line = (space, fmt) line

    type 'a result =
      {state: state,
       result: 'a option,
       output: output}

    type 'a m = env -> state -> 'a result

    fun ret a _ s = 
      {state = s,
       result = SOME a,
       output = NULL}

    fun bind m f (env : env) (state : state) = 
      let
        val {state = state', result, output} = m env state
      in
        case Option.map f result of 
           NONE => {state = state', result = NONE, output = output}
         | SOME m' => 
           let
             val {state = state'', result = result', output = output'} = m' env state'
           in
             {state = state'',
              result = result',
              output = SEQ (output, output')}
           end
      end

    fun fail () _ s =
      {state = s,
       result = NONE,
       output = NULL}

    fun alt (m1, m2) e s = 
      case m1 e s of 
         r as {state, result = SOME _, output} => r
       | _ => m2 e s

    local 
      val chunkLength = 
        fn TEXT t => String.size t
         | SPACE w => w
    in
      val measure : line -> space m =
        ret o foldl (fn ((c, _), n) => chunkLength c + n) 0
    end

    fun output out _ s =
      {state = s, 
       result = SOME (),
       output = out}

    fun censor f m e s =
      let
        val {state, result, output} = m e s
      in
        {state = state,
         result = result,
         output = f output}
      end

    fun askEnv e s =
      {state = s,
       result = SOME e,
       output = NULL}

    fun localEnv f m =
      m o f

    fun getState _ s =
      {state = s,
       result = SOME s,
       output = NULL}

    fun modifyState f e s =
      {state = f s,
       result = SOME (),
       output = NULL}
  end
end