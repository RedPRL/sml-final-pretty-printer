structure FppTypes = 
struct
  (* Strings or horizontal space to be displayed *)
  datatype 'w chunk = 
     (* An atomic string. Should not contain formatting spaces or newlines.
       Semantic/object-level spaces OK, but not newlines. *)
     TEXT of string
     (* An amount of horizontal space to insert. *)
   | SPACE of 'w

  (* Atomic pieces of output from the pretty printer *)
  datatype 'w atom = 
     (* Inclusion of chunks *)
     CHUNK of 'w chunk
     (* Newlines to be displayed *)
   | NEWLINE

  (* Pretty printer output represents a single annotated string *)
  datatype ('w, 'ann) output = 
     (* The empty output *)
     NULL
     (* Atomic output *)
   | ATOM of 'w atom
     (* An annotated region of output *)
   | ANN of 'ann * ('w, 'ann) output
     (* The concatenation of two outputs *)
   | SEQ of ('w, 'ann) output * ('w, 'ann) output

  type ('w, 'fmt) line = ('w chunk * 'fmt) list
  type ('w, 'fmt) state = {curLine : ('w, 'fmt) line}

  datatype layout = FLAT | BREAK
  datatype failure = CAN_FAIL | CANT_FAIL

  type ('w, 'ann, 'fmt) env = 
    {maxWidth : 'w,
     maxRibbon : 'w,
     nesting : 'w,
     layout : layout,
     failure : failure,
     formatting : 'fmt,
     formatAnn : 'ann -> 'fmt}
end
