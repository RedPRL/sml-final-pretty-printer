structure FppPlainBasisTypes : FPP_BASIS_TYPES = 
struct
  type space = int
  type ann = unit
  type fmt = unit

  structure Space =
  struct
    val compare = Int.compare
    fun sum (i, j) = i + j
    fun neg i = 0 - i
  end

  structure Fmt =
  struct
    val unit = ()
    fun mul _ = ()
  end
end
