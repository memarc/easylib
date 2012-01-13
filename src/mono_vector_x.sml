(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

signature MONO_VECTOR_X = sig

    include MONO_VECTOR

    val vector : int * elem -> vector

    val find_r : (elem -> bool) -> vector -> elem option

    val findi_r : (int * elem -> bool) -> vector -> (int * elem) option

    val append : vector * vector -> vector

    val to_list : vector -> elem list

    val collate_r : (elem * elem -> order) -> vector * vector -> order

    val existsi : (int * elem -> bool) -> vector -> bool

    val alli : (int * elem -> bool) -> vector -> bool

end

functor MonoVectorX (V: MONO_VECTOR) :> MONO_VECTOR_X
    where type elem = V.elem
    where type vector = V.vector = struct

    val k = Skicomb.k
    open V
    val op //! = V.sub 
    infix 8 //!

    fun vector (n, x) = V.tabulate (n, k x)

    fun findi_r f v =
        let fun check (i, a, b) =
                let val p = (i, a)
                in if f p then SOME p else b end
        in V.foldli check NONE v end

    fun find_r f v =
        let fun f' (_, x) = f x
        in 
            case findi_r f' v
             of NONE => NONE
              | SOME (_, x) => SOME x
        end

    fun append (v1, v2) = V.concat [v1, v2]

    fun to_list v = V.foldr (op ::) [] v

    fun collate_r f (a1, a2) = 
        let val (l1, l2) = (V.length a1, V.length a2)
            fun loop (~1, ~1) = EQUAL
              | loop (~1, _) = LESS
              | loop (_, ~1) = GREATER
              | loop (i, j)  = 
                case f (a1 //! i, a2 //! j)
                 of EQUAL => loop (i - 1, j - 1)
                  | ord => ord
        in loop (l1 - 1, l2 - 1) end

    fun existsi f v =
        case findi f v of SOME _ => true | NONE => false

    fun alli f v =
        case findi (not o f) v of SOME _ => false | NONE => true

end

structure BoolVectorX = MonoVectorX (BoolVector)

structure IntVectorX = MonoVectorX (IntVector)

structure RealVectorX = MonoVectorX (RealVector)

structure CharVectorX = MonoVectorX (CharVector)

structure Word8VectorX = MonoVectorX (Word8Vector)
