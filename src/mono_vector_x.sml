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
    open EasyLoop
    val filter = Option.filter

    fun vector (n, x) = V.tabulate (n, k x)

    fun findi_r f v =
        downfrom_until (fn i => filter f (i, v //! i)) $ V.length v

    fun find_r f v =
        downfrom_until (fn i => filter f (v //! i)) $ V.length v

    fun append (v1, v2) = V.concat [v1, v2]

    fun to_list v = V.foldr (op ::) [] v

    fun collate_r f (a1, a2) = 
        let val ls = (V.length a1, V.length a2)
            fun check (i, j) =
                case f (a1 //! i, a2 //! j) of EQUAL => NONE | ord => SOME ord
        in getOpt (downfrom_until2 check ls, Int.compare ls) end

    fun existsi f v = isSome $ findi f v 

    fun alli f v = not $ isSome $ findi (not o f) v

end

structure BoolVectorX = MonoVectorX (BoolVector)

structure IntVectorX = MonoVectorX (IntVector)

structure RealVectorX = MonoVectorX (RealVector)

structure CharVectorX = MonoVectorX (CharVector)

structure Word8VectorX = MonoVectorX (Word8Vector)
