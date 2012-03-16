(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

functor MonoVectorX (V: MONO_VECTOR) :> MONO_VECTOR_X
    where type elem = V.elem
    where type vector = V.vector = struct

    open V
    val op //! = V.sub 
    infix 8 //!
    open IterateX
    val filter = Option.filter

    fun vector (n, x) = V.tabulate (n, konst x)

    fun rfindi f v =
        downfromUntil (fn i => filter f (i, v //! i)) $ V.length v

    fun rfind f v =
        downfromUntil (fn i => filter f (v //! i)) $ V.length v

    fun append (v1, v2) = V.concat [v1, v2]

    fun toList v = V.foldr (op ::) [] v

    fun rcollate f (a1, a2) = 
        let val ls = (V.length a1, V.length a2)
            fun check (i, j) =
                case f (a1 //! i, a2 //! j) of EQUAL => NONE | ord => SOME ord
        in getOpt (downfromUntil2 check ls, Int.compare ls) end

    fun existsi f v = isSome $ findi f v 

    fun alli f v = not $ isSome $ findi (not o f) v

    fun concatWith sep vs = concat $ ListX.intersperse sep vs

    fun translate f v = concat $ List.map f (toList v)

end
