(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

structure VectorX :> VECTOR_X = struct

    structure V = Vector
    open V
    open IterateX
    val filter = Option.filter

    fun vector (n, x) = V.tabulate (n, konst x)

    fun rfindi f v =
        downfromUntil (fn i => filter f (i, v // i)) $ V.length v

    fun rfind f v =
        downfromUntil (fn i => filter f (v // i)) $ V.length v

    fun findAll f v =
        foldr (fn (x, xs) => if f x then x :: xs else xs) [] v

    fun findiAll f v =
        let fun prepend (i, x, xs) =
                let val p = (i, x) in if f p then p :: xs else xs end
        in foldri prepend [] v end

    fun append (v1, v2) = V.concat [v1, v2]

    fun toList v = V.foldr (op ::) [] v

    fun rcollate f (a1, a2) = 
        let val ls = (V.length a1, V.length a2)
            fun check (i, j) =
                case f (a1 // i, a2 // j) of EQUAL => NONE | ord => SOME ord
        in getOpt (downfromUntil2 check ls, Int.compare ls) end

    fun existsi f v = isSome $ findi f v

    fun alli f v = not $ isSome $ findi (not o f) v

    fun concatWith sep vs = concat $ ListX.intersperse sep vs

    fun translate f v = concat $ List.map f (toList v)

end
