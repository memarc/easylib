(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

structure VectorX :> VECTOR_X = struct

    structure V = Vector
    open V
    open VectorSupport
    val filter = Option.filter

    fun vector (n, x) = V.tabulate (n, konst x)

    fun findi_r f v =
        downfrom_until (fn i => filter f (i, v // i)) $ V.length v

    fun find_r f v =
        downfrom_until (fn i => filter f (v // i)) $ V.length v

    fun append (v1, v2) = V.concat [v1, v2]

    fun to_list v = V.foldr (op ::) [] v

    fun collate_r f (a1, a2) = 
        let val ls = (V.length a1, V.length a2)
            fun check (i, j) =
                case f (a1 // i, a2 // j) of EQUAL => NONE | ord => SOME ord
        in getOpt (downfrom_until2 check ls, Int.compare ls) end

    fun existsi f v = isSome $ findi f v

    fun alli f v = not $ isSome $ findi (not o f) v

end
