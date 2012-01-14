(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

(* Extra vector functions *)

signature VECTOR_X = sig

    include VECTOR

    val vector : int * 'a -> 'a vector

    val find_r : ('a -> bool) -> 'a vector -> 'a option

    val findi_r : (int * 'a -> bool) -> 'a vector -> (int * 'a) option

    val append : 'a vector * 'a vector -> 'a vector

    val to_list : 'a vector -> 'a list

    val collate_r : ('a * 'a -> order) -> 'a vector * 'a vector -> order

    val existsi : (int * 'a -> bool) -> 'a vector -> bool

    val alli : (int * 'a -> bool) -> 'a vector -> bool

end
    where type 'a vector = 'a Vector.vector


(* Subscripting operator *)

fun op // (x, y) = Vector.sub (x, y)
infix 8 //

structure VectorX :> VECTOR_X = struct

    structure V = Vector
    val k = Skicomb.k
    open V
    open EasyLoop
    val filter = Option.filter

    fun vector (n, x) = V.tabulate (n, k x)

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
