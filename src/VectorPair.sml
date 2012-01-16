(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

(* If these functions exists for lists, they should exist for vectors. *)


structure VectorPair :> VECTOR_PAIR = struct

    structure V = Vector
    structure S = VectorSlice
    open VectorSupport
    val filter = Option.filter

    exception UnequalLengths

    fun forget3_1 f (_, y, y') = f (y, y')
    fun forget4_1 f (_, y, y', z) = f (y, y', z)
    fun forget3_3 f (y, y', z) = f (y, y')
    fun forget4_4 f (i, y, y', z) = f (i, y, y')
    fun drop3_1 z = Option.map (forget3_1 id) z

    fun unzip v = (V.map #1 v, V.map #2 v)

    (* I don't like to use elementwise access to the input vector(s) due
     * to bounds checking.  But PolyML doesn't export the unsafe
     * indexing functions, and I don't want to copy their
     * implementations from the PolyML source code. *)

    fun zipwith (eq, f, v1, v2) =
        let val (l1, l2) = (V.length v1, V.length v2)
            val l = Int.min (l1, l2)
        in if l1 <> l2 andalso eq then raise UnequalLengths
           else V.tabulate (l, fn i => f (i, v1 // i, v2 // i))
        end

    fun zip (v, v') = zipwith (false, forget3_1 id, v, v')
    fun zipEq (v, v') = zipwith (true, forget3_1 id, v, v')
    fun map f (v, v') = zipwith (false, forget3_1 f, v, v')
    fun mapi f (v, v') = zipwith (false, f, v, v')
    fun mapEq f (v, v') = zipwith (true, forget3_1 f, v, v')
    fun mapiEq f (v, v') = zipwith (true, f, v, v')

    fun foldli' (eq, f, x, v1, v2) =
        let val (l1, l2) = (V.length v1, V.length v2)
            val lopt = SOME (Int.min (l1, l2))
            val (s1, s2) = (S.slice (v1, 0, lopt), S.slice (v2, 0, lopt))
            fun f' (i, y, z) = f (i, y, s2 //: i, z)
        in if l1 <> l2 andalso eq then raise UnequalLengths
           else S.foldli f' x s1
        end

    fun foldl f x (v, v') = foldli' (false, forget4_1 f, x, v, v')
    fun foldlEq  f x (v, v') = foldli' (true, forget4_1 f, x, v, v')
    fun foldli f x (v, v') = foldli' (false, f, x, v, v')
    fun foldliEq  f x (v, v') = foldli' (true, f, x, v, v')

    fun foldri' (eq, f, x, v1, v2) =
        let val (l1, l2) = (V.length v1, V.length v2)
            val l = Int.min (l1, l2)
            val (s1, s2) =
                (S.slice (v1, l1 - l, NONE), S.slice (v2, l2 - l, NONE))
            fun f' (i, y, z) = f (i, y, s2 //: i, z)
        in if l1 <> l2 andalso eq then raise UnequalLengths
           else S.foldri f' x s1
        end

    fun foldr f x (v, v') = foldri' (false, forget4_1 f, x, v, v')
    fun foldrEq f x (v, v') = foldri' (true, forget4_1 f, x, v, v')
    fun foldriEq f x (v, v') = foldri' (true, f, x, v, v')
    fun app f = foldl (forget3_3 f) ()
    fun appEq f = foldlEq (forget3_3 f) ()
    fun appi f = foldli (forget4_4 f) ()
    fun appiEq f = foldliEq (forget4_4 f) ()

    fun find' (eq, f, v1, v2) =
        let val (l1, l2) = (V.length v1, V.length v2)
            val l = Int.min (l1, l2)
        in if l1 <> l2 andalso eq then raise UnequalLengths
           else upto_until (fn i => filter f (i, v1 // i, v2 // i)) l
        end

    fun findi f (v, v') = find' (false, f, v, v')
    fun find f (v, v') = drop3_1 $ find' (false, forget3_1 f, v, v')
    fun findEq  f (v, v') = drop3_1 $ find' (true, forget3_1 f, v, v')
    fun findiEq  f (v, v') = find' (true, f, v, v')

    fun find_r f (v1, v2) =
        let val (l1, l2) = (V.length v1, V.length v2)
            fun check (i, j) = filter f (v1 // i, v2 // j)
        in downfrom_until2 check (l1, l2) end

    fun findiEq_r f (v1, v2) =
        let val (l1, l2) = (V.length v1, V.length v2)
        in if l1 <> l2 then raise UnequalLengths
           else downfrom_until (fn i => filter f (i, v1 // i, v2 // i)) l1
        end

    fun findEq_r f (v, v') = drop3_1 $ findiEq_r (forget3_1 f) (v, v')
    fun existsi f (v, v') = isSome $ findi f (v, v')
    fun exists f (v, v') = isSome $ find f (v, v')
    fun all f (v, v') = not $ isSome $ find (not o f) (v, v')
    fun alli f (v, v') = not $ isSome $ findi (not o f) (v, v')
    fun allEq f (v, v') = not $ isSome $ findEq (not o f) (v, v')
    fun alliEq f (v, v') = not $ isSome $ findiEq (not o f) (v, v')

end
