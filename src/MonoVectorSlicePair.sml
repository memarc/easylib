(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

(* If these functions exists for lists, they should exist for vectors. *)


functor MonoVectorSlicePair (
    structure L: MONO_VECTOR_SLICE;
    structure R: MONO_VECTOR_SLICE
):> MONO_VECTOR_SLICE_PAIR
    where type lelem = L.elem
    where type relem = R.elem
    where type lslice = L.slice
    where type rslice = R.slice =

struct

    structure V = Vector
    open IterateX
    val flt = Option.filter
    val min = Int.min
    val op //: = L.sub
    val op //:: = R.sub
    infix 9 //::

    type 'a vector = 'a V.vector
    type lelem = L.elem
    type relem = R.elem
    type lslice = L.slice
    type rslice = R.slice

    exception UnequalLengths

    fun forgeti3 f (_, y, y') = f (y, y')
    fun forgeti4 f (_, y, y', z) = f (y, y', z)
    fun forgeta3 f (y, y', _) = f (y, y')
    fun forgeta4 f (i, y, y', _) = f (i, y, y')
    fun dropi z = Option.map (forgeti3 id) z

    (* I don't like to use elementwise access to the input vector(s) due
     * to bounds checking.  But PolyML doesn't export the unsafe
     * indexing functions, and I don't want to copy their
     * implementations from the PolyML source code. *)

    fun zipwith (eq, f, v1, v2) =
        let val (l1, l2) = (L.length v1, R.length v2)
        in if l1 <> l2 andalso eq then raise UnequalLengths
           else V.tabulate (min (l1, l2), fn i => f (i, v1 //: i, v2 //:: i))
        end

    fun foldli' (eq, f, x, v1, v2) =
        let val (l1, l2) = (L.length v1, R.length v2)
            fun f' (i, z) = f (i, v1 //: i, v2 //:: i, z)
        in if l1 <> l2 andalso eq then raise UnequalLengths
           else repeat f' (min (l1, l2)) x
        end

    fun foldriEq f x (v1, v2) =
        let val (l1, l2) = (L.length v1, R.length v2)
            fun `i = l1 - 1 - i
        in if l1 <> l2 then raise UnequalLengths
           else repeat (fn (i, z) => f (`i, v1 //: `i, v2 //:: `i, z)) l1 x
        end

    fun foldr f x (v1, v2) =
        let val (l1, l2) = (L.length v1, R.length v2)
            val l = min (l1, l2)
            fun f' (i, z) = f (v1 //: (l1 - 1 - i), v2 //:: (l2 - 1 - i), z)
        in repeat f' l x end

    fun find' (eq, f, v1, v2) =
        let val (l1, l2) = (L.length v1, R.length v2)
        in if l1 <> l2 andalso eq then raise UnequalLengths
           else uptoUntil (fn i => flt f (i, v1 //: i, v2 //:: i)) $ min (l1, l2)
        end

    fun rfind f (v1, v2) =
        let val (l1, l2) = (L.length v1, R.length v2)
            fun check (i, j) = flt f (v1 //: i, v2 //:: j)
        in downfromUntil2 check (l1, l2) end

    fun rfindiEq f (v1, v2) =
        let val (l1, l2) = (L.length v1, R.length v2)
        in if l1 <> l2 then raise UnequalLengths
           else downfromUntil (fn i => flt f (i, v1 //: i, v2 //:: i)) l1
        end

    fun zip (v, v') = zipwith (false, forgeti3 id, v, v')
    fun zipEq (v, v') = zipwith (true, forgeti3 id, v, v')
    fun map f (v, v') = zipwith (false, forgeti3 f, v, v')
    fun mapi f (v, v') = zipwith (false, f, v, v')
    fun mapEq f (v, v') = zipwith (true, forgeti3 f, v, v')
    fun mapiEq f (v, v') = zipwith (true, f, v, v')
    fun foldl f x (v, v') = foldli' (false, forgeti4 f, x, v, v')
    fun foldlEq  f x (v, v') = foldli' (true, forgeti4 f, x, v, v')
    fun foldli f x (v, v') = foldli' (false, f, x, v, v')
    fun foldliEq  f x (v, v') = foldli' (true, f, x, v, v')
    fun foldrEq f x (v, v') = foldriEq (forgeti4 f) x (v, v')
    fun app f = foldl (forgeta3 f) ()
    fun appEq f = foldlEq (forgeta3 f) ()
    fun appi f = foldli (forgeta4 f) ()
    fun appiEq f = foldliEq (forgeta4 f) ()
    fun findi f (v, v') = find' (false, f, v, v')
    fun find f (v, v') = dropi $ find' (false, forgeti3 f, v, v')
    fun findEq  f (v, v') = dropi $ find' (true, forgeti3 f, v, v')
    fun findiEq  f (v, v') = find' (true, f, v, v')
    fun rfindEq f (v, v') = dropi $ rfindiEq (forgeti3 f) (v, v')
    fun existsi f (v, v') = isSome $ findi f (v, v')
    fun exists f (v, v') = isSome $ find f (v, v')
    fun all f (v, v') = not $ isSome $ find (not o f) (v, v')
    fun alli f (v, v') = not $ isSome $ findi (not o f) (v, v')
    fun allEq f (v, v') = not $ isSome $ findEq (not o f) (v, v')
    fun alliEq f (v, v') = not $ isSome $ findiEq (not o f) (v, v')

    fun findAllEq f (v1, v2) =
        let fun prepend (x1, x2, xs) =
                let val p = (x1, x2) in if f p then p :: xs else xs end
        in foldrEq prepend [] (v1, v2) end

    fun findiAllEq f (v1, v2) =
        let fun prepend (i, x1, x2, xs) =
                let val p = (i, x1, x2) in if f p then p :: xs else xs end
        in foldriEq prepend [] (v1, v2) end

end
