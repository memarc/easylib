(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

(* If these functions exists for lists, they should exist for vectors. *)

signature VECTOR_PAIR = sig

    exception UnequalLengths
    val zip   : 'a vector * 'b vector -> ('a * 'b) vector
    val zipEq : 'a vector * 'b vector -> ('a * 'b) vector
    val unzip : ('a * 'b) vector -> 'a vector * 'b vector
    val app   : ('a * 'b -> unit) -> 'a vector * 'b vector -> unit
    val appEq : ('a * 'b -> unit) -> 'a vector * 'b vector -> unit
    val map   : ('a * 'b -> 'c) -> 'a vector * 'b vector -> 'c vector
    val mapEq : ('a * 'b -> 'c) -> 'a vector * 'b vector -> 'c vector
    val appi   : (int * 'a * 'b -> unit) -> 'a vector * 'b vector -> unit
    val appiEq : (int * 'a * 'b -> unit) -> 'a vector * 'b vector -> unit
    val mapi   : (int * 'a * 'b -> 'c) -> 'a vector * 'b vector -> 'c vector
    val mapiEq : (int * 'a * 'b -> 'c) -> 'a vector * 'b vector -> 'c vector
    val foldl   : ('a * 'b * 'c -> 'c)
                    -> 'c -> 'a vector * 'b vector -> 'c
    val foldr   : ('a * 'b * 'c -> 'c)
                    -> 'c -> 'a vector * 'b vector -> 'c
    val foldlEq : ('a * 'b * 'c -> 'c)
                    -> 'c -> 'a vector * 'b vector -> 'c
    val foldrEq : ('a * 'b * 'c -> 'c)
                    -> 'c -> 'a vector * 'b vector -> 'c
    val foldli   : (int * 'a * 'b * 'c -> 'c)
                    -> 'c -> 'a vector * 'b vector -> 'c
    val foldliEq : (int * 'a * 'b * 'c -> 'c)
                    -> 'c -> 'a vector * 'b vector -> 'c
    val foldriEq : (int * 'a * 'b * 'c -> 'c)
                    -> 'c -> 'a vector * 'b vector -> 'c
    val findi : (int * 'a * 'b -> bool) -> 'a vector * 'b vector
                -> (int * 'a * 'b) option
    val find : ('a * 'b -> bool) -> 'a vector * 'b vector
                -> ('a * 'b) option
    val findiEq : (int * 'a * 'b -> bool) -> 'a vector * 'b vector
                -> (int * 'a * 'b) option
    val findEq : ('a * 'b -> bool) -> 'a vector * 'b vector
                -> ('a * 'b) option
    val findiEq_r : (int * 'a * 'b -> bool) -> 'a vector * 'b vector
                -> (int * 'a * 'b) option
    val find_r : ('a * 'b -> bool) -> 'a vector * 'b vector
                -> ('a * 'b) option
    val findEq_r : ('a * 'b -> bool) -> 'a vector * 'b vector
                -> ('a * 'b) option
    val all    : ('a * 'b -> bool) -> 'a vector * 'b vector -> bool
    val exists : ('a * 'b -> bool) -> 'a vector * 'b vector -> bool
    val allEq : ('a * 'b -> bool) -> 'a vector * 'b vector -> bool 
    val alli    : (int * 'a * 'b -> bool) -> 'a vector * 'b vector -> bool
    val existsi : (int * 'a * 'b -> bool) -> 'a vector * 'b vector -> bool
    val alliEq : (int * 'a * 'b -> bool) -> 'a vector * 'b vector -> bool 

end

structure VectorPair :> VECTOR_PAIR = struct

    structure V = Vector
    fun op // (x, y) = V.sub (x, y)
    infix 8 //
    structure S = VectorSlice
    fun op //: (x, y) = S.sub (x, y)
    infix 8 //:

    exception UnequalLengths

    fun unzip v = (V.map #1 v, V.map #2 v)

    (* I don't like to use elementwise access to the input vector(s) due
     * to bounds checking.  But PolyML doesn't export the unsafe
     * indexing functions, and I don't want to copy their
     * implementations from the PolyML source code. *)

    fun zipwith (eq, v1, v2) =
        let val (l1, l2) = (V.length v1, V.length v2)
            val l = Int.min (l1, l2)
        in if l1 <> l2 andalso eq then raise UnequalLengths
           else V.tabulate (l, fn i => (v1 // i, v2 // i))
        end

    fun zip (v, v') = zipwith (false, v, v')

    fun map f vs = V.map f $ zip vs

    fun mapi f vs =
        let fun f' (i, (y, y')) = f (i, y, y')
        in V.mapi f' $ zip vs end

    fun zipEq (v, v') = zipwith (true, v, v')

    fun mapEq f vs = V.map f $ zipEq vs

    fun mapiEq f vs =
        let fun f' (i, (y, y')) = f (i, y, y')
        in V.mapi f' $ zipEq vs end

    fun foldli' (eq, f, x, v1, v2) =
        let val (l1, l2) = (V.length v1, V.length v2)
            val lopt = SOME (Int.min (l1, l2))
            val (s1, s2) = (S.slice (v1, 0, lopt), S.slice (v2, 0, lopt))
            fun f' (i, y, z) = f (i, y, s2 //: i, z)
        in if l1 <> l2 andalso eq then raise UnequalLengths
           else S.foldli f' x s1
        end

    fun foldl f x (v, v') =
        let fun f' (_, y, y', z) = f (y, y', z)
        in foldli' (false, f', x, v, v') end

    fun foldlEq  f x (v, v') =
        let fun f' (_, y, y', z) = f (y, y', z)
        in foldli' (true, f', x, v, v') end

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

    fun foldr f x (v, v') =
        let fun f' (_, y, y', z) = f (y, y', z)
        in foldri' (false, f', x, v, v') end

    fun foldrEq f x (v, v') =
        let fun f' (_, y, y', z) = f (y, y', z)
        in foldri' (true, f', x, v, v') end

    fun foldriEq f x (v, v') = foldri' (true, f, x, v, v')

    fun app f =
        let fun act (y, y', _) = f (y, y')
        in foldl act () end

    fun appEq f =
        let fun act (y, y', _) = f (y, y')
        in foldlEq act () end

    fun appi f =
        let fun act (i, y, y', _) = f (i, y, y')
        in foldli act () end

    fun appiEq f =
        let fun act (i, y, y', _) = f (i, y, y')
        in foldliEq act () end

    fun findi f =
        let fun check (i, y, y', NONE) =
                let val p = (i, y, y')
                in if f p then SOME p else NONE end
              | check (_,_,_,rest) = rest
        in foldli check NONE end

    fun find f =
        let fun check (y, y', b) =
                let val p = (y, y')
                in if f p then SOME p else b end
        in foldr check NONE end

    fun findEq  f =
        let fun check (y, y', b) =
                let val p = (y, y')
                in if f p then SOME p else b end
        in foldrEq check NONE end

    fun findiEq  f =
        let fun check (i, y, y', b) =
                let val p = (i, y, y')
                in if f p then SOME p else b end
        in foldriEq check NONE end

    fun find_r f =
        let fun check (y, y', b) =
                let val p = (y, y')
                in if f p then SOME p else b end
        in foldl check NONE end

    fun findEq_r f =
        let fun check (y, y', b) =
                let val p = (y, y')
                in if f p then SOME p else b end
        in foldlEq check NONE end

    fun findiEq_r f =
        let fun check (i, y, y', b) =
                let val p = (i, y, y')
                in if f p then SOME p else b end
        in foldliEq check NONE end

    fun existsi f (v, v') =
        case findi f (v, v') of SOME _ => true | NONE => false

    fun exists f (v, v') =
        case find f (v, v') of SOME _ => true | NONE => false

    fun all f (v, v') =
        case find (not o f) (v, v') of SOME _ => false | NONE => true

    fun alli f (v, v') =
        case findi (not o f) (v, v') of SOME _ => false | NONE => true

    fun allEq f (v, v') =
        case findEq (not o f) (v, v') of SOME _ => false | NONE => true

    fun alliEq f (v, v') =
        case findiEq (not o f) (v, v') of SOME _ => false | NONE => true

end
