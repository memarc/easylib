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
    fun op //! (x, y) = EasyUnsafe.Vector.sub (x, y)
    infix 8 //!
    structure S = VectorSlice

    exception UnequalLengths

    fun unzip v = (V.map #1 v, V.map #2 v)

    (* This is complex because I don't want to use elementwise access
     * to the input vector(s) due to bounds checking.  PolyML doesn't
     * export the unsafe indexing functions, and I don't want to copy their
     * implementations from the PolyML source code. *)
    fun zipwith (l, v, v') =
        let val (sl1, sl2) = (S.slice (v, 0, l), S.slice (v', 0, l))
            fun fold s = S.foldr (op ::) [] s
            val (l1, l2) = (fold sl1, fold sl2)
        in V.fromList $ ListPair.zip (l1, l2) end

    fun zip (v, v') =
        let val (l1, l2) = (V.length v, V.length v')
            val l = Int.min (l1, l2)
        in zipwith (SOME l, v, v') end

    fun map f vs = V.map f $ zip vs

    fun zipEq (v, v') = 
        let val (l, l') = (V.length v, V.length v')
        in
            if l <> l' then raise UnequalLengths
            else zipwith (SOME l, v, v')
        end

    fun mapEq f vs = V.map f $ zipEq vs

    fun foldli' (l, f, x, v1, v2) =
        let fun loop (NONE, NONE, _, acc) = acc
              | loop (SOME (y1, s1), SOME (y2, s2), i, acc) = 
                loop (S.getItem s1, S.getItem s2, i + 1, f (i, y1, y2, acc))
            val (sl1, sl2) = (S.slice (v1, 0, l), S.slice (v2, 0, l))
        in loop (S.getItem sl1, S.getItem sl2, 0, x) end

    fun foldl f x (v, v') =
        let val (l1, l2) = (V.length v, V.length v')
            val l = Int.min (l1, l2)
            fun f' (_, y, y', z) = f (y, y', z)
        in foldli' (SOME l, f', x, v, v') end

    fun foldlEq  f x (v, v') =
        let val (l, l') = (V.length v, V.length v')
            fun f' (_, y, y', z) = f (y, y', z)
        in
            if l <> l' then raise UnequalLengths
            else foldli' (SOME l, f', x, v, v')
        end

    fun foldli f x (v, v') =
        let val (l1, l2) = (V.length v, V.length v')
            val l = Int.min (l1, l2)
        in foldli' (SOME l, f, x, v, v') end

    fun foldliEq  f x (v, v') =
        let val (l, l') = (V.length v, V.length v')
        in
            if l <> l' then raise UnequalLengths
            else foldli' (SOME l, f, x, v, v')
        end

    fun foldr f x (v, v') =
        let val (l1, l2) = (V.length v, V.length v')
            fun loop (~1, _, acc) = acc
              | loop (_, ~1, acc) = acc
              | loop (i, j, acc) =
                loop (i - 1, j - 1, f (v //! i, v' //! j, acc))
        in loop (l1 - 1, l2 - 1, x) end

    fun foldri' (l, f, x, v, v') =
        let fun loop (~1, acc) = acc
              | loop (i, acc) =
                loop (i - 1, f (i, v //! i, v' //! i, acc))
        in loop (l, x) end

    fun foldrEq f x (v, v') =
        let val (l, l') = (V.length v, V.length v')
            fun f' (_, y, y', z) = f (y, y', z)
        in
            if l <> l' then raise UnequalLengths
            else foldri' (l - 1, f', x, v, v')
        end

    fun foldriEq f x (v, v') =
        let val (l, l') = (V.length v, V.length v')
        in
            if l <> l' then raise UnequalLengths
            else foldri' (l - 1, f, x, v, v')
        end

    fun app f =
        let fun act (y, y', _) = f (y, y')
        in foldl act () end

    fun appEq f =
        let fun act (y, y', _) = f (y, y')
        in foldlEq act () end

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
