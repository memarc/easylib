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
    val id = Skicomb.i          (* identity function *)

    exception UnequalLengths

    fun zipwith (l, f, v, v') = V.tabulate (l, fn i => f (v //! i, v' //! i))

    fun unzip v =
        let fun vsub i = v //! i
            val l = V.length v
        in (V.tabulate (l, #1 o vsub), V.tabulate (l, #2 o vsub)) end

    fun app' (l, f, v, v') =
        let fun loop 0 = ()
              | loop i =
                let val i' = l - i
                in f (v //! i', v' //! i'); loop (i - 1) end
        in loop l end

    fun app f (v, v') =
        let val (l1, l2) = (V.length v, V.length v')
            val l = Int.min (l1, l2)
        in app' (l, f, v, v') end
        
    fun appEq f (v, v') =
        let val (l, l') = (V.length v, V.length v')
        in
            if l <> l' then raise UnequalLengths
            else app' (l, f, v, v')
        end

    fun map f (v, v') =
        let val (l1, l2) = (V.length v, V.length v')
            val l = Int.min (l1, l2)
        in zipwith (l, f, v, v') end

    fun zip (v, v') = map id (v, v')

    fun mapEq f (v, v') =
        let val (l, l') = (V.length v, V.length v')
        in
            if l <> l' then raise UnequalLengths
            else zipwith (l, f, v, v')
        end

    fun zipEq (v, v') = mapEq id (v, v')

    fun foldli' (l, f, x, v, v') =
        let fun loop (~1, acc) = acc
              | loop (i, acc) =
                let val i' = l - i
                in loop (i - 1, f (i', v //! i', v' //! i', acc)) end
        in loop (l, x) end

    fun foldl f x (v, v') =
        let val (l1, l2) = (V.length v, V.length v')
            val l = Int.min (l1, l2)
            fun f' (_, y, y', z) = f (y, y', z)
        in foldli' (l - 1, f', x, v, v') end

    fun foldlEq  f x (v, v') =
        let val (l, l') = (V.length v, V.length v')
            fun f' (_, y, y', z) = f (y, y', z)
        in
            if l <> l' then raise UnequalLengths
            else foldli' (l - 1, f', x, v, v')
        end

    fun foldli f x (v, v') =
        let val (l1, l2) = (V.length v, V.length v')
            val l = Int.min (l1, l2)
        in foldli' (l - 1, f, x, v, v') end

    fun foldliEq  f x (v, v') =
        let val (l, l') = (V.length v, V.length v')
        in
            if l <> l' then raise UnequalLengths
            else foldli' (l - 1, f, x, v, v')
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

    fun findi' (l, f, v, v') =
        let fun loop ~1 = NONE
              | loop i =
                let val i' = l - i
                    val p = (i', v //! i', v' //! i')
                in
                    if f p then SOME p
                    else loop (i - 1)
                end
        in loop l end

    fun find f (v, v') =
        let val (l1, l2) = (V.length v, V.length v')
            val l = Int.min (l1, l2)
            fun f' (_, y, y') = f (y, y')
        in
            case findi' (l - 1, f', v, v')
             of NONE => NONE
              | SOME (i, y, y') => SOME (y, y')
        end

    fun findEq  f (v, v') =
        let val (l, l') = (V.length v, V.length v')
            fun f' (_, y, y') = f (y, y')
        in
            if l <> l' then raise UnequalLengths
            else case findi' (l - 1, f', v, v')
                  of NONE => NONE
                   | SOME (i, y, y') => SOME (y, y')
        end

    fun findi f (v, v') =
        let val (l1, l2) = (V.length v, V.length v')
            val l = Int.min (l1, l2)
        in findi' (l - 1, f, v, v') end

    fun findiEq  f (v, v') =
        let val (l, l') = (V.length v, V.length v')
        in
            if l <> l' then raise UnequalLengths
            else findi' (l - 1, f, v, v')
        end

    fun find_r f (v, v') =
        let val (l1, l2) = (V.length v, V.length v')
            fun loop (~1, _) = NONE
              | loop (_, ~1) = NONE
              | loop (i, j) =
                let val p = (v //! i, v' //! j)
                in
                    if f p then SOME p else loop (i - 1, j - 1)
                end
        in loop (l1 - 1, l2 - 1) end

    fun findi_r' (l, f, v, v') =
        let fun loop ~1 = NONE
              | loop i =
                let val p = (i, v //! i, v' //! i)
                in
                    if f p then SOME p
                    else loop (i - 1)
                end
        in loop l end

    fun findEq_r f (v, v') =
        let val (l, l') = (V.length v, V.length v')
            fun f' (_, y, y') = f (y, y')
        in
            if l <> l' then raise UnequalLengths
            else case findi_r' (l - 1, f', v, v')
                  of NONE => NONE
                   | SOME (_, y, y') => SOME (y, y')
        end

    fun findiEq_r f (v, v') =
        let val (l, l') = (V.length v, V.length v')
        in
            if l <> l' then raise UnequalLengths
            else findi_r' (l - 1, f, v, v')
        end

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
