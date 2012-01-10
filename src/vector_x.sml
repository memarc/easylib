(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

(* Subscripting operator *)

fun op // (x, y) = Vector.sub (x, y)
infix 8 //

(* Extra vector functions *)

signature VECTOR_X = sig

    include VECTOR

    val vector : int * 'a -> 'a vector

    val find_r : ('a -> bool) -> 'a vector -> 'a option

    val findi_r : (int * 'a -> bool) -> 'a vector -> (int * 'a) option

    val fold_tabulate : int * (int * 'a -> 'a) * 'a -> 'a vector

    val append : 'a vector * 'a vector -> 'a vector

    val to_list : 'a vector -> 'a list

    val collate_r : ('a * 'a -> order) -> 'a vector * 'a vector -> order

    val existsi : (int * 'a -> bool) -> 'a vector -> bool

    val alli : (int * 'a -> bool) -> 'a vector -> bool

end

structure VectorX :> VECTOR_X = struct

    structure V = Vector
    fun op //! (x, y) = EasyUnsafe.Vector.sub (x, y)
    infix 8 //!
    fun op <- (a, (i, x)) = EasyUnsafe.Vector.update (a, i, x)
    infix 8 <-
    val k = Skicomb.k
    open V

    fun vector (n, x) = V.tabulate (n, k x)

    fun findi_r f v =
        let fun loop ~1 = NONE
              | loop i =
                let val x = v //! i
                    val p = (i, x)
                in
                    if f p then SOME p else loop (i - 1)
                end
            val l = V.length v
        in loop (l - 1) end

    fun find_r f v =
        let fun f' (_, x) = f x
        in 
            case findi_r f' v
             of NONE => NONE
              | SOME (_, x) => SOME x
        end

    fun fold_tabulate (n, f, x) =
        let val cell = ref x
            val v = vector (n, x)
            fun loop 0 = ()
              | loop i =
                let val i' = n - i
                in
                    cell := f (i', !cell)
                  ; v <- (i', !cell)
                  ; loop (i - 1)
                end
        in v before loop n end

    fun append (v1, v2) = V.concat [v1, v2]

    fun to_list v =
        let val l1 = V.length v - 1
            val res = ref []
            fun loop ~1 = !res
              | loop i =
                ( res := v //! i :: !res
                ; loop (i - 1) )
        in loop l1 end

    fun collate_r f (a1, a2) = 
        let val (l1, l2) = (V.length a1 - 1, V.length a2 - 1)
            fun loop (~1, ~1) = EQUAL
              | loop (~1, j) = LESS
              | loop (i, ~1) = GREATER
              | loop (i, j) =
                case f (a1 //! i, a2 //! j)
                 of EQUAL => loop (i - 1, j - 1)
                  | res => res
        in loop (l1, l2) end

    fun existsi f v =
        case findi f v of SOME _ => true | NONE => false

    fun alli f v =
        case findi (not o f) v of SOME _ => false | NONE => true

end
