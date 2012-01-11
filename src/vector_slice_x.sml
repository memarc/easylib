(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

(* More vector slice functions. *)

signature VECTOR_SLICE_X = sig

    include VECTOR_SLICE

    val append : 'a slice * 'a slice -> 'a Vector.vector

    (* This interface provides a very general way to initialize a
     * vector, tabulating the values of a general recursive
     * function. [rec_tabulate (n, f)] returns a vector [v] of the shape
     * [f (g 0), f (g 1) ... f (g (n - 1))] where [g i] is the slice of
     * [v] of length [i] and base index [0].  Raise [Subscript] if
     * n is negative. *)
    val rec_tabulate: int * ('a slice -> 'a) -> 'a Vector.vector

    val findi_r : (int * 'a -> bool)
                  -> 'a slice -> (int * 'a) option
    val find_r  : ('a -> bool) -> 'a slice -> 'a option
    val existsi : (int * 'a -> bool) -> 'a slice -> bool
    val alli : (int * 'a -> bool) -> 'a slice -> bool
    val collate_r : ('a * 'a -> order)
                    -> 'a slice * 'a slice -> order
end

structure VectorSliceX :> VECTOR_SLICE_X = struct

    structure S = VectorSlice
    open S
    fun op //! (s, i) =
        let val (v, j, _) = base s
        in EasyUnsafe.Vector.sub (v, j + i) end
    infix 8 //!

    fun append (s, s') = concat [s, s']

    fun rec_tabulate (l, setter) =
        let fun setter' (v, i) = setter $ slice (v, 0, SOME i)
        in EasyUnsafe.Vector.create (l, setter') end

    fun findi_r f v =
        let fun loop ~1 = NONE
              | loop i =
                let val x = v //! i
                    val p = (i, x)
                in
                    if f p then SOME p else loop (i - 1)
                end
            val l = S.length v
        in loop (l - 1) end

    fun find_r f v =
        let fun f' (_, x) = f x
        in 
            case findi_r f' v
             of NONE => NONE
              | SOME (_, x) => SOME x
        end

    fun collate_r f (a1, a2) = 
        let val (l1, l2) = (S.length a1 - 1, S.length a2 - 1)
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

val op //: = VectorSlice.sub
infix 8 //:
