(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

(* More vector slice functions. *)

signature VECTOR_SLICE_X = sig

    include VECTOR_SLICE

    val append : 'a slice * 'a slice -> 'a Vector.vector


    val findi_r : (int * 'a -> bool)
                  -> 'a slice -> (int * 'a) option
    val find_r  : ('a -> bool) -> 'a slice -> 'a option
    val existsi : (int * 'a -> bool) -> 'a slice -> bool
    val alli : (int * 'a -> bool) -> 'a slice -> bool
    val collate_r : ('a * 'a -> order)
                    -> 'a slice * 'a slice -> order
end
    where type 'a slice = 'a VectorSlice.slice

val op //: = VectorSlice.sub
infix 8 //:

structure VectorSliceX :> VECTOR_SLICE_X = struct

    structure S = VectorSlice
    open S

    fun append (s, s') = concat [s, s']

    fun findi_r f v =
        let fun check (i, a, b) =
                let val p = (i, a)
                in if f p then SOME p else b end
        in S.foldli check NONE v end

    fun find_r f v =
        let fun f' (_, x) = f x
        in 
            case findi_r f' v
             of NONE => NONE
              | SOME (_, x) => SOME x
        end

    (* There's no VectorSlicePair, unfortunately. Should there be? *)
    fun collate_r f (s1, s2) =
        let val (l1, l2) = (S.length s1, S.length s2)
            fun loop (~1, ~1) = EQUAL
              | loop (~1, _) = LESS
              | loop (_, ~1) = GREATER
              | loop (i, j)  = 
                case f (s1 //: i, s2 //: j)
                 of EQUAL => loop (i - 1, j - 1)
                  | ord => ord
        in loop (l1 - 1, l2 - 1) end

    fun existsi f v =
        case findi f v of SOME _ => true | NONE => false

    fun alli f v =
        case findi (not o f) v of SOME _ => false | NONE => true

end

