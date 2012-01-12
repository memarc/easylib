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

    val fold_tabulate : int * (int * 'a -> 'a) * 'a -> 'a vector

    val append : 'a vector * 'a vector -> 'a vector

    val to_list : 'a vector -> 'a list

    val collate_r : ('a * 'a -> order) -> 'a vector * 'a vector -> order

    val existsi : (int * 'a -> bool) -> 'a vector -> bool

    val alli : (int * 'a -> bool) -> 'a vector -> bool

end
    where type 'a vector = 'a Vector.vector

structure VectorX :> VECTOR_X = struct

    structure V = Vector
    val k = Skicomb.k
    open V

    fun vector (n, x) = V.tabulate (n, k x)

    fun findi_r f v =
        let fun check (i, a, b) =
                let val p = (i, a)
                in if f p then SOME p else b end
        in V.foldli check NONE v end

    fun find_r f v =
        let fun f' (_, x) = f x
        in 
            case findi_r f' v
             of NONE => NONE
              | SOME (_, x) => SOME x
        end

    fun fold_tabulate (n, f, x) =
        let val cell = ref x
            fun fold i =
                let val x' = f (i, !cell)
                in cell := x'; x' end
        in tabulate (n, fold) end

    fun append (v1, v2) = V.concat [v1, v2]

    fun to_list v = V.foldr (op ::) [] v

    fun collate_r f (a1, a2) = 
        let fun compare (x1, x2, EQUAL) = f (x1, x2)
              | compare (_, _, ord) = ord
        in case VectorPair.foldr compare EQUAL (a1, a2)
            of EQUAL => Int.compare (V.length a1, V.length a2)
             | ord => ord
        end

    fun existsi f v =
        case findi f v of SOME _ => true | NONE => false

    fun alli f v =
        case findi (not o f) v of SOME _ => false | NONE => true

end

(* Subscripting operator *)

fun op // (x, y) = Vector.sub (x, y)
infix 8 //

