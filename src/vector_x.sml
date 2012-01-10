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

    val find_r : ('a -> bool) -> 'a vector -> 'a option

    val findi_r : (int * 'a -> bool) -> 'a vector -> (int * 'a) option

    val fold_tabulate : int * (int * 'a -> 'a) * 'a -> 'a vector

    val append : 'a vector * 'a vector -> 'a vector

    val to_list : 'a vector -> 'a list

    val collate_r : ('a * 'a -> order) -> 'a vector * 'a vector -> order

end

local
    structure V = Vector
    fun op //! (x, y) = Unsafe.Vector.sub (x, y)
    infix 8 //!
    fun op //: (x, y) = Unsafe.Array.sub (x, y)
    infix 8 //:
in

structure Vector_x :> VECTOR_X = struct

    open V

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
        if n = 0 then V.fromList [] else
        let val a = Array.array (n + 1, x)
            fun fr (0, _) = x
              | fr (i, _) = f (i - 1, a //: (i - 1))
        in
            ( Array.modifyi fr a
            ; V.tabulate (n, fn i => a //: (i - 1)) )
        end

    fun append (v1, v2) = V.concat [v1, v2]

    fun to_list v =
        let val vr = ref []
            val l1 = V.length v - 1
            fun f (i, _) = vr := (v //! (l1 - i) :: !vr)
        in
            ( V.appi f v
            ; !vr )
        end

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

end                             (* structure *)

end                             (* local *)
