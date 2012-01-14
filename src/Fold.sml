
(* Taken from http://mlton.org/Fold *)

structure Fold :> FOLD = struct

    fun fold  (a, f) g = g (a, f)

    fun step0  h (a, f) = fold (h a, f)

    fun step1 h (a, f) b = fold (h (b, a), f)

    fun end_ (base, finish) = finish base

end
