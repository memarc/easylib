(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

(* More vector slice functions. *)


structure VectorSliceX :> VECTOR_SLICE_X = struct

    structure S = VectorSlice
    open S
    open IterateX
    val filter = Option.filter

    fun append (s, s') = concat [s, s']

    fun findi_r f v =
        downfrom_until (fn i => filter f (i, v //: i)) $ S.length v

    fun find_r f v =
        downfrom_until (fn i => filter f (v //: i)) $ S.length v

    (* There's no VectorSlicePair, unfortunately. Should there be? *)
    fun collate_r f (s1, s2) =
        let val ls = (S.length s1, S.length s2)
            fun check (i, j) =
                case f (s1 //: i, s2 //: j) of EQUAL => NONE | ord => SOME ord
        in getOpt (downfrom_until2 check ls, Int.compare ls) end

    fun existsi f v = isSome $ findi f v

    fun alli f v = not $ isSome $ findi (not o f) v

end

