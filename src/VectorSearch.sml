(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

structure VectorSearch :> VECTOR_SEARCH = 
struct

    structure V = VectorX
    structure S = VectorSliceX

    fun first x v =
        Option.map #1 $ V.findi (fn (_, y) => y = x) v

    fun last x v =
        Option.map #1 $ V.rfindi (fn (_, y) => y = x) v

    fun isPrefix v1 v2 =
        let val (l1, l2) = (V.length v1, V.length v2)
        in l1 <= l2 andalso V.alli (fn (i, y) => v2 // i = y) v1 end

    fun isSuffix v1 v2 =
        let val (l1, l2) = (V.length v1, V.length v2)
            val dl = l2 - l1
        in dl >= 0 andalso V.alli (fn (i, y) => v2 // (dl + i) = y) v1 end

    fun findTail (ls, v) =
        let val lv = V.length v
            val s = S.slice (v, lv - ls, SOME ls)
            fun match ~1 = NONE
              | match i =
                if S.alli (fn (j, y) => v // (i + j) = y) s then SOME i
                else match (i - 1)
        in match (lv - ls - 1) end

    fun isSub v1 v2 =
        let val (l1, l2) = (V.length v1, V.length v2)
            fun step [] = SOME (l1 - 1, 1)
              | step ((~1, _) :: _) = NONE
              | step ((i, j) :: _) =
                let val m = case findTail (l1 - i, v1) of
                                NONE => 1
                              | SOME k => i - k 
                in SOME (i - 1, Int.max (j, m)) end
            val nexttbl = V.fromList $ List.map #2 (ListX.tabulateRec step)
            fun check i =
                if i > l2 - l1 then false
                else case V.rfindi (fn (j, y) => v2 // (i + j) <> y) v1 of
                         NONE => true
                       | SOME (k, _) => check (i + nexttbl // k)
        in check 0 end

end
