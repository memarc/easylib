(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

structure VectorSearch :> VECTOR_SEARCH = 
struct

    structure V = VectorX
    structure IV = IntVector
    structure S = VectorSliceX
    val for = Iterate.for

    fun findElem x v =
        Option.map #1 $ V.findi (fn (_, y) => y = x) v

    fun rfindElem x v =
        Option.map #1 $ V.rfindi (fn (_, y) => y = x) v

    fun findAllElem x v =
        let fun prepend (j, y, js) = if y = x then j :: js else js
        in V.foldri prepend [] v end

    fun isSubI v1 v2 j = V.alli (fn (i, y) => v2 // (j + i) = y) v1

    fun isSubS (s, v, j) = S.alli (fn (i, y) => v // (j + i) = y) s

    fun isPrefix v1 v2 =
        let val (l1, l2) = (V.length v1, V.length v2)
        in l1 <= l2 andalso isSubI v1 v2 0 end

    fun isSuffix v1 v2 =
        let val (l1, l2) = (V.length v1, V.length v2)
            val dl = l2 - l1
        in dl >= 0 andalso isSubI v1 v2 dl end

    fun findSub' (v1, v2, l1, l2) =
        let fun step (m, r as j :: _) = 
                let val s = S.slice (v1, m, NONE) 
                    fun hit ~1 = m + 1
                      | hit i = if isSubS (s, v1, i) then m - i else hit (i - 1)
                in Int.max (j, hit (m - 1)) :: r end
            val nexttbl = IV.fromList $ for step (l1 - 1, 1, ~1) [1]
            fun check i =
                if i > l2 - l1 then NONE else
                case V.rfindi (fn (j, y) => v2 // (i + j) <> y) v1 of
                    NONE => SOME i
                  | SOME (k, _) => check (i + IV.sub (nexttbl, k))
        in check 0 end

    fun findSub v1 v2 =
        let val (l1, l2) = (V.length v1, V.length v2) in
            case l1 of
                0 => SOME 0
              | 1 => findElem (v1 // 0) v2
              | _ => findSub' (v1, v2, l1, l2)
        end

    fun isSub v1 v2 = isSome $ findSub v1 v2

    fun rfindSub' (v1, v2, l1, l2) =
        let fun step (m, r as j :: _) =
                let val m' = l1 - m
                    val s = S.slice (v1, 0, SOME m)
                    fun hit ~1 = m' + 1
                      | hit i = if isSubS (s, v1, m' - i) then m' - i else hit (i - 1)
                in Int.max (j, hit m') :: r end
            val nexttbl = IV.fromList $ for step (1, l1 - 1, 1) [1]
            fun check i =
                if i < 0 then NONE else
                case V.findi (fn (j, y) => v2 // (i + j) <> y) v1 of
                    NONE => SOME i
                  | SOME (k, _) => check (i - IV.sub (nexttbl, l1 - 1 - k))
        in check (l2 - l1) end

    fun rfindSub v1 v2 =
        let val (l1, l2) = (V.length v1, V.length v2) in
            case l1 of
                0 => SOME l2
              | 1 => rfindElem (v1 // 0) v2
              | _ => rfindSub' (v1, v2, l1, l2)
        end

end
