(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

structure VectorSliceSearch :> VECTOR_SLICE_SEARCH = 
struct

    structure IV = IntVector
    structure S = VectorSliceX
    val for = Iterate.for
    val map = Option.map

    type ''a slice = ''a S.slice

    fun findElem x v =
        map #1 $ S.findi (fn (_, y) => y = x) v

    fun rfindElem x v =
        map #1 $ S.rfindi (fn (_, y) => y = x) v

    fun findAllElem x v =
        let fun prepend (j, y, js) = if y = x then j :: js else js
        in S.foldri prepend [] v end

    fun isSubI v1 v2 j = S.alli (fn (i, y) => v2 //: (j + i) = y) v1

    fun isPrefix v1 v2 =
        let val (l1, l2) = (S.length v1, S.length v2)
        in l1 <= l2 andalso isSubI v1 v2 0 end

    fun isSuffix v1 v2 =
        let val (l1, l2) = (S.length v1, S.length v2)
            val dl = l2 - l1
        in dl >= 0 andalso isSubI v1 v2 dl end

    fun findSub' (v1, v2, l1, l2) =
        let fun step (_, []) = [1]
              | step (m, r as j :: _) = 
                let val s = S.subslice (v1, m, NONE) 
                    fun hit ~1 = m + 1
                      | hit i = if isSubI s v1 i then m - i else hit (i - 1)
                in Int.max (j, hit (m - 1)) :: r end
            val nexttbl = IV.fromList $ for step (l1, 1, ~1) []
            fun check i =
                if i > l2 - l1 then NONE else
                case S.rfindi (fn (j, y) => v2 //: (i + j) <> y) v1 of
                    NONE => SOME i
                  | SOME (k, _) => check (i + IV.sub (nexttbl, k))
        in check 0 end
    
    fun findSub v1 v2 =
        let val (l1, l2) = (S.length v1, S.length v2) in
            case l1 of
                0 => SOME 0
              | 1 => findElem (v1 //: 0) v2
              | _ => findSub' (v1, v2, l1, l2)
        end
    
    fun isSub v1 v2 = isSome $ findSub v1 v2
    
    fun rfindSub' (v1, v2, l1, l2) =
        let fun step (_, []) = [1]
              | step (m, r as j :: _) =
                let val s = S.subslice (v1, 0, SOME m)
                    fun hit i =
                        if i > l1 - m then m + 1
                        else if isSubI s v1 i then l1 - i
                        else hit (i + 1)
                in Int.max (j, hit m) :: r end
            val nexttbl = IV.fromList o rev $ for step (0, l1 - 1, 1) []
            fun check i =
                if i < 0 then NONE else
                case S.findi (fn (j, y) => v2 //: (i + j) <> y) v1 of
                    NONE => SOME i
                  | SOME (k, _) => check (i - IV.sub (nexttbl, k))
        in check (l2 - l1) end
    
    fun rfindSub v1 v2 =
        let val (l1, l2) = (S.length v1, S.length v2) in
            case l1 of
                0 => SOME l2
              | 1 => rfindElem (v1 //: 0) v2
              | _ => rfindSub' (v1, v2, l1, l2)
        end

    fun findSubsStride (v1 : ''a slice, v2 : ''a slice, stride: int) =
        let val l1 = S.length v1
            fun loop (acc, l) =
                case rfindSub v1 $ S.subslice (v2, 0, SOME l) of
                    NONE => acc
                  | SOME n => 
                    if n < stride then n :: acc
                    else loop (n :: acc, n - stride + l1)
        in loop ([], S.length v2) end

    fun findOverlappingSubs v1 v2 = findSubsStride (v1, v2, 1)

    fun findDisjointSubs v1 v2 = findSubsStride (v1, v2, S.length v1)

end
