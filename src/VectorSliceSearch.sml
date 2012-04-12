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

    fun fwdTable (v, l) =
        let fun step (_, []) = [1]
              | step (m, r as j :: _) = 
                let val s = S.subslice (v, m, NONE) 
                    fun hit ~1 = m + 1
                      | hit i = if isSubI s v i then m - i else hit (i - 1)
                in Int.max (j, hit (m - 1)) :: r end
        in IV.fromList $ for step (l, 1, ~1) [] end

    fun findSub' (v1, v2, l1, l2) =
        let val nexttbl = fwdTable (v1, l1)
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
    
    fun bkwdTable (v, l) =
        let fun step (_, []) = [1]
              | step (m, r as j :: _) =
                let val s = S.subslice (v, 0, SOME m)
                    fun hit i =
                        if i > l - m then m + 1
                        else if isSubI s v i then l - i
                        else hit (i + 1)
                in Int.max (j, hit m) :: r end
        in IV.fromList o rev $ for step (0, l - 1, 1) [] end

    fun rfindSub' (v1, v2, l1, l2) =
        let val nexttbl = bkwdTable (v1, l1)
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

    fun argh (m, f, msg) =
        LibBase.failure {module=m, func=f, msg=msg}

    fun findSubsStride (_, _, 0, _, _) =
        argh ("VectorSliceSearch", "findSubsStride", "l1=0: undefined")
      | findSubsStride (v1, v2, 1, _, 1) =
        findAllElem (v1 //: 0) v2
      | findSubsStride (v1, v2, l1, l2, stride) =
        let val nexttbl = bkwdTable (v1, l1)
            fun loop (acc, n) =
                if n < 0 then acc else 
                case S.findi (fn (j, y) => v2 //: (n + j) <> y) v1 of
                    NONE => loop (n :: acc, n - stride)
                  | SOME (k, _) => loop (acc, n - IV.sub (nexttbl, k))
        in loop ([], l2 - l1) end

    fun findOverlappingSubs v1 v2 = 
        let val (l1, l2) = (S.length v1, S.length v2)
        in findSubsStride (v1, v2, l1, l2, 1) end

    fun findDisjointSubs v1 v2 =
        let val (l1, l2) = (S.length v1, S.length v2)
        in findSubsStride (v1, v2, l1, l2, l1) end

end
