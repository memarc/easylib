(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

functor MonoVectorSliceSearch (
    structure S: MONO_VECTOR_SLICE_X
    structure E: sig eqtype elem end
    sharing type S.elem = E.elem
):> MONO_VECTOR_SLICE_SEARCH
    where type elem = S.elem
    where type slice = S.slice =

struct

    structure IV = IntVector
    val for = Iterate.for
    val map = Option.map
    val op //: = S.sub

    type slice = S.slice
    type elem = S.elem

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

    type flds =
         { sl: slice
         , len: int
         , topt: IV.vector option
         }

    datatype lrsearch = LR of flds

    fun compile v =
        case S.length v of
            0 => LR { sl = v, len = 0, topt = NONE }
          | 1 => LR { sl = v, len = 1, topt = NONE }
          | l =>
            let fun step (_, []) = [1]
              | step (m, r as j :: _) = 
                let val s = S.subslice (v, m, NONE) 
                    fun hit ~1 = m + 1
                      | hit i = if isSubI s v i then m - i else hit (i - 1)
                in Int.max (j, hit (m - 1)) :: r end
                val nexttbl = IV.fromList $ for step (l, 1, ~1) []
        in LR { sl = v, len = l, topt = SOME nexttbl } end

    fun really () = raise (LibBase.Impossible "MonoVectorSliceSearch: compile")

    fun findSub (LR { len = 0, ... }) _ = SOME 0
      | findSub (LR { sl = v1, len = 1, ... }) v2 = findElem (v1 //: 0) v2
      | findSub (LR { sl = _, len = _, topt = NONE }) _ = really ()
      | findSub (LR { sl = v1, len = l1, topt = SOME nexttbl }) v2 =
        let val l2 = S.length v2
            fun check i =
                if i > l2 - l1 then NONE else
                case S.rfindi (fn (j, y) => v2 //: (i + j) <> y) v1 of
                    NONE => SOME i
                  | SOME (k, _) => check (i + IV.sub (nexttbl, k))
        in check 0 end
    
    fun isSub v1 v2 = isSome $ findSub (compile v1) v2

    datatype rlsearch = RL of flds

    fun rcompile v =
        case S.length v of
            0 => RL { sl = v, len = 0, topt = NONE }
          | 1 => RL { sl = v, len = 1, topt = NONE }
          | l =>
            let fun step (_, []) = [1]
              | step (m, r as j :: _) =
                let val s = S.subslice (v, 0, SOME m)
                    fun hit i =
                        if i > l - m then m + 1
                        else if isSubI s v i then l - i
                        else hit (i + 1)
                in Int.max (j, hit m) :: r end
                val nexttbl = IV.fromList o rev $ for step (0, l - 1, 1) []
        in RL { sl = v, len = l, topt = SOME nexttbl } end

    fun rfindSub (RL { len = 0, ... }) v2 = SOME (S.length v2)
      | rfindSub (RL { len = 1, sl = v1 , ... }) v2 = rfindElem (v1 //: 0) v2
      | rfindSub (RL { sl = _, len = _, topt = NONE }) _ = really ()
      | rfindSub (RL { len = l1, sl = v1, topt = SOME nexttbl }) v2 =
        let val l2 = S.length v2
            fun check i =
                if i < 0 then NONE else
                case S.findi (fn (j, y) => v2 //: (i + j) <> y) v1 of
                    NONE => SOME i
                  | SOME (k, _) => check (i - IV.sub (nexttbl, k))
        in check (l2 - l1) end

    fun argh (m, f, msg) =
        LibBase.failure {module=m, func=f, msg=msg}

    fun findSubsStride (LR { len = 0, ... }, _) _ =
        argh ("VectorSliceSearch", "findSubsStride", "len=0: undefined")
      | findSubsStride (LR { len = 1, sl = v1, ... }, _) v2 =
        findAllElem (v1 //: 0) v2
      | findSubsStride (LR { sl = _, len = _, topt = NONE }, _) _ = really ()
      | findSubsStride (LR { len = l1, sl = v1, topt = SOME tbl }, stride) v2 =
        let val l2 = S.length v2
            fun loop (acc, n) =
                if n > l2 - l1 then acc else 
                case S.rfindi (fn (j, y) => v2 //: (n + j) <> y) v1 of
                    NONE => loop (n :: acc, n + stride)
                  | SOME (k, _) => loop (acc, n + IV.sub (tbl, k))
        in rev $ loop ([], 0) end

    fun findOverlappingSubs lr = findSubsStride (lr, 1)

    fun findDisjointSubs (lr as LR { len = l, ... }) = 
        findSubsStride (lr, l)

end
