(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

functor MonoVectorSearch (
    structure V: MONO_VECTOR_X
    structure S: MONO_VECTOR_SLICE_X
    structure SS: MONO_VECTOR_SLICE_SEARCH
    sharing type V.elem = S.elem = SS.elem
    sharing type V.vector = S.vector
    sharing type S.slice = SS.slice
):> MONO_VECTOR_SEARCH
    where type elem = V.elem
    where type vector = V.vector =

struct

    val op // = V.sub

    type vector = V.vector
    type elem = V.elem

    fun findElem x v =
        Option.map #1 $ V.findi (fn (_, y) => y = x) v

    fun rfindElem x v =
        Option.map #1 $ V.rfindi (fn (_, y) => y = x) v

    fun findAllElem x v =
        let fun prepend (j, y, js) = if y = x then j :: js else js
        in V.foldri prepend [] v end

    fun isSubI v1 v2 j = V.alli (fn (i, y) => v2 // (j + i) = y) v1

    fun isPrefix v1 v2 =
        let val (l1, l2) = (V.length v1, V.length v2)
        in l1 <= l2 andalso isSubI v1 v2 0 end

    fun isSuffix v1 v2 =
        let val (l1, l2) = (V.length v1, V.length v2)
            val dl = l2 - l1
        in dl >= 0 andalso isSubI v1 v2 dl end

    fun findSub v1 v2 = SS.findSub (S.full v1) (S.full v2)

    fun isSub v1 v2 = isSome $ findSub v1 v2

    fun rfindSub v1 v2 = SS.rfindSub (S.full v1) (S.full v2)

end
