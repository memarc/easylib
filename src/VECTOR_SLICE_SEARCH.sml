(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

(* Vector slice stuff that requires equality on elements *)

signature VECTOR_SLICE_SEARCH =
sig

    type ''a slice

    val findElem: ''a -> ''a slice -> int option
    val rfindElem: ''a -> ''a slice -> int option
    val findAllElem: ''a -> ''a slice -> int list
    val isPrefix: ''a slice -> ''a slice -> bool
    val isSuffix: ''a slice -> ''a slice -> bool
    val isSub: ''a slice -> ''a slice -> bool
    val isSubI: ''a slice -> ''a slice -> int -> bool

    (* For maximum power and efficiency in the substring search API we
     * have to expose the precompilation stage.  This could have been avoided if
     * we had only the rightmost and leftmost search by clever use of currying,
     * but that doesn't help when we also have the disjoint and overlapping
     * searches. *)

    type ''a lrsearch
    type ''a rlsearch

    val compile: ''a slice -> ''a lrsearch
    val findSub: ''a lrsearch -> ''a slice -> int option

    val rcompile: ''a slice -> ''a rlsearch
    val rfindSub: ''a rlsearch -> ''a slice -> int option
    val findOverlappingSubs: ''a rlsearch -> ''a slice -> int list
    val findDisjointSubs: ''a rlsearch -> ''a slice -> int list

end
    where type ''a slice = ''a VectorSlice.slice
