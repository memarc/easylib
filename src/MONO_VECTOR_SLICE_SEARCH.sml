(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

(* Mono vector slice stuff that requires equality on elements *)

signature MONO_VECTOR_SLICE_SEARCH =
sig
    eqtype elem
    type slice

    val findElem: elem -> slice -> int option
    val rfindElem: elem -> slice -> int option
    val findAllElem: elem -> slice -> int list
    val isPrefix: slice -> slice -> bool
    val isSuffix: slice -> slice -> bool
    val isSub: slice -> slice -> bool
    val isSubI: slice -> slice -> int -> bool

    (* For maximum power and efficiency in the substring search API we
     * have to expose the precompilation stage.  This could have been avoided if
     * we had only the rightmost and leftmost search by clever use of currying,
     * but that doesn't help when we also have the disjoint and overlapping
     * searches. *)

    type lrsearch
    type rlsearch

    val compile: slice -> lrsearch
    val findSub: lrsearch -> slice -> int option

    val rcompile: slice -> rlsearch
    val rfindSub: rlsearch -> slice -> int option
    val findOverlappingSubs: rlsearch -> slice -> int list
    val findDisjointSubs: rlsearch -> slice -> int list

end
