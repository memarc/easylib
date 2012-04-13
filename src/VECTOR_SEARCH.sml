(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

(* Vector stuff that requires equality on elements *)

signature VECTOR_SEARCH =
sig

    val findElem: ''a -> ''a vector -> int option
    val rfindElem: ''a -> ''a vector -> int option
    val findAllElem: ''a -> ''a vector -> int list
    val isPrefix: ''a vector -> ''a vector -> bool
    val isSuffix: ''a vector -> ''a vector -> bool
    val isSub: ''a vector -> ''a vector -> bool
    val isSubI: ''a vector -> ''a vector -> int -> bool

    type ''a lrsearch
    val compile: ''a vector -> ''a lrsearch
    val findSub: ''a lrsearch -> ''a vector -> int option

    type ''a rlsearch
    val rcompile: ''a vector -> ''a rlsearch
    val rfindSub: ''a rlsearch -> ''a vector -> int option

end
