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
    val findSub: ''a slice -> ''a slice -> int option
    val rfindSub: ''a slice -> ''a slice -> int option
    val isSub: ''a slice -> ''a slice -> bool
    val isSubI: ''a slice -> ''a slice -> int -> bool

end
    where type ''a slice = ''a VectorSlice.slice
