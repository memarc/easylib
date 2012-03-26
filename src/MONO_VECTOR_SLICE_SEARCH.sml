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
    val findSub: slice -> slice -> int option
    val rfindSub: slice -> slice -> int option
    val isSub: slice -> slice -> bool
    val isSubI: slice -> slice -> int -> bool

end
