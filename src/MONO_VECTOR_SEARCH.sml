(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

(* Mono vector stuff that requires equality on elements *)

signature MONO_VECTOR_SEARCH =
sig

    eqtype elem
    type vector

    val findElem: elem -> vector -> int option
    val rfindElem: elem -> vector -> int option
    val findAllElem: elem -> vector -> int list
    val isPrefix: vector -> vector -> bool
    val isSuffix: vector -> vector -> bool
    val findSub: vector -> vector -> int option
    val rfindSub: vector -> vector -> int option
    val isSub: vector -> vector -> bool
    val isSubI: vector -> vector -> int -> bool

end
