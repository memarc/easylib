(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

signature MONO_VECTOR_SLICE_PAIR = sig

    type lelem
    type relem
    type lvector
    type rvector
    type lslice
    type rslice

    exception UnequalLengths
    val zip   : lslice * rslice -> (lelem * relem) vector
    val zipEq : lslice * rslice -> (lelem * relem) vector
    val app   : (lelem * relem -> unit) -> lslice * rslice -> unit
    val appEq : (lelem * relem -> unit) -> lslice * rslice -> unit
    val map   : (lelem * relem -> 'c) -> lslice * rslice -> 'c vector
    val mapEq : (lelem * relem -> 'c) -> lslice * rslice -> 'c vector
    val appi   : (int * lelem * relem -> unit) -> lslice * rslice -> unit
    val appiEq : (int * lelem * relem -> unit) -> lslice * rslice -> unit
    val mapi   : (int * lelem * relem -> 'c) -> lslice * rslice -> 'c vector
    val mapiEq : (int * lelem * relem -> 'c) -> lslice * rslice -> 'c vector
    val foldl   : (lelem * relem * 'c -> 'c)
                    -> 'c -> lslice * rslice -> 'c
    val foldr   : (lelem * relem * 'c -> 'c)
                    -> 'c -> lslice * rslice -> 'c
    val foldlEq : (lelem * relem * 'c -> 'c)
                    -> 'c -> lslice * rslice -> 'c
    val foldrEq : (lelem * relem * 'c -> 'c)
                    -> 'c -> lslice * rslice -> 'c
    val foldli   : (int * lelem * relem * 'c -> 'c)
                    -> 'c -> lslice * rslice -> 'c
    val foldliEq : (int * lelem * relem * 'c -> 'c)
                    -> 'c -> lslice * rslice -> 'c
    val foldriEq : (int * lelem * relem * 'c -> 'c)
                    -> 'c -> lslice * rslice -> 'c
    val findi : (int * lelem * relem -> bool) -> lslice * rslice
                -> (int * lelem * relem) option
    val find : (lelem * relem -> bool) -> lslice * rslice
                -> (lelem * relem) option
    val findiAllEq: (int * lelem * relem -> bool) -> lslice * rslice
                -> (int * lelem * relem) list
    val findAllEq: (lelem * relem -> bool) -> lslice * rslice
                -> (lelem * relem) list
    val findiEq : (int * lelem * relem -> bool) -> lslice * rslice
                -> (int * lelem * relem) option
    val findEq : (lelem * relem -> bool) -> lslice * rslice
                -> (lelem * relem) option
    val rfindiEq : (int * lelem * relem -> bool) -> lslice * rslice
                -> (int * lelem * relem) option
    val rfind : (lelem * relem -> bool) -> lslice * rslice
                -> (lelem * relem) option
    val rfindEq : (lelem * relem -> bool) -> lslice * rslice
                -> (lelem * relem) option
    val all    : (lelem * relem -> bool) -> lslice * rslice -> bool
    val exists : (lelem * relem -> bool) -> lslice * rslice -> bool
    val allEq : (lelem * relem -> bool) -> lslice * rslice -> bool 
    val alli    : (int * lelem * relem -> bool) -> lslice * rslice -> bool
    val existsi : (int * lelem * relem -> bool) -> lslice * rslice -> bool
    val alliEq : (int * lelem * relem -> bool) -> lslice * rslice -> bool 

end
