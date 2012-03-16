(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

signature MONO_VECTOR_PAIR = 
sig

    exception UnequalLengths
    type lelem
    type relem
    type lvector
    type rvector
    val zip   : lvector * rvector -> (lelem * relem) vector
    val zipEq : lvector * rvector -> (lelem * relem) vector
    val unzip : (lelem * relem) vector -> lvector * rvector
    val app   : (lelem * relem -> unit) -> lvector * rvector -> unit
    val appEq : (lelem * relem -> unit) -> lvector * rvector -> unit
    val map   : (lelem * relem -> 'c) -> lvector * rvector -> 'c vector
    val mapEq : (lelem * relem -> 'c) -> lvector * rvector -> 'c vector
    val appi   : (int * lelem * relem -> unit) -> lvector * rvector -> unit
    val appiEq : (int * lelem * relem -> unit) -> lvector * rvector -> unit
    val mapi   : (int * lelem * relem -> 'c) -> lvector * rvector -> 'c vector
    val mapiEq : (int * lelem * relem -> 'c) -> lvector * rvector -> 'c vector
    val foldl   : (lelem * relem * 'c -> 'c)
                    -> 'c -> lvector * rvector -> 'c
    val foldr   : (lelem * relem * 'c -> 'c)
                    -> 'c -> lvector * rvector -> 'c
    val foldlEq : (lelem * relem * 'c -> 'c)
                    -> 'c -> lvector * rvector -> 'c
    val foldrEq : (lelem * relem * 'c -> 'c)
                    -> 'c -> lvector * rvector -> 'c
    val foldli   : (int * lelem * relem * 'c -> 'c)
                    -> 'c -> lvector * rvector -> 'c
    val foldliEq : (int * lelem * relem * 'c -> 'c)
                    -> 'c -> lvector * rvector -> 'c
    val foldriEq : (int * lelem * relem * 'c -> 'c)
                    -> 'c -> lvector * rvector -> 'c
    val findi : (int * lelem * relem -> bool) -> lvector * rvector
                -> (int * lelem * relem) option
    val find : (lelem * relem -> bool) -> lvector * rvector
                -> (lelem * relem) option
    val findiEq : (int * lelem * relem -> bool) -> lvector * rvector
                -> (int * lelem * relem) option
    val findEq : (lelem * relem -> bool) -> lvector * rvector
                -> (lelem * relem) option
    val rfindiEq : (int * lelem * relem -> bool) -> lvector * rvector
                -> (int * lelem * relem) option
    val rfind : (lelem * relem -> bool) -> lvector * rvector
                -> (lelem * relem) option
    val rfindEq : (lelem * relem -> bool) -> lvector * rvector
                -> (lelem * relem) option
    val all    : (lelem * relem -> bool) -> lvector * rvector -> bool
    val exists : (lelem * relem -> bool) -> lvector * rvector -> bool
    val allEq : (lelem * relem -> bool) -> lvector * rvector -> bool 
    val alli    : (int * lelem * relem -> bool) -> lvector * rvector -> bool
    val existsi : (int * lelem * relem -> bool) -> lvector * rvector -> bool
    val alliEq : (int * lelem * relem -> bool) -> lvector * rvector -> bool 

end
