(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

signature VECTOR_SLICE_PAIR = sig

    type 'a slice
    type 'a vector

    exception UnequalLengths
    val zip   : 'a slice * 'b slice -> ('a * 'b) vector
    val zipEq : 'a slice * 'b slice -> ('a * 'b) vector
    val unzip : ('a * 'b) slice -> 'a vector * 'b vector
    val app   : ('a * 'b -> unit) -> 'a slice * 'b slice -> unit
    val appEq : ('a * 'b -> unit) -> 'a slice * 'b slice -> unit
    val map   : ('a * 'b -> 'c) -> 'a slice * 'b slice -> 'c vector
    val mapEq : ('a * 'b -> 'c) -> 'a slice * 'b slice -> 'c vector
    val appi   : (int * 'a * 'b -> unit) -> 'a slice * 'b slice -> unit
    val appiEq : (int * 'a * 'b -> unit) -> 'a slice * 'b slice -> unit
    val mapi   : (int * 'a * 'b -> 'c) -> 'a slice * 'b slice -> 'c vector
    val mapiEq : (int * 'a * 'b -> 'c) -> 'a slice * 'b slice -> 'c vector
    val foldl   : ('a * 'b * 'c -> 'c)
                    -> 'c -> 'a slice * 'b slice -> 'c
    val foldr   : ('a * 'b * 'c -> 'c)
                    -> 'c -> 'a slice * 'b slice -> 'c
    val foldlEq : ('a * 'b * 'c -> 'c)
                    -> 'c -> 'a slice * 'b slice -> 'c
    val foldrEq : ('a * 'b * 'c -> 'c)
                    -> 'c -> 'a slice * 'b slice -> 'c
    val foldli   : (int * 'a * 'b * 'c -> 'c)
                    -> 'c -> 'a slice * 'b slice -> 'c
    val foldliEq : (int * 'a * 'b * 'c -> 'c)
                    -> 'c -> 'a slice * 'b slice -> 'c
    val foldriEq : (int * 'a * 'b * 'c -> 'c)
                    -> 'c -> 'a slice * 'b slice -> 'c
    val findi : (int * 'a * 'b -> bool) -> 'a slice * 'b slice
                -> (int * 'a * 'b) option
    val find : ('a * 'b -> bool) -> 'a slice * 'b slice
                -> ('a * 'b) option
    val findiAllEq: (int * 'a * 'b -> bool) -> 'a slice * 'b slice
                -> (int * 'a * 'b) list
    val findAllEq: ('a * 'b -> bool) -> 'a slice * 'b slice
                -> ('a * 'b) list
    val findiEq : (int * 'a * 'b -> bool) -> 'a slice * 'b slice
                -> (int * 'a * 'b) option
    val findEq : ('a * 'b -> bool) -> 'a slice * 'b slice
                -> ('a * 'b) option
    val rfindiEq : (int * 'a * 'b -> bool) -> 'a slice * 'b slice
                -> (int * 'a * 'b) option
    val rfind : ('a * 'b -> bool) -> 'a slice * 'b slice
                -> ('a * 'b) option
    val rfindEq : ('a * 'b -> bool) -> 'a slice * 'b slice
                -> ('a * 'b) option
    val all    : ('a * 'b -> bool) -> 'a slice * 'b slice -> bool
    val exists : ('a * 'b -> bool) -> 'a slice * 'b slice -> bool
    val allEq : ('a * 'b -> bool) -> 'a slice * 'b slice -> bool 
    val alli    : (int * 'a * 'b -> bool) -> 'a slice * 'b slice -> bool
    val existsi : (int * 'a * 'b -> bool) -> 'a slice * 'b slice -> bool
    val alliEq : (int * 'a * 'b -> bool) -> 'a slice * 'b slice -> bool 

end
    where type 'a slice = 'a VectorSlice.slice
    where type 'a vector = 'a Vector.vector
