(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

signature VECTOR_PAIR = sig

    exception UnequalLengths
    val zip   : 'a vector * 'b vector -> ('a * 'b) vector
    val zipEq : 'a vector * 'b vector -> ('a * 'b) vector
    val unzip : ('a * 'b) vector -> 'a vector * 'b vector
    val app   : ('a * 'b -> unit) -> 'a vector * 'b vector -> unit
    val appEq : ('a * 'b -> unit) -> 'a vector * 'b vector -> unit
    val map   : ('a * 'b -> 'c) -> 'a vector * 'b vector -> 'c vector
    val mapEq : ('a * 'b -> 'c) -> 'a vector * 'b vector -> 'c vector
    val appi   : (int * 'a * 'b -> unit) -> 'a vector * 'b vector -> unit
    val appiEq : (int * 'a * 'b -> unit) -> 'a vector * 'b vector -> unit
    val mapi   : (int * 'a * 'b -> 'c) -> 'a vector * 'b vector -> 'c vector
    val mapiEq : (int * 'a * 'b -> 'c) -> 'a vector * 'b vector -> 'c vector
    val foldl   : ('a * 'b * 'c -> 'c)
                    -> 'c -> 'a vector * 'b vector -> 'c
    val foldr   : ('a * 'b * 'c -> 'c)
                    -> 'c -> 'a vector * 'b vector -> 'c
    val foldlEq : ('a * 'b * 'c -> 'c)
                    -> 'c -> 'a vector * 'b vector -> 'c
    val foldrEq : ('a * 'b * 'c -> 'c)
                    -> 'c -> 'a vector * 'b vector -> 'c
    val foldli   : (int * 'a * 'b * 'c -> 'c)
                    -> 'c -> 'a vector * 'b vector -> 'c
    val foldliEq : (int * 'a * 'b * 'c -> 'c)
                    -> 'c -> 'a vector * 'b vector -> 'c
    val foldriEq : (int * 'a * 'b * 'c -> 'c)
                    -> 'c -> 'a vector * 'b vector -> 'c
    val findi : (int * 'a * 'b -> bool) -> 'a vector * 'b vector
                -> (int * 'a * 'b) option
    val find : ('a * 'b -> bool) -> 'a vector * 'b vector
                -> ('a * 'b) option
    (* val findiAll: (int * 'a * 'b -> bool) -> 'a vector * 'b vector
     *             -> (int * 'a * 'b) list
     * val findAll: ('a * 'b -> bool) -> 'a vector * 'b vector
     *             -> ('a * 'b) list *)
    val findiEq : (int * 'a * 'b -> bool) -> 'a vector * 'b vector
                -> (int * 'a * 'b) option
    val findEq : ('a * 'b -> bool) -> 'a vector * 'b vector
                -> ('a * 'b) option
    val rfindiEq : (int * 'a * 'b -> bool) -> 'a vector * 'b vector
                -> (int * 'a * 'b) option
    val rfind : ('a * 'b -> bool) -> 'a vector * 'b vector
                -> ('a * 'b) option
    val rfindEq : ('a * 'b -> bool) -> 'a vector * 'b vector
                -> ('a * 'b) option
    val all    : ('a * 'b -> bool) -> 'a vector * 'b vector -> bool
    val exists : ('a * 'b -> bool) -> 'a vector * 'b vector -> bool
    val allEq : ('a * 'b -> bool) -> 'a vector * 'b vector -> bool 
    val alli    : (int * 'a * 'b -> bool) -> 'a vector * 'b vector -> bool
    val existsi : (int * 'a * 'b -> bool) -> 'a vector * 'b vector -> bool
    val alliEq : (int * 'a * 'b -> bool) -> 'a vector * 'b vector -> bool 

end
