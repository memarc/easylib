(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

signature VECTOR_SLICE_X = sig

    include VECTOR_SLICE

    val append : 'a slice * 'a slice -> 'a Vector.vector

    val concatWith: 'a Vector.vector -> 'a slice list -> 'a Vector.vector

    val rfindi : (int * 'a -> bool)
                  -> 'a slice -> (int * 'a) option
    val rfind  : ('a -> bool) -> 'a slice -> 'a option
    val existsi : (int * 'a -> bool) -> 'a slice -> bool
    val alli : (int * 'a -> bool) -> 'a slice -> bool
    val rcollate : ('a * 'a -> order)
                    -> 'a slice * 'a slice -> order
    val tokens: ('a -> bool) -> 'a slice -> 'a slice list
    val fields: ('a -> bool) -> 'a slice -> 'a slice list

end
    where type 'a slice = 'a VectorSlice.slice

