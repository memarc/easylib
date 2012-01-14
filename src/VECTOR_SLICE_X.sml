(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

signature VECTOR_SLICE_X = sig

    include VECTOR_SLICE

    val append : 'a slice * 'a slice -> 'a Vector.vector


    val findi_r : (int * 'a -> bool)
                  -> 'a slice -> (int * 'a) option
    val find_r  : ('a -> bool) -> 'a slice -> 'a option
    val existsi : (int * 'a -> bool) -> 'a slice -> bool
    val alli : (int * 'a -> bool) -> 'a slice -> bool
    val collate_r : ('a * 'a -> order)
                    -> 'a slice * 'a slice -> order
end
    where type 'a slice = 'a VectorSlice.slice

