(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

(* Extra vector functions *)

signature VECTOR_X = sig

    include VECTOR

    val vector : int * 'a -> 'a vector

    val concatWith : 'a vector -> 'a vector list -> 'a vector

    val translate: ('a -> 'a vector) -> 'a vector -> 'a vector

    val find_r : ('a -> bool) -> 'a vector -> 'a option

    val findi_r : (int * 'a -> bool) -> 'a vector -> (int * 'a) option

    val append : 'a vector * 'a vector -> 'a vector

    val toList : 'a vector -> 'a list

    val collate_r : ('a * 'a -> order) -> 'a vector * 'a vector -> order

    val existsi : (int * 'a -> bool) -> 'a vector -> bool

    val alli : (int * 'a -> bool) -> 'a vector -> bool

end
    where type 'a vector = 'a Vector.vector
