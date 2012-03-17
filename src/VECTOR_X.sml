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

    val rfind : ('a -> bool) -> 'a vector -> 'a option

    val rfindi : (int * 'a -> bool) -> 'a vector -> (int * 'a) option

    val findAll: ('a -> bool) -> 'a vector -> 'a list

    val findiAll: (int * 'a -> bool) -> 'a vector -> (int * 'a) list

    val append : 'a vector * 'a vector -> 'a vector

    val toList : 'a vector -> 'a list

    val rcollate : ('a * 'a -> order) -> 'a vector * 'a vector -> order

    val existsi : (int * 'a -> bool) -> 'a vector -> bool

    val alli : (int * 'a -> bool) -> 'a vector -> bool

end
    where type 'a vector = 'a Vector.vector
