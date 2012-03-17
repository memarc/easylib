(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

signature MONO_VECTOR_X = sig

    include MONO_VECTOR

    val vector : int * elem -> vector

    val concatWith : vector -> vector list -> vector

    val translate: (elem -> vector) -> vector -> vector

    val rfind : (elem -> bool) -> vector -> elem option

    val rfindi : (int * elem -> bool) -> vector -> (int * elem) option

    val findAll: (elem -> bool) -> vector -> elem list

    val findiAll: (int * elem -> bool) -> vector -> (int * elem) list

    val append : vector * vector -> vector

    val toList : vector -> elem list

    val rcollate : (elem * elem -> order) -> vector * vector -> order

    val existsi : (int * elem -> bool) -> vector -> bool

    val alli : (int * elem -> bool) -> vector -> bool

end
