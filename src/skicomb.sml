(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

(* Basic combinators *)

structure Skicomb : sig

    val i: 'a -> 'a
    val k: 'b -> ('a -> 'b)
    val s: 'a -> ('a -> 'b -> 'c) * ('a -> 'b) -> 'c

end = struct

    fun i x = x
    fun k x y = x
    fun s z (f, g) = (f z) (g z)

end
