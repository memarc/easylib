(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

structure Flip : sig

    (* swap the members of a tuple *)
    val swap: 'a * 'b -> 'b * 'a
    (* same with a triple, for all choices of the 2 members *)
    val swap3_12: 'a * 'b * 'c -> 'b * 'a * 'c
    val swap3_23: 'a * 'b * 'c -> 'a * 'c * 'b
    val swap3_31: 'a * 'b * 'c -> 'c * 'b * 'a
    (* transform a function by permuting its arguments *)
    val flip: ('a * 'b -> 'c) -> ('b * 'a -> 'c)
    val flip3_12: ('a * 'b * 'c -> 'd) -> ('b * 'a * 'c -> 'd)
    val flip3_23: ('a * 'b * 'c -> 'd) -> ('a * 'c * 'b -> 'd)
    val flip3_31: ('a * 'b * 'c -> 'd) -> ('c * 'b * 'a -> 'd)
    (* functions for injection into a cross product *)
    val linject: 'a -> 'b -> 'a * 'b
    val rinject: 'b -> 'a -> 'a * 'b
    val inject3_1: 'a -> 'b * 'c -> 'a * 'b *'c
    val inject3_2: 'b -> 'a * 'c -> 'a * 'b * 'c
    val inject3_3: 'c -> 'a * 'b -> 'a * 'b * 'c

end = struct

    fun swap (x, y) = (y, x)
    fun swap3_12 (x, y, z) = (y, x, z)
    fun swap3_23 (x, y, z) = (x, z, y)
    fun swap3_31 (x, y, z) = (z, y, x)
    fun flip f (x, y) = f (y, x)
    fun flip3_12 f (x, y, z) = f (y, x, z)
    fun flip3_23 f (x, y, z) = f (x, z, y)
    fun flip3_31 f (x, y, z) = f (z, y, x)
    fun linject x y = (x, y)
    fun rinject y x = (x, y)
    fun inject3_1 x (y, z) = (x, y, z)
    fun inject3_2 y (x, z) = (x, y, z)
    fun inject3_3 z (x, y) = (x, y, z)

    fun drop3 (_, y, y') = (y, y')
    fun drop4 (_, y, y', z) = (y, y', z)
    fun drop3' (y, y', _) = (y, y')
    fun drop4' (i, y, y', _) = (i, y, y')

end
