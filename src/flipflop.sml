(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

(* Here I collect some basic structural operators, many of them compensating
 * for the regretable emphasis on pairing in SML as opposed to currying. *)

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

end

structure Flop : sig

    (* This is simply the function application.  It is reified like this
     * to change its precedence and associativity. *)
    val apply: ('a -> 'b) * 'a -> 'b

    (*  Left section.  For instance, [Array.sub (a, i)] can be written
     * [apply (lsect (a, Array.sub), i)]. *)
    val lsect: 'a * ('a * 'b -> 'c) -> 'b -> 'c

    (* Right section. *)
    val rsect: ('a * 'b -> 'c) * 'b -> 'a -> 'c
    (* Make me a curry! *)
    val curry: ('a * 'b -> 'c) -> 'a -> 'b -> 'c
    (* Eat the curry, and make me a tuple. *)
    val uncurry: ('a -> 'b -> 'c) -> ('a * 'b -> 'c)
    (* Triple curry *)
    val curry3: ('a * 'b * 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
    val uncurry3: ('a -> 'b -> 'c -> 'd) -> ('a * 'b * 'c -> 'd)

end = struct

    fun apply (f, x) = f x
    fun lsect (x, f) = fn y => f (x, y)
    fun rsect (f, y) = fn x => f (x, y)
    fun curry f x y = f (x, y)
    fun uncurry f (x, y) = f x y
    fun curry3 f x y z = f (x, y, z)
    fun uncurry3 f (x, y, z) = f x y z

end

(* See above for the motivation for these *)
infixr 3 $
val op $ = Flop.apply

infix 4 <| 
val op <| = Flop.lsect

infix 4 |>
val op |> = Flop.rsect
