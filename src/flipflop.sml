(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

(* Here I collect some basic structural operators, many of them compensating
 * for the regretable emphasis on pairing in SML as opposed to currying. *)


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
