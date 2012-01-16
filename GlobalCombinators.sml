(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

(* Make me a curry! *)
fun curry f x y = f (x, y)
(* Eat the curry, and make me a tuple. *)
fun uncurry f (x, y) = f x y

(* transform a function by permuting its arguments *)
fun flip f (x, y) = f (y, x)

(* S, K, I combinators *)
fun subst z (f, g) = (f z) (g z)

fun id x = x

fun konst y x = y

(* This is simply the function application.  It is reified like this
 * to change its precedence and associativity. *)
fun op $ (f, x) = f x
infixr 3 $

(* Right on left application.  Useful for folds. *)
fun op >$ (x, f) = f x

(*  Left section.  For instance, [Array.sub (a, i)] can be written
 * [$ (<| (a, Array.sub), i)]. *)
fun op <| (x, f) = fn y => f (x, y)
infix 4 <| 

(* Right section. *)
fun op |> (f, y) = fn x => f (x, y)
infix 4 |>

(* defined in Product module *)
val op & = Product.&
infix 0 &

(* Other global operators *)
val op // = Vector.sub
infix 8 //

val op //: = VectorSlice.sub
infix 8 //:

(* Operators for reals with different names to sidestep overloading mess *)

val op +: = Real.+
infix 6 +:

val op -: = Real.-
infix 6 -:

val op *: = Real.*
infix 7 *:

val op /: = Real./
infix 7 /:

val ~: = Real.~

val absr = Real.abs

val op <:  = Real.<
infix 4 <:

val op >: = Real.>
infix 4 >:

val op <=: = Real.<=
infix 4 <=:

val op >=: = Real.>=
infix 4 >=:
