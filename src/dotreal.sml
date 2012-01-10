(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

(* Operators for reals with different names to sidestep overloading mess *)

fun op +: (x: real, y: real) = x + y
infix 6 +:

fun op -: (x: real, y: real) = x - y
infix 6 -:

fun op *: (x: real, y: real) = x * y
infix 7 *:

fun op /: (x: real, y: real) = x / y
infix 7 /:

fun ~: (x: real) = ~ x

fun abs_r (x: real) = abs x

fun op <: (x: real, y: real) = x < y
infix 4 <:

fun op >: (x: real, y: real) = x > y
infix 4 >:

fun op <=: (x: real, y: real) = x <= y
infix 4 <=:

fun op >=: (x: real, y: real) = x >= y
infix 4 >=:
