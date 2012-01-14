(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

signature GLOBAL_COMBINATORS =
  sig
    val |> : ('a * 'b -> 'c) * 'b -> 'a -> 'c
    val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
    val subst : 'a -> ('a -> 'b -> 'c) * ('a -> 'b) -> 'c
    val konst : 'a -> 'b -> 'a
    val id : 'a -> 'a
    val flip : ('a * 'b -> 'c) -> 'b * 'a -> 'c
    val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
    val <| : 'a * ('a * 'b -> 'c) -> 'b -> 'c
    val $ : ('a -> 'b) * 'a -> 'b
  end
