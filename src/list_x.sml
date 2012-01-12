(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

(* Even the List module is missing some important functions. *)

signature LIST_X = sig

    include LIST

    val take_while : ('a -> bool) -> 'a list -> 'a list
    val drop_while : ('a -> bool) -> 'a list -> 'a list
    val rec_tabulate : ('a list -> 'a option) -> 'a list

end

structure ListX :> LIST_X = struct

    open List

    fun take_while f [] = []
      | take_while f (x::xs) =
        if f x then x :: take_while f xs else []

    fun drop_while f [] = []
      | drop_while f (l as x::xs) =
        if f x then drop_while f xs else l

    fun rec_tabulate f =
        let fun loop l =
                case f l of NONE => l | SOME x => loop (x :: l)
        in loop [] end

end
