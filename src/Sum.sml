(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

structure Sum :> SUM = struct

    exception Sum

    datatype ('a, 'b) sum = Inl of 'a | Inr of 'b

    fun isl (Inl _) = true | isl (Inr _) = false

    fun isr (Inl _) = false | isr (Inr _) = true

    fun lval (Inl x) = x | lval _ = raise Sum

    fun rval (Inr x) = x | rval _ = raise Sum

    fun getl (Inl x, _) = x | getl (_, y) = y

    fun getr (Inr x, _) = x | getr (_, y) = y

    fun partition p (f, g) x =
        if p x then Inl (f x) else Inr (g x)

    fun map (f, _) (Inl y) = Inl (f y)
      | map (_, g) (Inr z) = Inr (g z)

end
