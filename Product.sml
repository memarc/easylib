(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

structure Product :> PRODUCT =
struct
    datatype ('a, 'b) product = & of 'a * 'b

    fun lpro (& (x, y)) = x

    fun rpro (& (x, y)) = y

    fun withl (& (x, y), x') = & (x', y)

    fun withr (& (x, y), y') = & (x, y')

    fun filter p (f, g) (& (x, y)) = 
        let val x' = f x
        in if p x' then x' else g y end

    fun map (f, g) (& (x, y)) = & (f x, g y)
end
