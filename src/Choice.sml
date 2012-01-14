(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

structure Choice :> CHOICE = struct

    exception Choice

    datatype ('a, 'b) choice = Left of 'a | Right of 'b

    fun is_left (Left _) = true | is_left (Right _) = false

    fun is_right (Left _) = false | is_right (Right _) = true

    fun lval_of (Left x) = x | lval_of _ = raise Choice

    fun rval_of (Right x) = x | rval_of _ = raise Choice

    fun get_left (Left x, _) = x | get_left (_, y) = y

    fun get_right (Right x, _) = x | get_right (_, y) = y

    fun partition p (f, g) x =
        if p x then Left (f x) else Right (g x)

    fun map (f, _) (Left y) = Left (f y)
      | map (_, g) (Right z) = Right (g z)

end
