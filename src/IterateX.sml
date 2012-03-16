(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

structure IterateX :> ITERATE_X =
struct
    open Iterate

    fun find (f, n) =
        let exception Found of 'a
            fun f' (i, _) = Option.map (fn v => raise Found v) $ f i
        in repeat f' n NONE handle Found v => SOME v end
    
    fun uptoUntil f n = find (f, n)

    fun downfromUntil f n = find (fn i => f (n - 1 - i), n)

    fun downfromUntil2 f (m, n) =
        let fun f' i = f (m - 1 - i, n - 1 - i) in find (f', Int.min (m, n)) end

end
