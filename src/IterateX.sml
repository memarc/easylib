(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

structure IterateX :> ITERATE_X =
struct
    open Iterate
    
    fun upto_until f n =
        let exception Found of 'a
            fun f' (i, _) = Option.map (fn v => raise Found v) $ f i
        in repeat f' n NONE handle Found v => SOME v end

    fun downfrom_until f n =
        let exception Found of 'a
            fun f'' i = f (n - i - 1)
            fun f' (i, _) = Option.map (fn v => raise Found v) $ f'' i
        in repeat f' n NONE handle Found v => SOME v end

    fun downfrom_until2 f (m, n) =
        let exception Found of 'a
            fun f'' i = f (m - i - 1, n - i - 1)
            fun f' (i, _) = Option.map (fn v => raise Found v) $ f'' i
        in repeat f' (Int.min (m, n)) NONE handle Found v => SOME v end

end
