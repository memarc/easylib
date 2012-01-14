(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

structure VectorSupport : sig

    val upto_until : (int -> 'a option) -> int -> 'a option

    val upto_until2 : (int * int -> 'a option) -> (int * int) -> 'a option

    val downfrom_until : (int -> 'a option) -> int -> 'a option

    val downfrom_until2 : (int * int -> 'a option) -> (int * int) -> 'a option

end = struct

    fun upto_until f n =
        let fun loop 0 = NONE
              | loop i =
                let val v = f (n - i)
                in if isSome v then v else loop (i - 1) end
        in loop n end

    fun upto_until2 f (m, n) =
        let fun loop (0, _) = NONE
              | loop (_, 0) = NONE
              | loop (i, j) =
                let val v = f (m - i, n - j)
                in if isSome v then v else loop (i - 1, j - 1) end
        in loop (m, n) end

    fun downfrom_until f n =
        let fun loop ~1 = NONE
              | loop i =
                let val v = f i in if isSome v then v else loop (i - 1) end
        in loop (n - 1) end

    fun downfrom_until2 f (m, n) =
        let fun loop (~1, _) = NONE
              | loop (_, ~1) = NONE
              | loop (i, j) =
                let val v = f (i, j)
                in if isSome v then v else loop (i - 1, j - 1) end
        in loop (m - 1, n - 1) end

end

