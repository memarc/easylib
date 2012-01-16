(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

signature PRODUCT =
sig
    datatype ('a, 'b) product = & of 'a * 'b

    val lpro: ('a, 'b) product -> 'a

    val rpro: ('a, 'b) product -> 'b

    val withl: ('a, 'b) product * 'a -> ('a, 'b) product

    val withr: ('a, 'b) product * 'b -> ('a, 'b) product

    val filter: ('c -> bool) -> ('a -> 'c) * ('b -> 'c)
                   -> ('a, 'b) product -> 'c

    val map: ('a -> 'c) * ('b -> 'd) -> ('a, 'b) product -> ('c, 'd) product
end
