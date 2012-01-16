(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

signature ITERATE_X =
sig
    include ITERATE
    val upto_until: (int -> 'a option) -> int -> 'a option
    val downfrom_until: (int -> 'a option) -> int -> 'a option
    val downfrom_until2: (int * int -> 'a option) -> (int * int) -> 'a option
end
