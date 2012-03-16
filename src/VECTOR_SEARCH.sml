(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

(* Vector stuff that requires equality on elements *)

signature VECTOR_SEARCH =
sig

    val first: ''a -> ''a vector -> int option
    val last: ''a -> ''a vector -> int option
    val isPrefix: ''a vector -> ''a vector -> bool
    val isSuffix: ''a vector -> ''a vector -> bool
    val isSub: ''a vector -> ''a vector -> bool

end
