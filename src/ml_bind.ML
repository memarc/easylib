(* emacs: -*-SML-*- *)

(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

val op //: = VectorSlice.sub
infix 8 //:

val id : 'a -> 'a = fn x => x

val konst : 'b -> 'a -> 'b = fn y => fn x => y

val >$ = Fold.end_           (* not an infix *)
