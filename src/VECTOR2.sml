(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

(* There's ARRAY2, but no VECTOR2.  Why? *)

signature VECTOR2 =
sig

    type 'a vector

    type 'a region =
         { base: 'a vector
         , row: int
         , col: int
         , nrows: int option
         , ncols: int option
         }

    datatype traversal = RowMajor | ColMajor

    val vector: int * int * 'a -> 'a vector
    (* The ARRAY2 signature has no columns argument.
     * How can a 0 * m array be created then? *)
    val fromList: int * 'a list list -> 'a vector
    val toList: traversal -> 'a vector -> 'a list list
    val tabulate: traversal -> int * int * (int * int -> 'a) -> 'a vector
    val sub: 'a vector * int * int -> 'a
    val dimensions: 'a vector -> int * int
    val nCols: 'a vector -> int
    val nRows: 'a vector -> int
    val row: 'a vector * int -> 'a Vector.vector
    val column: 'a vector * int -> 'a Vector.vector
    val map: traversal -> ('a -> 'a) -> 'a vector -> 'a vector
    val mapi: traversal -> (int * int * 'a -> 'a) -> 'a region -> 'a vector
    val appi: traversal -> (int * int * 'a -> unit) -> 'a region -> unit
    val app : traversal -> ('a -> unit) -> 'a vector -> unit
    val foldi: traversal -> (int * int * 'a * 'b -> 'b) -> 'b -> 'a region -> 'b
    val fold : traversal -> ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
end
