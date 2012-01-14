(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

(** The [Choice] module provides a sum type and functions operating on it. **)

signature CHOICE = sig

    exception Choice

    (** The sum type; **)
    datatype ('a, 'b) choice = Left of 'a | Right of 'b

    (** [is_left x] tests if [x] belongs to the left part of the sum. **)
    val is_left : ('b, 'c) choice -> bool

    (** [is_right x] tests if [x] belongs to the right part of the sum. **)
    val is_right :('b, 'c) choice -> bool

    (** [lval_of x] extracts the contents of [x] provided it belongs to the
     * left part of the sum.  If it does not, raise [Choice]. **)
    val lval_of : ('a, 'b) choice -> 'a

    (** [rval_of x] extracts the contents of [x] provided it belongs to the
     * right part of the sum.  If it does not, raise [Choice]. **)
    val rval_of : ('a, 'b) choice -> 'b

    (** [get_left (x, y)] extracts the contents of [x] provided it belongs to the
     * left part of the sum.  If it does not, returns [y]. **)
    val get_left : ('a, 'b) choice * 'a -> 'a

    (** [get_right (x, y)] extracts the contents of [x] provided it belongs to the
     * right part of the sum.  If it does not, returns [y]. **)
    val get_right : ('a, 'b) choice * 'b -> 'b

    (** [partition p (f, g) x] returns [Left (f x)] if [p x] is [true],
     * otherwise it returns [Right (g x)]. **)
    val partition :
        ('a -> bool) -> ('a -> 'b) * ('a -> 'c) -> 'a -> ('b, 'c) choice

    (** [map (f, g) x] returns [Left (f y)] if [x] is of the form [Left y].
     * If [x] is of the form [Right z], it returns [Right (g z)]. **)
    val map : ('a -> 'b) * ('c -> 'd) -> ('a, 'c) choice -> ('b, 'd) choice

end