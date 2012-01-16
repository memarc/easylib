(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

signature SUM =
sig

    exception Sum

    (** The sum type; **)
    datatype ('a, 'b) sum = Inl of 'a | Inr of 'b

    (** [isl x] tests if [x] belongs to the left part of the sum. **)
    val isl: ('a, 'b) sum -> bool

    (** [isr x] tests if [x] belongs to the right part of the sum. **)
    val isr: ('a, 'b) sum -> bool

    (** [lval x] extracts the contents of [x] provided it belongs to the
     * left part of the sum.  If it does not, raise [Sum]. **)
    val lval: ('a, 'b) sum -> 'a

    (** [rval x] extracts the contents of [x] provided it belongs to the
     * right part of the sum.  If it does not, raise [Sum]. **)
    val rval: ('a, 'b) sum -> 'b

    (** [getl (x, y)] extracts the contents of [x] provided it belongs to the
     * left part of the sum.  If it does not, returns [y]. **)
    val getl: ('a, 'b) sum * 'a -> 'a

    (** [getr (x, y)] extracts the contents of [x] provided it belongs to the
     * right part of the sum.  If it does not, returns [y]. **)
    val getr: ('a, 'b) sum * 'b -> 'b

    (** [partition p (f, g) x] returns [Inl (f x)] if [p x] is [true],
     * otherwise it returns [Inr (g x)]. **)
    val partition: ('a -> bool) -> ('a -> 'b) * ('a -> 'c) -> 'a -> ('b, 'c) sum

    (** [map (f, g) x] returns [Inl (f y)] if [x] is of the form [Inl y].
     * If [x] is of the form [Inr z], it returns [Inr (g z)]. **)
    val map: ('a -> 'b) * ('c -> 'd) -> ('a, 'c) sum -> ('b, 'd) sum

end
