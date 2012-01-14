
(* Taken from http://mlton.org/Fold *)

signature FOLD = sig

    val fold : 'a * 'b -> ('a * 'b -> 'c) -> 'c

    val step0 : ('a -> 'b) -> 'a * 'c -> ('b * 'c -> 'd) -> 'd

    val step1: ('a * 'b -> 'c) -> 'b * 'd -> 'a -> ('c * 'd -> 'e) -> 'e

    val end_ : 'a * ('a -> 'b) -> 'b

end
