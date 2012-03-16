(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

(* Even the List module is missing some important functions. *)

signature LIST_X = sig

    include LIST

    val takeWhile : ('a -> bool) -> 'a list -> 'a list
    val dropWhile : ('a -> bool) -> 'a list -> 'a list
    val tabulateRec : ('a list -> 'a option) -> 'a list
    val intersperse: 'a -> 'a list -> 'a list

end
