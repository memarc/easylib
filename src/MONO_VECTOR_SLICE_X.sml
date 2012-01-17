(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

signature MONO_VECTOR_SLICE_X = sig

    include MONO_VECTOR_SLICE

    val append : slice * slice -> vector


    val findi_r : (int * elem -> bool)
                  -> slice -> (int * elem) option
    val find_r  : (elem -> bool) -> slice -> elem option
    val existsi : (int * elem -> bool) -> slice -> bool
    val alli : (int * elem -> bool) -> slice -> bool
    val collate_r : (elem * elem -> order)
                    -> slice * slice -> order
end
