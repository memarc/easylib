local
    open RuntimeCalls
    val System_loadw: word*int->word = RunCall.run_call2 POLY_SYS_load_word;
    val System_loadb: string*word->char = RunCall.run_call2 POLY_SYS_load_byte;
    val System_setw: word * int * word -> unit   = RunCall.run_call3 POLY_SYS_assign_word;
    val wordSize : word = RunCall.run_call0 RuntimeCalls.POLY_SYS_bytes_per_word ();

    val vecAsWord: 'a vector -> word = RunCall.unsafeCast

    (* Predeclared in the basis with special equality props. *)
    type 'a array = 'a array 

    (* Unsafe subscript and update functions used internally for
       cases where we've already checked the range.  N.B.  THESE ADD
       THE ONE WHICH IS NECESSARY TO SKIP THE LENGTH WORD *)

    fun unsafeSubArray(v: 'a array, i: int): 'a =
        RunCall.unsafeCast(System_loadw (RunCall.unsafeCast v, i+1))

    and unsafeUpdateArray(v: 'a array, i: int, new: 'a): unit =
        System_setw (RunCall.unsafeCast v, i+1, RunCall.unsafeCast new);

    fun unsafeSubVector(v: 'a vector, i: int): 'a =
        RunCall.unsafeCast(System_loadw (vecAsWord v, i))

    fun unsafeUpdateVector(v: 'a vector, i: int, new : 'a) =
        System_setw (vecAsWord v, i, RunCall.unsafeCast new);

    (* If a vector/string is short (i.e. has an integer tag) it must be the character
       itself rather than a pointer to a segment. *)
    val isShortString: string -> bool = RunCall.run_call1 POLY_SYS_is_short

    fun unsafeSubString(s: string, i: int): char =
        if isShortString s then RunCall.unsafeCast s
        else System_loadb(s, Word.fromInt i + wordSize);

in
    structure EasyUnsafe = struct
        structure Array = struct
            val sub = unsafeSubArray
            val update = unsafeUpdateArray
        end;
        structure Vector = struct
            val sub = unsafeSubVector
            val update = unsafeUpdateVector
        end;
        structure CharVector = struct
            val sub = unsafeSubString
        end
    end
end
