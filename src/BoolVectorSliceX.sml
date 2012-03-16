(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

structure BoolVectorSliceX :>
          MONO_VECTOR_SLICE_X
          where type elem = bool
          where type vector = BoolVector.vector =
struct
    structure V = BoolVector
    open V
    open LibBase
    open IterateX
    val filter = Option.filter
    fun op <- (a, (i, x)) = Array.update (a, i, x)
    infix 4 <-
    val op //? = Array.sub
    infix 8 //?

    fun forgeta3 f (i, y, _) = f (i, y)

    type elem = bool
    type vector = V.vector
    type slice = {base: vector, start: int, len: int}

    fun length {len=l,...} = l

    fun sub ({base=v, start=s, len=l}, i) =
        if i < 0 orelse l <= i then raise Subscript
        else V.sub (v, s + i)

    val op //! = sub
    infix 8 //!

    fun full v = {base=v, start=0, len=V.length v}

    fun slice (v, start, SOME l) =
        if start < 0 orelse l < 0 orelse V.length v < start + l then
            raise Subscript
        else {base=v, start=start, len=l}
      | slice (v, start, NONE) = 
        if start < 0 orelse V.length v < start then raise Subscript
        else {base=v, start=start, len=V.length v - start}

    fun subslice ({base=v, start=s, len=l}, s', SOME l') =
        if s' < 0 orelse l' < 0 orelse l < s' + l' then
            raise Subscript
        else {base=v, start=s + s', len=l'}
      | subslice ({base=v, start=s, len=l}, s', NONE) = 
        if s' < 0 orelse l < s' then raise Subscript
        else {base=v, start=s + s', len=l - s'}

    fun base {base=v, start=s, len=l} = (v, s, l)

    fun isEmpty {len=0,...} = true | isEmpty _ = false

    fun getItem {len=0,...} = NONE
      | getItem {base=v, start=s, len=l} =
        SOME (V.sub (v, s), {base=v, start=s + 1, len=l - 1})

    fun mapi f s = V.tabulate (length s, fn i => f (i, s //! i))

    fun map f s = V.tabulate (length s, fn i => f $ s //! i)

    fun vector sl = map id sl

    fun foldli f x s = repeat (fn (i, v) => f (i, s //! i, v)) (length s) x

    fun foldl f x s = repeat (fn (i, v) => f (s //! i, v)) (length s) x

    fun appi f s = foldli (forgeta3 f) () s

    fun app f s = foldl (f o #1) () s

    fun findi f s = uptoUntil (fn i => filter f (i, s //! i)) $ length s

    fun find f s = uptoUntil (fn i => filter f (s //! i)) $ length s

    fun exists f s = isSome $ find f s

    fun all f s = not $ isSome $ find (not o f) s

    fun collate f (s1 as {len=l1,...}, s2 as {len=l2,...}) =
        let fun check i =
                case f (s1 //! i, s2 //! i) of EQUAL => NONE | ord => SOME ord
            val ls = (l1, l2)
        in getOpt (uptoUntil check (Int.min ls), Int.compare ls) end

    fun foldri f x (s as {len=l,...}) =
        let fun `i = l - 1 - i
        in repeat (fn (i, v) => f (`i, s //! `i, v)) l x end

    fun foldr f x (s as {len=l,...}) =
        repeat (fn (i, v) => f (s //! (l - 1 - i), v)) l x

    fun rfindi f s =
        downfromUntil (fn i => filter f (i, s //! i)) $ length s

    fun rfind f s =
        downfromUntil (fn i => filter f (s //! i)) $ length s

    (* There's no VectorSlicePair, unfortunately. Should there be? *)
    fun rcollate f (s1, s2) =
        let val ls = (length s1, length s2)
            fun check (i, j) =
                case f (s1 //! i, s2 //! j) of EQUAL => NONE | ord => SOME ord
        in getOpt (downfromUntil2 check ls, Int.compare ls) end

    fun existsi f s = isSome $ findi f s

    fun alli f s = not $ isSome $ findi (not o f) s

    fun concat ss =
        let val ltot = List.foldl (fn (sl, n) => n + length sl) 0 ss
            val zilch = V.tabulate (0, fn _ => raise Impossible "empty vector")
            val filler = (slice (zilch, 0, NONE), 0)
            val a = Array.array (ltot, filler)
            fun setptr (_, ([], _)) = ()
              | setptr (i, (l as s::t, j)) =
                if j = length s then setptr (i, (t, 0))
                else (a <- (i, (s, j)); setptr (i + 1, (l, j + 1)))
        in setptr (0, (ss, 0))
         ; V.tabulate (ltot, fn i => #1 (a //? i) //! #2 (a //? i))
        end

    fun append (s, s') = concat [s, s']

    fun concatWith sep vs = concat $ ListX.intersperse (full sep) vs

    fun tokens p s =
        let val (_, i0, _) = base s
            fun mksl (i, l) = subslice (s, i, SOME l)
            fun prepend (i, x, []) = if p x then [] else [mksl (i, 1)]
              | prepend (i, x, toks as t :: ts) =
                if p x then toks else
                let val (_, j, l) = base t in
                    if i0 + i < j - 1 then mksl (i, 1) :: toks
                    else mksl (i, l + 1) :: ts
                end
        in foldri prepend [] s end

    fun fields p s =
        let val (_, i0, l0) = base s
            fun mksl (i, l) = subslice (s, i, SOME l)
            fun prepend (x, []) =
                if p x then [mksl (l0, 0)]
                else [mksl (l0 - 1, 1)]
              | prepend (x, flds as f :: fs) =
                let val (_, j, l) = base f
                    val j' = j - i0 - 1
                in
                    if p x then mksl (j', 0) :: flds else mksl (j', l + 1) :: fs
                end
        in foldr prepend [] s end

end
