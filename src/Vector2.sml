(* Copyright (C) 2012 Ian Zimmerman <itz@buug.org>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the conditions spelled out in
 * the file LICENSE are met. *)

structure Vector2 :> VECTOR2 =
struct
    open IterateX

    type 'a vector =
         { rows: int
         , cols: int
         , data: 'a Vector.vector
         }

    type 'a region =
         { base: 'a vector
         , row: int
         , col: int
         , nrows: int option
         , ncols: int option
         }

    datatype traversal = RowMajor | ColMajor

    fun vector (r, c, x) = 
        if r < 0 orelse c < 0 then raise Size
        else {rows=r, cols=c, data=VectorX.vector (r*c, x)}

    fun fromList (c, rows) =
        if c < 0 then raise Size
        else if List.null rows then
            let fun impossible _ = raise LibBase.Impossible "empty Vector2"
            in { rows = 0
               , cols = c
               , data = Vector.tabulate (0, impossible)
               }
            end
        else if List.all (fn r => List.length r = c) rows then
            { rows = List.length rows
            , cols = c
            , data = Vector.concat $ List.map Vector.fromList rows
            }
        else raise Size

    fun row ({cols=c, data=d,...}, i) =
        Vector.tabulate (c, fn j => d // (i * c + j))

    fun column ({cols=c, rows=r, data=d}, j) =
        Vector.tabulate (r, fn i => d // (i * c + j))

    fun toList RowMajor (v as {rows=r,...}) =
        List.tabulate (r, fn i => VectorX.toList $ row (v, i))
      | toList ColMajor (v as {cols=c,...}) =
        List.tabulate (c, fn i => VectorX.toList $ column (v, i))

    fun tabulate tr (r, c, f) =
        if r < 0 orelse c < 0 then raise Size
        else case tr
              of RowMajor =>
                 { rows = r
                 , cols = c
                 , data = Vector.tabulate (r * c, fn i => f (i div c, i mod c))
                 }
               | ColMajor =>
                 let fun makecol j = Vector.tabulate (r, f |> j)
                     val col_vecs = Vector.tabulate (c, makecol)
                     fun select i = col_vecs // (i mod c) // (i div c)
                 in { rows = r
                    , cols = c
                    , data = Vector.tabulate (r * c, select)
                    }
                 end

    fun sub (v as {cols=c, data=d,...}, i, j) = d // (i * c + j)

    fun dimensions {rows=r, cols=c,...} = (r, c)

    fun nRows {rows=r,...} = r

    fun nCols {cols=c,...} = c

    fun map tr f (v as {rows=r, cols=c, ...}) =
        tabulate tr (r, c, fn (i, j) => f $ sub (v, i, j))

    fun app tr f (v as {rows=r, cols=c,...}) = 
        let fun select i =
                let val (j, k) = case tr
                                  of RowMajor => (i div c, i mod c)
                                   | ColMajor => (i mod r, i div r)
                in sub (v, j, k) end
        in repeat (f o select o #1) (r * c) () end

    fun fold tr f x (v as {rows=r, cols=c, ...}) =
        let fun foldval (i, z) =
                let val (j, k) = case tr
                                  of RowMajor => (i div c, i mod c)
                                   | ColMajor => (i mod r, i div r)
                in f (sub (v, j, k), z) end
        in repeat foldval (r * c) x end

    (* Copied from polyml/basis/Array2.sml *)

    (* Internal function.  Check that the region is valid and get
       the actual lengths. *)
    fun getRegion {base, row, col, nrows, ncols} =
    let
        val (lRows, lCols) = dimensions base
        val nrows' =
            case nrows of
                NONE =>
                    if row < 0 orelse row > lRows
                    then raise Subscript
                    else lRows - row
            |   SOME r =>
                    if r < 0 orelse row < 0 orelse r+row > lRows
                    then raise Subscript
                    else r
        val ncols' =
            case ncols of
                NONE =>
                    if col < 0 orelse col > lCols
                    then raise Subscript
                    else lCols - col
            |   SOME c =>
                    if c < 0 orelse col < 0 orelse c+col > lCols
                    then raise Subscript
                    else c
    in
        (nrows', ncols')
    end

    fun mapi tr f (reg as {base, row, col, ...}) =
        let val (nrows, ncols) = getRegion reg
            fun select (i, j) = 
                let val (i', j') = (i + row, j + col)
                in (i', j', sub (base, i', j')) end
        in tabulate tr (nrows, ncols, f o select) end

    fun foldi tr f x (reg as {base, row, col, ...}) =
        let val (nrows, ncols) = getRegion reg
            fun foldval (i, z) =
                let val (j, k) =
                        case tr
                         of RowMajor => (row + i div ncols, col + i mod ncols)
                          | ColMajor => (row + i mod nrows, col + i div nrows)
                in f (j, k, sub (base, j, k), z) end
        in repeat foldval (nrows * ncols) x end

    fun appi tr f (reg as {base, row, col, ...}) =
        let val (nrows, ncols) = getRegion reg
            fun foldval (i, _) =
                let val (j, k) =
                        case tr
                         of RowMajor => (row + i div ncols, col + i mod ncols)
                          | ColMajor => (row + i mod nrows, col + i div nrows)
                in f (j, k, sub (base, j, k)) end
        in repeat foldval (nrows * ncols) () end


end
