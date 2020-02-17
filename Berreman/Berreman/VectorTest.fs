module VectorTest

type IDimension =
  interface 
    abstract Size : int
  end

type Dim1 () = class interface IDimension with member x.Size = 1 end end
type Dim2 () = class interface IDimension with member x.Size = 2 end end
type Dim3 () = class interface IDimension with member x.Size = 3 end end
type Dim4 () = class interface IDimension with member x.Size = 4 end end
type Dim5 () = class interface IDimension with member x.Size = 5 end end
type Dim6 () = class interface IDimension with member x.Size = 6 end end
type Dim7 () = class interface IDimension with member x.Size = 7 end end
type Dim8 () = class interface IDimension with member x.Size = 8 end end


type Vector<'Dim  when  'Dim :> IDimension 
                  and   'Dim : (new : unit -> 'Dim)
           > () =
  class
    let dim = new 'Dim()

    let vs  = Array.zeroCreate<float> dim.Size

    member x.Dim    = dim
    member x.Values = vs
  end

type Matrix<'RowDim, 'ColumnDim when  'RowDim :> IDimension 
                                and   'RowDim : (new : unit -> 'RowDim) 
                                and   'ColumnDim :> IDimension 
                                and   'ColumnDim : (new : unit -> 'ColumnDim)
           > () =
  class
    let rowDim    = new 'RowDim()
    let columnDim = new 'ColumnDim()

    let vs  = Array.zeroCreate<float> (rowDim.Size * columnDim.Size)

    member x.RowDim     = rowDim
    member x.ColumnDim  = columnDim
    member x.Values     = vs
  end


let apply (m : Matrix<'R, 'C>) (v : Vector<'C>) : Vector<'R> =
    failwith ""


let m76 = Matrix<Dim7, Dim6> ()
let v6  = Vector<Dim6> ()
let v7  = apply m76 v6 // Vector<Dim7>

// Doesn't compile because v7 has the wrong dimension
let vv = apply m76 v7
