namespace Berreman

// The purpose of this module is to abstract away differences in vector / matrix libraries.
// Switching between them is VERY painful.
module MathNetNumericsMath =
    open System.Numerics
    open MathNet.Numerics
    open MathNet.Numerics.LinearAlgebra

    let complexZero = Complex(0.0, 0.0)
    let complexOne = Complex(1.0, 0.0)
    let complexMinusOne = Complex(-1.0, 0.0)
    let complexI = Complex(0.0, 1.0)
    let complexMinusI = Complex(0.0, -1.0)


    type Complex
        with
        member this.conjugate
            with get () = Complex.Conjugate this

        member this.abs
            with get () = Complex.Abs this

        /// The complex number with real part r and no imaginary part.
        static member ofReal (r : double) : Complex = Complex(r, 0.0)

        /// The complex number r + i * im.
        static member create (r : double) (im : double) : Complex = Complex(r, im)


    [<Softellect.Sys.NumericCarrier("a MathNet vector: its elements are the solver's numbers (R3)")>]
    type RealVector =
        | RealVector of Vector<double>
        member this.Item
            with get (i: int) =
                let (RealVector v) = this
                v.[i]

        static member create (a : #seq<double>) = vector a |> RealVector
        static member (+) (RealVector a, RealVector b) = a + b |> RealVector
        static member (-) (RealVector a, RealVector b) = a - b |> RealVector

        member this.norm =
            let (RealVector v) = this
            v * v |> sqrt


    [<Softellect.Sys.NumericCarrier("a MathNet vector: its elements are the solver's numbers (R3)")>]
    type ComplexVector =
        | ComplexVector of Vector<Complex>
        static member (*) (ComplexVector a, ComplexVector b) = a.DotProduct(b)
        static member (*) ((a : Complex), ComplexVector b) = (a * b) |> ComplexVector
        static member (*) (a : ComplexVector, (b : Complex)) = b * a
        static member (+) (ComplexVector a, ComplexVector b) = (a + b) |> ComplexVector
        static member (-) (ComplexVector a, ComplexVector b) = (a - b) |> ComplexVector
        static member create (a : #seq<Complex>) = vector a |> ComplexVector

        static member fromRe (a : #seq<double>) =
            a
            |> Seq.map Complex.ofReal
            |> vector
            |> ComplexVector

        static member fromIm (a : #seq<double>) =
           (Complex.create 0. 1.) * (ComplexVector.fromRe a)

        member this.Item
            with get (i: int) =
                let (ComplexVector v) = this
                v.[i]

        member this.conjugate =
            let (ComplexVector v) = this
            v.Conjugate() |> ComplexVector

        member this.re =
            let (ComplexVector v) = this
            v.Real() |> RealVector

        member this.im =
            let (ComplexVector v) = this
            v.Imaginary() |> RealVector

        member this.norm = (this * this.conjugate).Real |> sqrt


    [<Softellect.Sys.NumericCarrier("a MathNet matrix: its elements are the solver's numbers (R3)")>]
    type RealMatrix =
        | RealMatrix  of Matrix<double>

        member this.Item
            with get((i : int), (j : int)) =
                let (RealMatrix v) = this
                v.[i, j]

        static member create (a : #seq<#seq<double>>) = matrix a |> RealMatrix

        static member (*) (RealMatrix a, RealMatrix b) =
            (a * b) |> RealMatrix

        static member (+) (RealMatrix a, RealMatrix b) =
            (a + b) |> RealMatrix

        static member (-) (RealMatrix a, RealMatrix b) =
            (a - b) |> RealMatrix

        static member (*) (RealVector a, RealMatrix b) : RealVector =
            a * b |> RealVector

        static member (*) (RealMatrix a, RealVector b) : RealVector =
            a * b |> RealVector

        member this.inverse =
            let (RealMatrix m) = this
            m.Inverse() |> RealMatrix


    [<Softellect.Sys.NumericCarrier("a MathNet matrix: its elements are the solver's numbers (R3)")>]
    type ComplexMatrix =
        | ComplexMatrix of Matrix<Complex>

        static member (*) (ComplexMatrix a, ComplexMatrix b) =
            (a * b) |> ComplexMatrix

        static member (*) ((a : Complex), ComplexMatrix b) =
            (a * b) |> ComplexMatrix

        static member (*) (a : ComplexMatrix, b : Complex) = b * a

        static member (+) (ComplexMatrix a, ComplexMatrix b) =
            (a + b) |> ComplexMatrix

        static member (-) (ComplexMatrix a, ComplexMatrix b) =
            (a - b) |> ComplexMatrix

        static member (*) (ComplexVector a, ComplexMatrix b) : ComplexVector =
            a * b |> ComplexVector

        static member (*) (ComplexMatrix a, ComplexVector b) : ComplexVector =
            a * b |> ComplexVector

        static member create (a : #seq<#seq<Complex>>) = matrix a |> ComplexMatrix

        static member fromRe (a : #seq<#seq<double>>) =
            a
            |> Seq.map (fun e -> e |> Seq.map Complex.ofReal)
            |> matrix
            |> ComplexMatrix

        static member fromIm (a : #seq<#seq<double>>) =
            (Complex.create 0. 1.) * (ComplexMatrix.fromRe a)

        member this.inverse =
            let (ComplexMatrix m) = this
            m.Inverse() |> ComplexMatrix

        member this.Item
            with get((i : int), (j : int)) =
                let (ComplexMatrix v) = this
                v.[i, j]

        member this.conjugateTranspose =
            let (ComplexMatrix m) = this
            m.ConjugateTranspose() |> ComplexMatrix

        member this.determinant =
            let (ComplexMatrix m) = this
            m.Determinant()

        member this.re =
            let (ComplexMatrix m) = this
            let len = m.RowCount
            [| for i in 0..(len-1) -> [| for j in 0..(len-1) -> m.[i, j].Real |] |] |> RealMatrix.create

        member this.im =
            let (ComplexMatrix m) = this
            let len = m.RowCount
            [| for i in 0..(len-1) -> [| for j in 0..(len-1) -> m.[i, j].Imaginary |] |] |> RealMatrix.create


    let realFromDiagonal (d : #seq<double>) =
        d
        |> Array.ofSeq
        |> DiagonalMatrix.ofDiagArray


    let complexFromDiagonal (d : #seq<Complex>) =
        d
        |> Array.ofSeq
        |> DiagonalMatrix.ofDiagArray
