namespace Berreman

// The purpose of this module is to abstract away differences in vector / matrix libraries.
// Switching between them is VERY painful.
module MathNetNumericsMath =
    open System.Numerics
    open MathNet.Numerics
    open MathNet.Numerics.LinearAlgebra

    let pi = Constants.Pi
    let degree = Constants.Pi / 180.0
    let toDegree r = r * 180.0 / Constants.Pi

    let cplx r = Complex(r, 0.0)
    let createComplex r i = Complex(r, i)

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


    type RealVector =
        | RealVector of Vector<double>
        member this.Item
            with get (i: int) =
                let (RealVector v) = this
                v.[i]

        static member (*) (RealVector a, RealVector b) = a * b
        static member (*) (a : double, RealVector b) = a * b |> RealVector
        static member (*) (RealVector a, b : double) = b * a |> RealVector
        static member (/) (RealVector a, b : double) = a / b |> RealVector
        static member create (a : #seq<double>) = vector a |> RealVector
        static member (+) (RealVector a, RealVector b) = a + b |> RealVector
        static member (-) (RealVector a, RealVector b) = a - b |> RealVector

        member this.norm =
            let (RealVector v) = this
            v * v |> sqrt


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
            |> Seq.map (fun e -> cplx e)
            |> vector
            |> ComplexVector

        static member fromIm (a : #seq<double>) =
           (createComplex 0. 1.) * (ComplexVector.fromRe a)

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


    type RealMatrix =
        | RealMatrix  of Matrix<double>

        member this.Item
            with get((i : int), (j : int)) =
                let (RealMatrix v) = this
                v.[i, j]

        static member create (a : #seq<#seq<double>>) = matrix a |> RealMatrix

        static member (*) (RealMatrix a, RealMatrix b) =
            (a * b) |> RealMatrix

        static member (*) ((a : double), RealMatrix b) =
            (a * b) |> RealMatrix

        static member (*) (a : RealMatrix, b : double) = b * a

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
            |> Seq.map (fun e -> e |> Seq.map (fun x -> cplx x))
            |> matrix
            |> ComplexMatrix

        static member fromIm (a : #seq<#seq<double>>) =
            (createComplex 0. 1.) * (ComplexMatrix.fromRe a)

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


    let realDiagonalMatrix (n : int) (e : double) =
        DiagonalMatrix.create n e |> RealMatrix


    let realFromDiagonal (d : #seq<double>) =
        d
        |> Array.ofSeq
        |> DiagonalMatrix.ofDiagArray


    let complexDiagonalMatrix (n : int) (e : Complex) =
        DiagonalMatrix.create n e |> ComplexMatrix


    let complexFromDiagonal (d : #seq<Complex>) =
        d
        |> Array.ofSeq
        |> DiagonalMatrix.ofDiagArray
