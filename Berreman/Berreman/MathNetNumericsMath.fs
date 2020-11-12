namespace Berreman

open System.Numerics
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra


/// kkkmail:20201111 - The main purpose of this file is to deal with the bug in MathNet.Numerics.
/// Because of that bug we have to use a custom fix for complex matrix eigenvalue decomposition (Evd) method.

/// The original purpose of this module was to abstract away differences in vector / matrix libraries.
/// Switching between them is VERY painful.
module MathNetNumericsMath =

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


    /// L2 norm of complex vector.
    let l2Norm (v : #seq<Complex>) =
        let norm = v |> Seq.fold (fun acc r -> acc + r.Real * r.Real + r.Imaginary * r.Imaginary) 0.0 |> sqrt
        norm


    /// Normalizes complex vector using L2 norm.
    let normalize (v : #seq<Complex>) =
        let norm = v |> l2Norm |> cplx
        let retVal = v |> Seq.map (fun e -> e / norm)
        retVal


    let normalizeMatrix (m : Matrix<Complex>) =
        let len = m.RowCount

        [| for i in 0..(len-1) ->
            [| for j in 0..(len-1) -> m.[j, i] |]
            |> normalize
            |> Array.ofSeq
        |]
        |> matrix


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

        member this.Item
            with get (i: int) =
                let (ComplexVector v) = this
                v.[i]

        static member fromIm (a : #seq<double>) = complexI * (ComplexVector.fromRe a)
        member v.conjugate = let (ComplexVector a) = v in a.Conjugate() |> ComplexVector
        member v.re = let (ComplexVector a) = v in a.Real() |> RealVector
        member v.im = let (ComplexVector a) = v in a.Imaginary() |> RealVector
        member v.norm = (v * v.conjugate).Real |> sqrt
        member v.toArray() = let (ComplexVector a) = v in a.ToArray()


    type RealMatrix =
        | RealMatrix  of Matrix<double>

        member this.Item
            with get((i : int), (j : int)) =
                let (RealMatrix v) = this
                v.[i, j]

        static member create (a : #seq<#seq<double>>) = matrix a |> RealMatrix
        static member (*) (RealMatrix a, RealMatrix b) = (a * b) |> RealMatrix
        static member (*) ((a : double), RealMatrix b) = (a * b) |> RealMatrix
        static member (*) (a : RealMatrix, b : double) = b * a
        static member (+) (RealMatrix a, RealMatrix b) = (a + b) |> RealMatrix
        static member (-) (RealMatrix a, RealMatrix b) = (a - b) |> RealMatrix
        static member (*) (RealVector a, RealMatrix b) : RealVector = a * b |> RealVector
        static member (*) (RealMatrix a, RealVector b) : RealVector = a * b |> RealVector
        member m.inverse = let (RealMatrix v) = m in v.Inverse() |> RealMatrix


    type ComplexMatrix =
        | ComplexMatrix of Matrix<Complex>

        static member (*) (ComplexMatrix a, ComplexMatrix b) = (a * b) |> ComplexMatrix
        static member (*) ((a : Complex), ComplexMatrix b) = (a * b) |> ComplexMatrix
        static member (*) (a : ComplexMatrix, b : Complex) = b * a
        static member (+) (ComplexMatrix a, ComplexMatrix b) = (a + b) |> ComplexMatrix
        static member (-) (ComplexMatrix a, ComplexMatrix b) = (a - b) |> ComplexMatrix
        static member (*) (ComplexVector a, ComplexMatrix b) = a * b |> ComplexVector
        static member (*) (ComplexMatrix a, ComplexVector b) = a * b |> ComplexVector
        static member create (a : #seq<#seq<Complex>>) = matrix a |> ComplexMatrix

        static member fromRe (a : #seq<#seq<double>>) =
            a
            |> Seq.map (fun e -> e |> Seq.map (fun x -> cplx x))
            |> matrix
            |> ComplexMatrix

        static member fromIm (a : #seq<#seq<double>>) = complexI * (ComplexMatrix.fromRe a)

        member this.Item
            with get((i : int), (j : int)) =
                let (ComplexMatrix v) = this
                v.[i, j]

        member m.inverse = let (ComplexMatrix v) = m in v.Inverse() |> ComplexMatrix
        member m.conjugateTranspose = let (ComplexMatrix v) = m in v.ConjugateTranspose() |> ComplexMatrix
        member m.determinant = let (ComplexMatrix v) = m in v.Determinant()
        member m.rowCount = let (ComplexMatrix v) = m in v.RowCount

        member this.re =
            let (ComplexMatrix m) = this
            let len = m.RowCount
            [| for i in 0..(len-1) -> [| for j in 0..(len-1) -> m.[i, j].Real |] |] |> RealMatrix.create

        member this.im =
            let (ComplexMatrix m) = this
            let len = m.RowCount
            [| for i in 0..(len-1) -> [| for j in 0..(len-1) -> m.[i, j].Imaginary |] |] |> RealMatrix.create


    type Evd =
        {
            eigenValues : ComplexVector
            eigenVectors : ComplexMatrix
        }


    type ComplexMatrix
        with

        member m.evd() : Evd =
            let (ComplexMatrix v) = m
            let evd = v.Evd()

            {
                eigenValues = evd.EigenValues |> ComplexVector
                eigenVectors = evd.EigenVectors |> ComplexMatrix
            }


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
