namespace Berreman

// The purpose of this module is to abstract away differences in vector / matrix libraries.
// Switching between them is VERY painful.
module ExtremeNumericsMath =
    open Extreme.Mathematics
    open Extreme.Mathematics.LinearAlgebra

    let pi = Constants.Pi
    let degree = Constants.Pi / 180.0

    let cplx r = Complex<double>(r, 0.0)
    let createComplex r i = Complex<double>(r, i)

    type Complex = Complex<double>

    type RealVector =
        RealVector of DenseVector<double>


    type RealMatrix = DenseMatrix<double>


    type ComplexVector = 
        | ComplexVector of DenseVector<Complex>
        static member (*) (ComplexVector a, ComplexVector b) = 
            a.DotProduct(b)

        static member (*) ((a : Complex), ComplexVector b) = 
            (a * b).ToDenseVector() |> ComplexVector

        static member (*) (a : ComplexVector, (b : Complex)) = b * a

        static member (+) (ComplexVector a, ComplexVector b) = 
            (a + b).ToDenseVector() |> ComplexVector

        static member create (a : #seq<Complex>) = 
            let e = a |> Array.ofSeq
            Vector.Create(e) |> ComplexVector

        member this.Item 
            with get (i: int) = 
                let (ComplexVector v) = this
                v.[i]

        member this.conjugate = 
            let (ComplexVector v) = this
            v.Conjugate().ToDenseVector() |> ComplexVector

        member this.re = 
            let (ComplexVector v) = this
            v.ToArray()
            |> Array.map (fun e -> e.Re)
            |> Vector.Create
            |> RealVector

        member this.im = 
            let (ComplexVector v) = this
            v.ToArray()
            |> Array.map (fun e -> e.Im)
            |> Vector.Create
            |> RealVector


    type ComplexMatrix = 
        | ComplexMatrix of DenseMatrix<Complex>
        static member (*) (ComplexMatrix a, ComplexMatrix b) = 
            (a * b).ToDenseMatrix() |> ComplexMatrix

        static member (*) ((a : Complex), ComplexMatrix b) = 
            //let c = 
            //    b.ToArray()
            //    |> Array.map (fun e -> a * e)
            //let d = Matrix.Create(b.RowCount, b.ColumnCount, c, MatrixElementOrder.ColumnMajor)
            //d |> ComplexMatrix
            (a * b).ToDenseMatrix() |> ComplexMatrix

        static member (*) (a : ComplexMatrix, b : Complex) = b * a

        static member (+) (ComplexMatrix a, ComplexMatrix b) = 
            (a + b).ToDenseMatrix() |> ComplexMatrix

        static member (*) (ComplexVector a, ComplexMatrix b) : ComplexVector = 
            a * b |> ComplexVector

        static member (*) (ComplexMatrix a, ComplexVector b) : ComplexVector = 
            a * b |> ComplexVector

        static member create (a : #seq<#seq<Complex>>) = 
            Matrix.Create(array2D a) |> ComplexMatrix

        member this.inverse = 
            let (ComplexMatrix m) = this
            m.GetInverse().ToDenseMatrix() |> ComplexMatrix

        member this.matrixExp = 
            let (ComplexMatrix m) = this
            m.GetExponential().ToDenseMatrix() |> ComplexMatrix

        member this.Item
            with get((i : int), (j : int)) =
                let (ComplexMatrix v) = this
                v.[i, j]

        member this.conjugateTranspose = 
            let (ComplexMatrix m) = this
            //let c = 
            //    m.ToArray()
            //    |> Array.map (fun e -> Complex(e.Real, -e.Imaginary))
            //let d = Matrix.Create(m.ColumnCount, m.RowCount, c, MatrixElementOrder.RowMajor)
            //d |> ComplexMatrix
            m.ConjugateTranspose().ToDenseMatrix() |> ComplexMatrix

        member this.evd = 
            let (ComplexMatrix m) = this
            let evd = m.GetEigenvalueDecomposition()

            {
                eigenValues = evd.Eigenvalues.ToDenseVector() |> ComplexVector
                eigenVectors = evd.Eigenvectors.ToDenseMatrix() |> ComplexMatrix
            }

        member this.determinant = 
            let (ComplexMatrix m) = this
            m.GetDeterminant()

    and Evd = 
        {
            eigenValues : ComplexVector
            eigenVectors : ComplexMatrix
        }


    let diagonalMatrix (n : int) (e : Complex) = 
        Matrix.Create(n, n, fun _ _ -> e) |> ComplexMatrix
