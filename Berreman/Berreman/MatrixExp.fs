namespace Berreman

open System.Numerics
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNetNumericsMath

module MatrixExp =

    /// Simple port of https://people.sc.fsu.edu/~jburkardt/c_src/matrix_exponential/matrix_exponential.c & related.
    /// No optimization was performed.
    /// This code is distributed under the GNU LGPL license, which is identical to the source license.
    type ComplexMatrix
    with
        static member identity (n : int) : ComplexMatrix =
            complexDiagonalMatrix n (cplx 1.0)

        member this.lInfinityNorm() : double =
            let (ComplexMatrix m) = this

            let rowSum (v : Vector<Complex>) =
                v.ToArray()
                |> Array.fold (fun acc e -> acc + (sqrt (e.Norm()))) 0.0

            let rows = [ for i in 0..m.RowCount - 1 -> m.Row(i) ]
            rows |> List.fold (fun acc v -> max acc (rowSum v)) 0.0

        static member addScaled (a : Complex) (ma : ComplexMatrix) (b : Complex) (mb : ComplexMatrix) =
            a * ma + b * mb

        member this.matrixExp() : ComplexMatrix =
            let (ComplexMatrix m) = this
            let q = 6
            let one = cplx 1.0

            let aNorm = this.lInfinityNorm ()
            //printfn "aNorm = %A" aNorm

            let ee = int ((log aNorm) / (log 2.0)) + 1
            let s = max 0 (ee + 1)
            //printfn "s = %A" s

            let t = 1.0 / (pown 2.0 s)
            //printfn "t = %A" t

            let a2 = (cplx t) * this
            //printfn "a2 = %A" a2

            let x = a2
            //printfn "x = %A" x

            let c = cplx 0.5

            let e = ComplexMatrix.identity m.RowCount
            //printfn "e = %A" e

            let e1 = ComplexMatrix.addScaled one e c a2
            //printfn "e1 = %A" e1

            let d1 = ComplexMatrix.addScaled one e (-c) a2
            //printfn "d1 = %A" d1

            let p = true

            let rec update
                (pp : bool)
                (kk : int)
                (cc : Complex)
                (dd : ComplexMatrix)
                (ee : ComplexMatrix)
                (xx : ComplexMatrix) =
                if kk <= q
                then
                    let cn = cc * (cplx ((double (q - kk + 1)) / (double (kk * (2 * q - kk + 1)))))
                    //printfn "cn = %A" cn

                    let xn = a2 * xx
                    let en = ComplexMatrix.addScaled cn xn one ee

                    let dn =
                        match pp with
                        | true -> ComplexMatrix.addScaled cn xn one dd
                        | false -> ComplexMatrix.addScaled (-cn) xn one dd

                    update (not pp) (kk + 1) cn dn en xn
                else
                    (dd, ee)

            let (dn, en) = update p 2 c d1 e1 x
            let e1 = dn.inverse * en
            //printfn "e1 = %A" e1

            let rec mult (k : int) (res : ComplexMatrix) =
                if k <= s
                then mult (k + 1) (res * res)
                else res

            let retVal = mult 1 e1

            //printfn "retVal = %A" retVal
            //printfn "retVal * e1 = %A" (retVal * e1)
            retVal

