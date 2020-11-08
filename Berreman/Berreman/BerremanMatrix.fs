namespace Berreman

open System.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNetNumericsMath

open Geometry
open Fields
open MaterialProperties
open Media
open Constants

module BerremanMatrix =

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


    type BerremanField =
        {
            waveLength : WaveLength
            opticalProperties : OpticalProperties
            eh : BerremanFieldEH
        }
        member b.eX = b.eh.eX
        member b.hY = b.eh.hY
        member b.eY = b.eh.eY
        member b.hX = b.eh.hX
        member b.sZ = b.eh.sZ

        static member create (info : IncidentLightInfo) (o : OpticalProperties) (eh : ComplexVector4) =
            {
                waveLength = info.waveLength
                opticalProperties = o
                eh = eh |> BerremanFieldEH
            }


    type BerremanMatrix =
        {
            berremanMatrix : ComplexMatrix4x4
            n1SinFita : N1SinFita
            opticalProperties : OpticalProperties
        }

        /// Generated, do not modify.
        static member create (o : OpticalProperties) (nsf : N1SinFita) =
            let n1SinFita = nsf.complex

            let berremanMatrix =
                [
                    [
                        (o.eps.[2, 2] * (o.mu.[2, 2] * o.rhoT.[1, 0] - o.mu.[1, 2] * o.rhoT.[2, 0]) + o.eps.[2, 0] * o.mu.[1, 2] * o.rhoT.[2, 2] - o.rho.[2, 2] * o.rhoT.[1, 0] * o.rhoT.[2, 2] - o.eps.[2, 0] * o.mu.[2, 2] * (o.rhoT.[1, 2] + n1SinFita) + o.rho.[2, 2] * o.rhoT.[2, 0] * (o.rhoT.[1, 2] + n1SinFita))/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                        (o.eps.[2, 2] * (-(o.mu.[1, 2] * o.mu.[2, 1]) + o.mu.[1, 1] * o.mu.[2, 2]) + o.mu.[2, 1] * o.rho.[2, 2] * o.rhoT.[1, 2] + o.mu.[1, 2] * o.rho.[2, 1] * o.rhoT.[2, 2] - o.mu.[1, 1] * o.rho.[2, 2] * o.rhoT.[2, 2] + o.mu.[2, 1] * o.rho.[2, 2] * n1SinFita + o.mu.[1, 2] * o.rhoT.[2, 2] * n1SinFita - o.mu.[2, 2] * (o.rho.[2, 1] + n1SinFita) * (o.rhoT.[1, 2] + n1SinFita))/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                        (o.eps.[2, 2] * o.mu.[2, 2] * o.rhoT.[1, 1] + o.eps.[2, 1] * o.mu.[1, 2] * o.rhoT.[2, 2] - o.rho.[2, 2] * o.rhoT.[1, 1] * o.rhoT.[2, 2] - o.eps.[2, 1] * o.mu.[2, 2] * (o.rhoT.[1, 2] + n1SinFita) + o.rho.[2, 2] * (o.rhoT.[2, 1] - n1SinFita) * (o.rhoT.[1, 2] + n1SinFita) + o.eps.[2, 2] * o.mu.[1, 2] * (-o.rhoT.[2, 1] + n1SinFita))/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                        (o.eps.[2, 2] * (o.mu.[1, 2] * o.mu.[2, 0] - o.mu.[1, 0] * o.mu.[2, 2]) - o.mu.[1, 2] * o.rho.[2, 0] * o.rhoT.[2, 2] + o.mu.[1, 0] * o.rho.[2, 2] * o.rhoT.[2, 2] + o.mu.[2, 2] * o.rho.[2, 0] * (o.rhoT.[1, 2] + n1SinFita) - o.mu.[2, 0] * o.rho.[2, 2] * (o.rhoT.[1, 2] + n1SinFita))/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                    ]
                    [
                        (-(o.eps.[0, 2] * o.eps.[2, 0] * o.mu.[2, 2]) + o.eps.[0, 0] * o.eps.[2, 2] * o.mu.[2, 2] - o.eps.[2, 2] * o.rho.[0, 2] * o.rhoT.[2, 0] + o.eps.[0, 2] * o.rho.[2, 2] * o.rhoT.[2, 0] + o.eps.[2, 0] * o.rho.[0, 2] * o.rhoT.[2, 2] - o.eps.[0, 0] * o.rho.[2, 2] * o.rhoT.[2, 2])/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                        (o.eps.[2, 2] * (o.mu.[2, 2] * o.rho.[0, 1] - o.mu.[2, 1] * o.rho.[0, 2]) + o.eps.[0, 2] * o.mu.[2, 1] * o.rho.[2, 2] - o.rho.[0, 1] * o.rho.[2, 2] * o.rhoT.[2, 2] - o.eps.[0, 2] * o.mu.[2, 2] * (o.rho.[2, 1] + n1SinFita) + o.rho.[0, 2] * o.rhoT.[2, 2] * (o.rho.[2, 1] + n1SinFita))/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                        (o.eps.[0, 1] * o.eps.[2, 2] * o.mu.[2, 2] - o.eps.[2, 2] * o.rho.[0, 2] * o.rhoT.[2, 1] + o.eps.[2, 1] * o.rho.[0, 2] * o.rhoT.[2, 2] - o.eps.[0, 1] * o.rho.[2, 2] * o.rhoT.[2, 2] + o.eps.[2, 2] * o.rho.[0, 2] * n1SinFita - o.eps.[0, 2] * (o.eps.[2, 1] * o.mu.[2, 2] + o.rho.[2, 2] * (-o.rhoT.[2, 1] + n1SinFita)))/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                        (-(o.eps.[2, 2] * o.mu.[2, 2] * o.rho.[0, 0]) + o.eps.[2, 2] * o.mu.[2, 0] * o.rho.[0, 2] + o.eps.[0, 2] * o.mu.[2, 2] * o.rho.[2, 0] - o.eps.[0, 2] * o.mu.[2, 0] * o.rho.[2, 2] - o.rho.[0, 2] * o.rho.[2, 0] * o.rhoT.[2, 2] + o.rho.[0, 0] * o.rho.[2, 2] * o.rhoT.[2, 2])/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                    ]
                    [
                        (-(o.eps.[2, 2] * o.mu.[2, 2] * o.rhoT.[0, 0]) + o.eps.[2, 0] * o.mu.[2, 2] * o.rhoT.[0, 2] + o.eps.[2, 2] * o.mu.[0, 2] * o.rhoT.[2, 0] - o.rho.[2, 2] * o.rhoT.[0, 2] * o.rhoT.[2, 0] - o.eps.[2, 0] * o.mu.[0, 2] * o.rhoT.[2, 2] + o.rho.[2, 2] * o.rhoT.[0, 0] * o.rhoT.[2, 2])/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                        -((o.eps.[2, 2] * (-(o.mu.[0, 2] * o.mu.[2, 1]) + o.mu.[0, 1] * o.mu.[2, 2]) + o.mu.[2, 1] * o.rho.[2, 2] * o.rhoT.[0, 2] + o.mu.[0, 2] * o.rho.[2, 1] * o.rhoT.[2, 2] - o.mu.[0, 1] * o.rho.[2, 2] * o.rhoT.[2, 2] + o.mu.[0, 2] * o.rhoT.[2, 2] * n1SinFita - o.mu.[2, 2] * o.rhoT.[0, 2] * (o.rho.[2, 1] + n1SinFita))/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2]))
                        (o.eps.[2, 1] * o.mu.[2, 2] * o.rhoT.[0, 2] - o.rho.[2, 2] * o.rhoT.[0, 2] * o.rhoT.[2, 1] - o.eps.[2, 1] * o.mu.[0, 2] * o.rhoT.[2, 2] + o.rho.[2, 2] * o.rhoT.[0, 1] * o.rhoT.[2, 2] + o.rho.[2, 2] * o.rhoT.[0, 2] * n1SinFita - o.eps.[2, 2] * (o.mu.[2, 2] * o.rhoT.[0, 1] + o.mu.[0, 2] * (-o.rhoT.[2, 1] + n1SinFita)))/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                        (-(o.eps.[2, 2] * o.mu.[0, 2] * o.mu.[2, 0]) + o.eps.[2, 2] * o.mu.[0, 0] * o.mu.[2, 2] - o.mu.[2, 2] * o.rho.[2, 0] * o.rhoT.[0, 2] + o.mu.[2, 0] * o.rho.[2, 2] * o.rhoT.[0, 2] + o.mu.[0, 2] * o.rho.[2, 0] * o.rhoT.[2, 2] - o.mu.[0, 0] * o.rho.[2, 2] * o.rhoT.[2, 2])/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                    ]
                    [
                        (o.eps.[1, 2] * (-(o.eps.[2, 0] * o.mu.[2, 2]) + o.rho.[2, 2] * o.rhoT.[2, 0]) + o.eps.[1, 0] * (o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2]) - (o.eps.[2, 2] * o.rhoT.[2, 0] - o.eps.[2, 0] * o.rhoT.[2, 2]) * (o.rho.[1, 2] - n1SinFita))/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                        (o.eps.[2, 2] * o.mu.[2, 2] * o.rho.[1, 1] + o.eps.[1, 2] * o.mu.[2, 1] * o.rho.[2, 2] - o.rho.[1, 1] * o.rho.[2, 2] * o.rhoT.[2, 2] + o.eps.[2, 2] * o.mu.[2, 1] * (-o.rho.[1, 2] + n1SinFita) - o.eps.[1, 2] * o.mu.[2, 2] * (o.rho.[2, 1] + n1SinFita) + o.rhoT.[2, 2] * (o.rho.[1, 2] - n1SinFita) * (o.rho.[2, 1] + n1SinFita))/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                        (o.eps.[1, 1] * (o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2]) + (o.rho.[1, 2] - n1SinFita) * (-(o.eps.[2, 2] * o.rhoT.[2, 1]) + o.eps.[2, 1] * o.rhoT.[2, 2] + o.eps.[2, 2] * n1SinFita) - o.eps.[1, 2] * (o.eps.[2, 1] * o.mu.[2, 2] + o.rho.[2, 2] * (-o.rhoT.[2, 1] + n1SinFita)))/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                        (o.eps.[1, 2] * o.mu.[2, 2] * o.rho.[2, 0] - o.eps.[1, 2] * o.mu.[2, 0] * o.rho.[2, 2] - o.rho.[1, 2] * o.rho.[2, 0] * o.rhoT.[2, 2] + o.rho.[1, 0] * o.rho.[2, 2] * o.rhoT.[2, 2] + o.rho.[2, 0] * o.rhoT.[2, 2] * n1SinFita - o.eps.[2, 2] * (o.mu.[2, 2] * o.rho.[1, 0] + o.mu.[2, 0] * (-o.rho.[1, 2] + n1SinFita)))/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                    ]
                ]
                |> ComplexMatrix4x4.create

            {
                berremanMatrix = berremanMatrix
                n1SinFita = nsf
                opticalProperties = o
            }

        static member identity = BerremanMatrix.create OpticalProperties.vacuum N1SinFita.normal


//    type BerremanField
//        with
//        member this.toEmField() : EmField =
//            let emXY =
//                {
//                    waveLength = this.waveLength
////                    n1SinFita = this.n1SinFita
//                    opticalProperties = this.opticalProperties
//                    e2 = [ this.eX; this.eY ] |> E2.create
//                    h2 = [ this.hX; this.hY ] |> H2.create
//                } : EmFieldXY
//
//            BerremanMatrix.createEmField this.opticalProperties emXY


    type EmField
        with
        member this.toBerremanField() : BerremanField =
            {
                waveLength = this.waveLength
                opticalProperties = this.opticalProperties
                eh = [ this.e.x; this.h.y; this.e.y; -this.h.x ] |> ComplexVector.create |> ComplexVector4 |> BerremanFieldEH
            }


    type BerremanMatrixPropagated =
        | BerremanMatrixPropagated of ComplexMatrix4x4

        static member propagateLayer (l : Layer) (em : EmComponent) (WaveLength w) : BerremanMatrixPropagated =
            let m = BerremanMatrix.create l.properties em.n1SinFita

            match l.thickness with
            | Thickness x -> m.berremanMatrix.matrixExp (Complex(0.0, (2.0 * pi * x / w))) |> BerremanMatrixPropagated
            | Infinity -> failwith "TODO: Implement infinite thickness by making this layer the output media."

        static member propagateInclinedLayer (l : WedgeLayer) (em : EmComponent) (w : WaveLength) : BerremanMatrixPropagated =
            if abs em.n1SinFita.value < almostZero
            then BerremanMatrixPropagated.propagateLayer l.layer em w
            else failwith "propagateInclinedLayer for not normal incidence is not yet implemented."

        static member propagate (ls : List<Layer>, em : EmComponent, w : WaveLength) : BerremanMatrixPropagated =
            ls |> List.fold (fun acc r -> (BerremanMatrixPropagated.propagateLayer r em w) * acc) BerremanMatrixPropagated.identity

        static member identity = ComplexMatrix4x4.identity |> BerremanMatrixPropagated

        static member (*) (BerremanMatrixPropagated a, BerremanMatrixPropagated b) =
            (a * b) |> BerremanMatrixPropagated


    type ComplexMatrix4x4
        with

        member this.eigenBasis() =
            let (ComplexMatrix4x4 (ComplexMatrix m)) = this
            let evd = m.Evd()

            /// We need to normalize and transpose eigenvectors.
            let toArrays (e : Matrix<Complex>) =
                let len = e.RowCount

                let normed =
                    [| for i in 0..(len-1) ->
                        [| for j in 0..(len-1) -> e.[j, i] |]
                        |> normalize
                        |> Array.ofSeq
                    |]

                [| for i in 0..(len-1) -> [| for j in 0..(len-1) -> normed.[i].[j] |] |]

            let ve =
                Array.zip (evd.EigenValues.ToArray()) (evd.EigenVectors |> toArrays)
                |> List.ofArray
                |> List.map (fun (v, e) -> v, e |> normalize |> ComplexVector4.create)
                |> List.map (fun (v, e) -> v, e, ((BerremanFieldEH e).sZ, (BerremanFieldEH e).xy))
                |> List.sortBy (fun (_, _, s) -> s) // Sort by z component of Poynting vector first, then by x <-> y relative polarization.
                |> List.map (fun (v, e, _) -> v, e)

            let up = ve |> List.take 2 |> EigenBasis.create
            let dn = ve |> List.rev |> List.take 2 |> List.rev |> EigenBasis.create

            {
                down = dn
                up = up
            }


    type BerremanMatrix
        with
        member this.eigenBasis() = this.berremanMatrix.eigenBasis()


    type BerremanMatrixPropagated
        with
        member this.eigenBasis() =
            let (BerremanMatrixPropagated p) = this
            p.eigenBasis()


//    let private propagate (emf : EmField) (BerremanMatrixPropagated bmp) =
//        let b = emf.toBerremanField()
//        let (BerremanFieldEH beh) = b.eh
//        let bp = { b with eh = bmp * beh |> BerremanFieldEH }
//        bp.toEmField()


    type EmField
        with
        member emf.propagate (s : Layer) : EmField =
//             failwith "EmField.propagate is not yet implemented"

            let propagate (emc : EmComponent) =
                match s.thickness with
                | Thickness x ->
                    let multiplier = exp (emc.emEigenVector.eigenValue * Complex(0.0, (2.0 * pi * x / emf.waveLength.value)))
                    emc * multiplier
                | Infinity -> failwith "TODO: Implement infinite thickness by making this layer the output media."

            let a =
                emf.emComponents
                |> List.map propagate

            { emf with emComponents = a }

        member emf.propagate (s : WedgeLayer) : EmField =
//             failwith "EmField.propagate is not yet implemented"
//            if abs emf.n1SinFita.value < almostZero
//            then BerremanMatrixPropagated.propagateLayer s.layer em w
//            else failwith "propagateInclinedLayer for not normal incidence is not yet implemented."
//
//            BerremanMatrixPropagated.propagateInclinedLayer s emf |> propagate emf

            emf.propagate s.layer
