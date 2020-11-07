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
        //printfn "v = %A" (v |> List.ofSeq)
        let norm = v |> Seq.fold (fun acc r -> acc + r.Real * r.Real + r.Imaginary * r.Imaginary) 0.0 |> sqrt |> cplx
        let retVal = v |> Seq.map (fun e -> e / norm)
        //printfn "retVal = %A" (retVal |> List.ofSeq)
        retVal


    let normalizeMatrix (m : Matrix<Complex>) =
        let len = m.RowCount

        [| for i in 0..(len-1) ->
            [| for j in 0..(len-1) -> m.[j, i] |]
            |> normalize
            |> Array.ofSeq
        |]
        |> matrix


    // [ Ex, Hy, Ey, -Hx ]
    type BerremanFieldEH =
        | BerremanFieldEH of ComplexVector4

        member private this.eh =
            let (BerremanFieldEH v) = this
            v

        member b.eX = b.eh.[0]
        member b.hY = b.eh.[1]
        member b.eY = b.eh.[2]
        member b.hX = - b.eh.[3]

        /// z component of Poynting vector
        member b.sZ = ((b.eX) * (b.hY.conjugate) - (b.eY) * (b.hX.conjugate)).Real

        /// Relative x or y "polarization" of the field.
        member b.xy = (b.eX.abs * b.eX.abs - b.eY.abs * b.eY.abs) / (b.eX.abs * b.eX.abs + b.eY.abs * b.eY.abs)


    type BerremanField =
        {
            waveLength : WaveLength
//            n1SinFita : N1SinFita
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
//                n1SinFita = info.n1SinFita
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
        static member create (nsf : N1SinFita) (o : OpticalProperties) =
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

        static member identity = BerremanMatrix.create N1SinFita.normal OpticalProperties.vacuum

        /// Generated, do not modify.
        static member createEmField (nsf : N1SinFita) (o : OpticalProperties) (emXY : EmFieldXY) : EmField =
            let n1SinFita = nsf.complex

            let eX = emXY.e2.x
            let eY = emXY.e2.y
            let hX = emXY.h2.x
            let hY = emXY.h2.y
            let eZ = ((-(o.eps.[2, 0] * o.mu.[2, 2]) + o.rho.[2, 2] * o.rhoT.[2, 0]) * eX - o.eps.[2, 1] * o.mu.[2, 2] * eY + o.rho.[2, 2] * o.rhoT.[2, 1] * eY - o.rho.[2, 2] * n1SinFita * eY - o.mu.[2, 2] * o.rho.[2, 0] * hX + o.mu.[2, 0] * o.rho.[2, 2] * hX + (o.mu.[2, 1] * o.rho.[2, 2] - o.mu.[2, 2] * (o.rho.[2, 1] + n1SinFita)) * hY)/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
            let hZ = ((-(o.eps.[2, 2] * o.rhoT.[2, 0]) + o.eps.[2, 0] * o.rhoT.[2, 2]) * eX - o.eps.[2, 2] * o.rhoT.[2, 1] * eY + o.eps.[2, 1] * o.rhoT.[2, 2] * eY + o.eps.[2, 2] * n1SinFita * eY - o.eps.[2, 2] * o.mu.[2, 0] * hX + o.rho.[2, 0] * o.rhoT.[2, 2] * hX + (-(o.eps.[2, 2] * o.mu.[2, 1]) + o.rhoT.[2, 2] * (o.rho.[2, 1] + n1SinFita)) * hY)/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
            //EmField.create (emXY, eZ, hZ)
            0


    type BerremanField
        with
        member this.toEmField() : EmField =
            let emXY =
                {
                    waveLength = this.waveLength
//                    n1SinFita = this.n1SinFita
                    opticalProperties = this.opticalProperties
                    e2 = [ this.eX; this.eY ] |> E2.create
                    h2 = [ this.hX; this.hY ] |> H2.create
                } : EmFieldXY

            BerremanMatrix.createEmField this.opticalProperties emXY


    type EmField
        with
        member this.toBerremanField() : BerremanField =
            {
                waveLength = this.waveLength
                n1SinFita = this.n1SinFita
                opticalProperties = this.opticalProperties
                eh = [ this.e.x; this.h.y; this.e.y; -this.h.x ] |> ComplexVector.create |> ComplexVector4 |> BerremanFieldEH
            }


    type BerremanMatrixPropagated =
        | BerremanMatrixPropagated of ComplexMatrix4x4

        static member propagateLayer (l : Layer) (em : EmField) : BerremanMatrixPropagated =
            let m = BerremanMatrix.create em.n1SinFita l.properties
            let (WaveLength w) = em.waveLength

            match l.thickness with
            | Thickness t -> m.berremanMatrix.matrixExp (Complex(0.0, (2.0 * pi * t / w))) |> BerremanMatrixPropagated
            | Infinity -> failwith "TODO: Implement infinite thickness by making that layer the output media."

        static member propagateInclinedLayer (l : WedgeLayer) (em : EmField) : BerremanMatrixPropagated =
            if abs em.n1SinFita.fita.value < almostZero
            then BerremanMatrixPropagated.propagateLayer l.layer em
            else failwith "propagateInclinedLayer for not normal incidence is not yet implemented."

        static member propagate (ls : List<Layer>, em : EmField) : BerremanMatrixPropagated =
            ls |> List.fold (fun acc r -> (BerremanMatrixPropagated.propagateLayer r em) * acc) BerremanMatrixPropagated.identity

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


    let private propagate (emf : EmField) (BerremanMatrixPropagated bmp) =
        let b = emf.toBerremanField()
        let (BerremanFieldEH beh) = b.eh
        let bp = { b with eh = bmp * beh |> BerremanFieldEH }
        bp.toEmField()


    type EmField
        with
        member this.propagate (s : Layer) : EmField =
            BerremanMatrixPropagated.propagateLayer s this |> propagate this

        member this.propagate (s : WedgeLayer) : EmField =
            BerremanMatrixPropagated.propagateInclinedLayer s this |> propagate this
