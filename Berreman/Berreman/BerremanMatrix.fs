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

            let a =
                {
                    berremanMatrix = berremanMatrix
                    n1SinFita = nsf
                    opticalProperties = o
                }

            a

        static member identity = BerremanMatrix.create OpticalProperties.vacuum N1SinFita.normal


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

        static member propagateLayer (l : Layer) (emc : EmComponent) (WaveLength w) : BerremanMatrixPropagated =
            let m = BerremanMatrix.create l.properties emc.n1SinFita

            match l.thickness with
            | Thickness x ->
                let a = m.berremanMatrix.matrixExp (Complex(0.0, (2.0 * pi * x / w))) |> BerremanMatrixPropagated
                a
            | Infinity -> failwith "TODO: Implement infinite thickness by making this layer the output media."

        static member propagateInclinedLayer (l : WedgeLayer) (emc : EmComponent) (w : WaveLength) : BerremanMatrixPropagated =
            if abs emc.n1SinFita.value < almostZero
            then BerremanMatrixPropagated.propagateLayer l.layer emc w
            else failwith "propagateInclinedLayer for not normal incidence is not yet implemented."

        static member propagate (ls : List<Layer>, emc : EmComponent, w : WaveLength) : BerremanMatrixPropagated =
            ls |> List.fold (fun acc r -> (BerremanMatrixPropagated.propagateLayer r emc w) * acc) BerremanMatrixPropagated.identity

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
//                |> List.map (fun (v, e) -> v, e, ((BerremanFieldEH e).sZ, (BerremanFieldEH e).xy))
                |> List.map (fun (v, e) -> v, e, (sign (BerremanFieldEH e).sZ, (BerremanFieldEH e).xy))
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

//        static member create (info : IncidentLightInfo, o : OpticalProperties) : EmField =
//            let n1SinFita = info.refractionIndex.value * (sin info.incidenceAngle.value) |> N1SinFita
//            let bm = BerremanMatrix.create o n1SinFita
//            let ev = bm.eigenBasis()
//            let (Ellipticity e) = info.ellipticity
//            let a90 = e / sqrt(1.0 + e * e)
//            let a0 = 1.0 / sqrt(1.0 + e * e)
//
//            let (e0, h0, n0) = normalizeEH info.eh0
//            let (e90, h90, n90) = normalizeEH info.eh90
//
//            let emc0 = EmComponent.create ev.down.evv0 (cplx 1.0) n1SinFita o
//            let emc1 = EmComponent.create ev.down.evv1 (cplx 1.0) n1SinFita o
//            let norm0 = emc0.e.value.norm
//            let norm1 = emc1.e.value.norm
//
//            let em =
//                {
//                    waveLength = info.waveLength
//                    opticalProperties = o
//                    emComponents =
//                        [
//                            { emc0 with amplitude = (a90 / norm0 |> cplx) * cplxI }
//                            { emc1 with amplitude = a0 / norm1 |> cplx }
//
//
////                            {
////                                amplitude = a0 * n0
////                                emEigenVector =
////                                    {
////                                        eigenValue = cplx info.refractionIndex.value
////                                        e = e0
////                                        h = h0
////                                    }
////                            }
////
////                            {
////                                amplitude = cplxI * a90 * n90
////                                emEigenVector =
////                                    {
////                                        eigenValue = cplx info.refractionIndex.value
////                                        e = e90
////                                        h = h90
////                                    }
////                            }
//                        ]
//                }
//
//            em


        static member create (info : IncidentLightInfo, o : OpticalProperties) : EmField =
//            let n1SinFita = info.refractionIndex.value * (sin info.incidenceAngle.value) |> N1SinFita
//            let bm = BerremanMatrix.create o n1SinFita
//            let ev = bm.eigenBasis()
//            let (Ellipticity e) = info.ellipticity
//            let a90 = e / sqrt(1.0 + e * e)
//            let a0 = 1.0 / sqrt(1.0 + e * e)
//
//            let emc0 = EmComponent.create ev.down.evv0 (cplx 1.0) n1SinFita o
//            let emc1 = EmComponent.create ev.down.evv1 (cplx 1.0) n1SinFita o
//            let norm0 = emc0.e.value.norm
//            let norm1 = emc1.e.value.norm
//
//            let em =
//                {
//                    waveLength = info.waveLength
//                    opticalProperties = o
//                    emComponents =
//                        [
//                            { emc0 with amplitude = (a90 / norm0 |> cplx) * cplxI }
//                            { emc1 with amplitude = a0 / norm1 |> cplx }
//                        ]
//                }
//
//            em

            let nsf = N1SinFita.normal
            let bm = BerremanMatrix.create o nsf
            let ev = bm.eigenBasis()
            let (Ellipticity e) = info.ellipticity
            let a90 = (e / sqrt(1.0 + e * e) |> cplx) * cplxI
            let a0 = 1.0 / sqrt(1.0 + e * e) |> cplx

            let emc0 = EmComponent.create ev.down.evv0 (cplx 1.0) nsf o
            let emc1 = EmComponent.create ev.down.evv1 (cplx 1.0) nsf o
            let norm0 = emc0.e.value.norm |> cplx
            let norm1 = emc1.e.value.norm |> cplx

            let em =
                {
                    waveLength = info.waveLength
                    opticalProperties = o
                    emComponents =
                        [
                            { emc0 with amplitude = a90 / norm0 }
                            { emc1 with amplitude = a0 / norm1 }
                        ]
                }

//            let emr = em.rotateZY info.polarization.angle (-info.incidenceAngle.angle)
            let emr = em.rotateYZ (info.incidenceAngle.angle) info.polarization.angle
            emr

        /// TODO kk:20201108 - Does not work properly yet.
        /// Both propagate and propagate1 seems to produce the same result except that the identity tests,
        /// e.g. muellerMatrixR_BiaxialCrystalSubstrateSystem_Polarized_WithEllipticity then fail.
        member emf.propagate (s : Layer) : EmField =
            let propagate (emc : EmComponent) =
                match s.thickness with
                | Thickness x ->
                    let multiplier = exp (emc.emEigenVector.eigenValue * Complex(0.0, (2.0 * pi * x / emf.waveLength.value)))
                    emc * multiplier
                | Infinity -> failwith "TODO: Implement infinite thickness by making this layer the output media."

            let propagate1 (emc : EmComponent) : EmComponent =
                let (BerremanMatrixPropagated bmp) = BerremanMatrixPropagated.propagateLayer s emc emf.waveLength
                let beh = [ emc.emEigenVector.e.x; emc.emEigenVector.h.y; emc.emEigenVector.e.y; -emc.emEigenVector.h.x ] |> ComplexVector.create |> ComplexVector4
                let bp = bmp * beh
                let norm = beh.norm
                let multiplier = bp * (beh.conjugate) / ((norm * norm) |> cplx)
                emc * multiplier

            let a = emf.emComponents |> List.map propagate
            let a1 = emf.emComponents |> List.map propagate1

            let retVal = { emf with emComponents = a1 }
            retVal

        member emf.propagate (s : WedgeLayer) : EmField =
            let m = emf.emComponents |> List.map (fun e -> abs e.n1SinFita.value)|> List.max

            if m < almostZero
            then emf.propagate s.layer
            else failwith "propagateInclinedLayer for not normal incidence is not yet implemented."


