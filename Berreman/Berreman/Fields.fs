namespace Berreman

open System.Numerics
open MathNetNumericsMath

open Geometry
open MaterialProperties
open Constants

module Fields =

    // CGS units are used.


        /// L2 norm of complex vector.
    let l2Norm (v : #seq<Complex>) =
        let norm = v |> Seq.fold (fun acc r -> acc + r.Real * r.Real + r.Imaginary * r.Imaginary) 0.0 |> sqrt
        norm


    type IncidenceAngle =
        | IncidenceAngle of Angle
        static member create (Angle p) =
            (p % (pi / 2.0) + pi) % (pi / 2.0) |> Angle |> IncidenceAngle

        member angle.value = let (IncidenceAngle a) = angle in a.value
        static member normal = IncidenceAngle.create (Angle.degree 0.0)
        static member maxValue = IncidenceAngle.create (Angle.degree 89.0)
        member this.description = sprintf "incidence angle: %A degree(s)" (this.value / degree)
        static member (+) (IncidenceAngle a, Angle b) = a.value + b |> Angle |> IncidenceAngle
        static member (-) (IncidenceAngle a, Angle b) = a.value - b |> Angle |> IncidenceAngle


    /// n1 * sin(fita), where fita is the incidence angle and n1 is the refraction index of upper media.
    type N1SinFita =
        | N1SinFita of double

        static member defaultValue = 0.0 |> N1SinFita
        member nsf.value = let (N1SinFita v) = nsf in v
        member nsf.complex = nsf.value |> cplx
        static member normal = N1SinFita 0.0
        static member create (RefractionIndex n) (IncidenceAngle (Angle f)) = n * (sin f) |> N1SinFita


    type RT =
        | Reflected
        | Transmitted


    /// Electromagnetic field E.
    type E =
        | E of ComplexVector3

        static member (*) (Eps (ComplexMatrix3x3 a), E (ComplexVector3 b)) = a * b |> ComplexVector3 |> D
        static member (*) (RhoT (ComplexMatrix3x3 a), E (ComplexVector3 b)) = a * b |> ComplexVector3 |> B
        static member (+) (E (ComplexVector3 a), E (ComplexVector3 b)) : E = a + b |> ComplexVector3 |> E
        static member (*) (a : Complex, E (ComplexVector3 b)) = a * b |> ComplexVector3 |> E
        static member (*) (E (ComplexVector3 a), b : Complex) = a * b |> ComplexVector3 |> E
        static member (/) (E (ComplexVector3 a), b : Complex) = a * ((cplx 1.0) / b) |> ComplexVector3 |> E
        static member create a = a |> ComplexVector3.create |> E
        static member fromRe a = a |> ComplexVector3.fromRe |> E
        static member defaultValue = [ 0.0; 0.0; 0.0 ] |> E.fromRe
        member e.value = let (E a) = e in a
        member e.x = let (E a) = e in a.x
        member e.y = let (E a) = e in a.y
        member e.z = let (E a) = e in a.z
        member e.rotate r = let (E a) = e in a.rotate r |> E


    /// Electromagnetic field H.
    and H =
        | H of ComplexVector3

        static member (*) (Rho (ComplexMatrix3x3 a), H (ComplexVector3 b)) = a * b |> ComplexVector3 |> D
        static member (*) (Mu (ComplexMatrix3x3 a), H (ComplexVector3 b)) = a * b |> ComplexVector3 |> B
        static member (+) (H (ComplexVector3 a), H (ComplexVector3 b)) : H = a + b |> ComplexVector3 |> H
        static member (*) (a : Complex, H (ComplexVector3 b)) = a * b |> ComplexVector3 |> H
        static member (*) (H (ComplexVector3 a), b : Complex) = a * b |> ComplexVector3 |> H
        static member (/) (H (ComplexVector3 a), b : Complex) = a * ((cplx 1.0) / b) |> ComplexVector3 |> H
        static member create a = a |> ComplexVector3.create |> H
        static member fromRe a = a |> ComplexVector3.fromRe |> H
        static member defaultValue = [ 0.0; 0.0; 0.0 ] |> H.fromRe
        member h.value = let (H a) = h in a
        member h.x = let (H a) = h in a.x
        member h.y = let (H a) = h in a.y
        member h.z = let (H a) = h in a.z
        member h.rotate r = let (H a) = h in a.rotate r |> H


    /// Electromagnetic field D.
    and D =
        | D of ComplexVector3

        static member (+) (D (ComplexVector3 a), D (ComplexVector3 b)) : D = a + b |> ComplexVector3 |> D
        static member (*) (a : Complex, D (ComplexVector3 b)) = a * b |> ComplexVector3 |> D
        static member (*) (D (ComplexVector3 a), b : Complex) = a * b |> ComplexVector3 |> D
        member d.value = let (D a) = d in a
        member d.x = let (D a) = d in a.x
        member d.y = let (D a) = d in a.y
        member d.z = let (D a) = d in a.z


    /// Electromagnetic field B.
    and B =
        | B of ComplexVector3

        static member (+) (B (ComplexVector3 a), B (ComplexVector3 b)) : B = a + b |> ComplexVector3 |> B
        static member (*) (a : Complex, B (ComplexVector3 b)) = a * b |> ComplexVector3 |> B
        static member (*) (B (ComplexVector3 a), b : Complex) = a * b |> ComplexVector3 |> B
        member b.value = let (B a) = b in a
        member b.x = let (B a) = b in a.x
        member b.y = let (B a) = b in a.y
        member b.z = let (B a) = b in a.z


    /// [ Ex, Hy, Ey, -Hx ]
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


    /// Poynting vector S.
    type S =
        | S of RealVector3

        static member create (E e) (H h) = (ComplexVector3.cross e h.conjugate).re |> S
        member s.norm = let (S a) = s in a.norm
        member s.value = let (S a) = s in a
        member s.x = let (S a) = s in a.x
        member s.y = let (S a) = s in a.y
        member s.z = let (S a) = s in a.z

        member s.normal =
            let norm = s.norm

            if norm > almostZero
            then Some (s.value / norm)
            else None


    let normalizeEH (E e, H h) =
        let norm = [ e.x; h.y; e.y; -h.x ] |> l2Norm |> cplx
        e / norm |> E, h / norm |> H, norm


    /// (E, H) eigenvector.
    type EmEigenVector =
        {
            eigenValue : Complex
            e : E
            h : H
        }

        /// The eigenvectors must be normalized in BerremanFieldEH space: [ Ex, Hy, Ey, -Hx ]
        member emv.rotate r =
            let (e, h, n) = normalizeEH (emv.e.rotate r, emv.h.rotate r)

            let v =
                {
                    eigenValue = emv.eigenValue
                    e = e
                    h = h
                }

            v, n

        member emv.s = S.create emv.e emv.h

        /// For a given EM eigenvector the "old" isotropic n1 * sin(fi) is the x component of Poynting vector.
        /// The eigenvalue carries n1 part and normalized x component of s carries sin(fi) part.
        member emv.n1SinFita = emv.eigenValue.Real * (emv.s.x / emv.s.norm) |> N1SinFita


    /// (E, H) part, which is proportional to a given eigenvector.
    type EmComponent =
        {
            amplitude : Complex
            emEigenVector : EmEigenVector
        }

        member emc.e = emc.amplitude * emc.emEigenVector.e
        member emc.h = emc.amplitude * emc.emEigenVector.h

        member emc.rotate r =
            let (v, n) = emc.emEigenVector.rotate r
            { emc with amplitude = emc.amplitude * n; emEigenVector = v }

        member emc.rotateY a = Rotation.rotateY a |> emc.rotate
        member emc.rotatePiX = emc.rotate Rotation.rotatePiX
        member emc.n1SinFita = emc.emEigenVector.n1SinFita
        static member (*) (a : Complex, b : EmComponent) = { b with amplitude = b.amplitude * a }
        static member (*) (b : EmComponent, a : Complex) = { b with amplitude = b.amplitude * a }

        /// Functions getEz and getHz were generated, do not modify.
        static member create (emv : EigenValueVector) amplitude (nsf : N1SinFita) (o : OpticalProperties) =
            let n1SinFita = nsf.complex
            let bf = emv.vector |> BerremanFieldEH

            let getEz eX eY hX hY =
                ((-(o.eps.[2, 0] * o.mu.[2, 2]) + o.rho.[2, 2] * o.rhoT.[2, 0]) * eX - o.eps.[2, 1] * o.mu.[2, 2] * eY + o.rho.[2, 2] * o.rhoT.[2, 1] * eY - o.rho.[2, 2] * n1SinFita * eY - o.mu.[2, 2] * o.rho.[2, 0] * hX + o.mu.[2, 0] * o.rho.[2, 2] * hX + (o.mu.[2, 1] * o.rho.[2, 2] - o.mu.[2, 2] * (o.rho.[2, 1] + n1SinFita)) * hY)/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])

            let getHz eX eY hX hY =
                 ((-(o.eps.[2, 2] * o.rhoT.[2, 0]) + o.eps.[2, 0] * o.rhoT.[2, 2]) * eX - o.eps.[2, 2] * o.rhoT.[2, 1] * eY + o.eps.[2, 1] * o.rhoT.[2, 2] * eY + o.eps.[2, 2] * n1SinFita * eY - o.eps.[2, 2] * o.mu.[2, 0] * hX + o.rho.[2, 0] * o.rhoT.[2, 2] * hX + (-(o.eps.[2, 2] * o.mu.[2, 1]) + o.rhoT.[2, 2] * (o.rho.[2, 1] + n1SinFita)) * hY)/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])

            let eX = bf.eX
            let eY = bf.eY
            let hX = bf.hX
            let hY = bf.hY
            let eZ = getEz eX eY hX hY
            let hZ = getHz eX eY hX hY

            let emc =
                {
                    amplitude = amplitude

                    emEigenVector =
                        {
                            eigenValue = emv.value
                            e = [ eX; eY; eZ ] |> E.create
                            h = [ hX; hY; hZ ] |> H.create
                        }
                }

            emc


    /// Two component of electromagnetic field E.
    type E2 =
        | E2 of ComplexVector2

        member this.x = let (E2 a) = this in a.x
        member this.y = let (E2 a) = this in a.y
        static member create a = a |> ComplexVector2.create |> E2


    /// Two component of electromagnetic field H.
    and H2 =
        | H2 of ComplexVector2

        member this.x = let (H2 a) = this in a.x
        member this.y = let (H2 a) = this in a.y
        static member create a = a |> ComplexVector2.create |> H2


    type WaveLength =
        | WaveLength of double
        with
        member this.value = let (WaveLength w) = this in w
        static member nm l = l * nm |> WaveLength
        static member mkm l = l * mkm |> WaveLength
        member this.description =
            let (WaveLength w) = this
            if w < mkm then sprintf "wavelength: %A nm" (w / nm)
            else sprintf "wavelength: %A mkm" (w / mkm)


    type Ellipticity =
        | Ellipticity of float

        member this.value = let (Ellipticity w) = this in w
        static member create (e : double) = Ellipticity (max (min e 1.0) -1.0)
        static member (~-) (Ellipticity a) = -a |> Ellipticity
        static member defaultValue = Ellipticity 0.0
        static member minValue = Ellipticity -1.0
        static member maxValue = Ellipticity 1.0
        member this.description = sprintf "ellipticity: %A" this.value


    type Polarization =
        | Polarization of Angle

        member angle.value = let (Polarization a) = angle in a.value
        static member create (Angle p) = ((p + (pi / 2.0)) % pi) - (pi / 2.0) |> Angle |> Polarization
        member this.crossed = Angle (this.value + (pi / 2.0)) |> Polarization
        static member defaultValue = Angle 0.0 |> Polarization
        static member s = Angle 0.0 |> Polarization
        static member p = Polarization.s.crossed
        member this.description = sprintf "polarization: %A degree(s)" (this.value / degree)


    type WedgeAngle =
        | WedgeAngle of Angle

        member angle.value = let (WedgeAngle a) = angle in a.value
        member this.description = sprintf "wedge angle: %A degree(s)" (this.value / degree)
        static member defaultValue = 0.0 |> Angle |> WedgeAngle


    /// We can only construct IncidentLightInfo in isotropic, non-absorbing, etc... media.
    type IncidentLightInfo =
        {
            waveLength : WaveLength
            refractionIndex : RefractionIndex
            incidenceAngle : IncidenceAngle
            polarization : Polarization
            ellipticity : Ellipticity
        }
        member this.getEH (Polarization (Angle beta)) =
            let (RefractionIndex n1) = this.refractionIndex
            let (IncidenceAngle (Angle fita)) = this.incidenceAngle

            let e =
                [
                    cos(beta) * cos(fita) |> cplx
                    sin(beta) |> cplx
                    -cos(beta) * sin(fita) |> cplx
                ]
                |> ComplexVector3.create
                |> E

            let h =
                [
                    -n1 * cos(fita) * sin(beta) |> cplx
                    n1 * cos(beta) |> cplx
                    n1 * sin(beta) * sin(fita) |> cplx
                ]
                |> ComplexVector3.create
                |> H

            (e, h)

        member this.eh0 = this.getEH this.polarization
        member this.eh90 = this.getEH this.polarization.crossed
        member this.ehS = this.getEH Polarization.s
        member this.ehP = this.getEH Polarization.p

        static member create w =
            {
                waveLength = w
                refractionIndex = RefractionIndex.vacuum
                incidenceAngle = IncidenceAngle.normal
                polarization = Polarization.defaultValue
                ellipticity = Ellipticity.defaultValue
            }

        static member createInclined w a =
            {
                waveLength = w
                refractionIndex = RefractionIndex.vacuum
                incidenceAngle = a
                polarization = Polarization.defaultValue
                ellipticity = Ellipticity.defaultValue
            }

        member this.s =
            {
                waveLength = this.waveLength
                refractionIndex = this.refractionIndex
                incidenceAngle = this.incidenceAngle
                polarization = Polarization.s
                ellipticity = Ellipticity.defaultValue
            }

        member this.p =
            {
                waveLength = this.waveLength
                refractionIndex = this.refractionIndex
                incidenceAngle = this.incidenceAngle
                polarization = Polarization.p
                ellipticity = Ellipticity.defaultValue
            }

        member this.rotateY y = { this with incidenceAngle = this.incidenceAngle + y }


    type EmFieldXY =
        {
            waveLength : WaveLength
            opticalProperties : OpticalProperties
            e2 : E2
            h2 : H2
        }


    type EmField =
        {
            waveLength : WaveLength
            opticalProperties : OpticalProperties
            emComponents : List<EmComponent>
        }

        member emf.e = emf.emComponents |> List.fold (fun acc r -> acc + r.e) E.defaultValue
        member emf.h = emf.emComponents |> List.fold (fun acc r -> acc + r.h) H.defaultValue
        member emf.d = emf.opticalProperties.eps * emf.e + emf.opticalProperties.rho * emf.h
        member emf.b = emf.opticalProperties.rhoT * emf.e + emf.opticalProperties.mu * emf.h
        member emf.s = S.create emf.e emf.h
        member emf.normal  = emf.s.normal
        member emf.complexNormal = thread emf.normal (fun n -> [ cplx n.x; cplx n.y; cplx n.z ] |> ComplexVector3.create)

        member emf.rotate r =
            { emf with opticalProperties = emf.opticalProperties.rotate r; emComponents = emf.emComponents |> List.map (fun e -> e.rotate r)  }

        member emf.rotatePiX = emf.rotate Rotation.rotatePiX
        member emf.rotateY y = Rotation.rotateY y |> emf.rotate

        static member getDefaultValue w =
            {
                waveLength = w
                opticalProperties = OpticalProperties.vacuum
//                e = E.defaultValue
//                h = H.defaultValue
//                eh1 = 0
//                eh2 = 0
                emComponents = []
            }

        /// Basis in the system of coordinates where ez is the direction of propagation of incident light,
        /// ey lays in the plane of media boundary and is orthogonal to direction of propagation,
        /// and ex = cross ey ez.
        member emf.complexBasis =
            thread emf.complexNormal (fun cz ->
                let cy =
                    if (cz * ComplexBasis3.defaultValue.cZ).Real >= 0.0
                    then [ cplx 0.0; cplx 1.0; cplx 0.0 ] |> ComplexVector3.create
                    else  [ cplx 0.0; cplx -1.0; cplx 0.0 ] |> ComplexVector3.create

                {
                    cX = ComplexVector3.cross cy cz
                    cY = cy
                    cZ = cz
                })

//        static member create (emXY : EmFieldXY, eZ, hZ) : EmField =
//            {
//                waveLength = emXY.waveLength
////                n1SinFita = emXY.n1SinFita
//                opticalProperties = emXY.opticalProperties
////                e = [ emXY.e2.x; emXY.e2.y; eZ ] |> ComplexVector.create |> ComplexVector3 |> E
////                h = [ emXY.h2.x; emXY.h2.y; hZ ] |> ComplexVector.create |> ComplexVector3 |> H
//            }
//
        static member create (info : IncidentLightInfo, o : OpticalProperties) : EmField =
            let (Ellipticity e) = info.ellipticity
            let a0 = 1.0 / sqrt(1.0 + e * e) |> cplx
            let a90 = e / sqrt(1.0 + e * e) |> cplx
            let (e0, h0, n0) = normalizeEH info.eh0
            let (e90, h90, n90) = normalizeEH info.eh90

//                e = a0 * e0 + cplxI * a90 * e90
//                h = a0 * h0 + cplxI * a90 * h90

            {
                waveLength = info.waveLength
                opticalProperties = o
                emComponents =
                    [
                        {
                            amplitude = a0 * n0
                            emEigenVector =
                                {
                                    eigenValue = cplx info.refractionIndex.value
                                    e = e0
                                    h = h0
                                }
                        }

                        {
                            amplitude = cplxI * a90 * n90
                            emEigenVector =
                                {
                                    eigenValue = cplx info.refractionIndex.value
                                    e = e90
                                    h = h90
                                }
                        }
                    ]
            }

        /// s = x', x' must look in the same direction as x, so that projection of x' on x is positive.
        member emf.amplitudeS =
            let cX =
                match emf.complexBasis with
                | Some b -> b.cX
                | None -> ComplexBasis3.defaultValue.cX // If the value is too small, then we don't care about the direction.

            let (E e) = emf.e
            (cX * e)

        /// p = y' for transmitted but -y' for reflected.
        member emf.amplitudeP =
            let cY =
                match emf.complexBasis with
                | Some b -> b.cY
                | None -> ComplexBasis3.defaultValue.cY // If the value is too small, then we don't care about the direction.

            let (E e) = emf.e
            (cY * e)


    /// Value representation of EmField to be used by tests.
    type EmFieldValue =
        {
            waveLength : WaveLength
            opticalProperties : OpticalProperties
            e : E
            h : H
        }


    type EmFieldSystem =
        {
            incident : EmField
            reflected : EmField
            transmitted : EmField
        }

    /// Value representation of EmFieldSystem to be used by tests.
    type EmFieldSystemValue =
        {
            incidentValue : EmFieldValue
            reflectedValue : EmFieldValue
            transmittedValue : EmFieldValue
        }

    type ReflectedTransmitted =
        {
            reflected : EmField option
            transmitted : EmField option
        }

    type MultipleEmFieldSystem =
        {
            incident : EmField

            /// List of reflected * transmitted
            rt : List<ReflectedTransmitted>
        }


    type StokesVector =
        | StokesVector of RealVector4

        static member create v = v |> RealVector4.create |> StokesVector
        static member (+) (StokesVector a, StokesVector b) = a + b |> StokesVector

        static member Zero
            with get () = StokesVector RealVector4.Zero


    type StokesSystem =
        {
            incidentStokes : StokesVector
            reflectedStokes : StokesVector
            transmittedStokes : StokesVector
        }


    type MuellerMatrix =
        | MuellerMatrix of RealMatrix4x4

        static member create (kSS : Complex) (kSP : Complex) (kPS : Complex) (kPP : Complex) =
            let conjugate (a : Complex) = a.conjugate

            (
                [
                    [
                        (kPP * conjugate(kPP) + kPS * conjugate(kPS) + kSP * conjugate(kSP) + kSS * conjugate(kSS))/(cplx 2.0)
                        (-(kPP * conjugate(kPP)) - kPS * conjugate(kPS) + kSP * conjugate(kSP) + kSS * conjugate(kSS))/(cplx 2.0)
                        (kSP * conjugate(kPP) + kSS * conjugate(kPS) + kPP * conjugate(kSP) + kPS * conjugate(kSS))/(cplx 2.0)
                        (-complexI/(cplx 2.0)) * (kSP * conjugate(kPP) + kSS * conjugate(kPS) - kPP * conjugate(kSP) - kPS * conjugate(kSS))
                    ]
                    [
                        (-(kPP * conjugate(kPP)) + kPS * conjugate(kPS) - kSP * conjugate(kSP) + kSS * conjugate(kSS))/(cplx 2.0)
                        (kPP * conjugate(kPP) - kPS * conjugate(kPS) - kSP * conjugate(kSP) + kSS * conjugate(kSS))/(cplx 2.0)
                        (-(kSP * conjugate(kPP)) + kSS * conjugate(kPS) - kPP * conjugate(kSP) + kPS * conjugate(kSS))/(cplx 2.0)
                        (complexI/(cplx 2.0)) * (kSP * conjugate(kPP) - kSS * conjugate(kPS) - kPP * conjugate(kSP) + kPS * conjugate(kSS))
                    ]
                    [
                        (kPS * conjugate(kPP) + kPP * conjugate(kPS) + kSS * conjugate(kSP) + kSP * conjugate(kSS))/(cplx 2.0)
                        (-(kPS * conjugate(kPP)) - kPP * conjugate(kPS) + kSS * conjugate(kSP) + kSP * conjugate(kSS))/(cplx 2.0)
                        (kSS * conjugate(kPP) + kSP * conjugate(kPS) + kPS * conjugate(kSP) + kPP * conjugate(kSS))/(cplx 2.0)
                        (-complexI/(cplx 2.0)) * (kSS * conjugate(kPP) + kSP * conjugate(kPS) - kPS * conjugate(kSP) - kPP * conjugate(kSS))
                    ]
                    [
                        (complexI/(cplx 2.0)) * (kPS * conjugate(kPP) - kPP * conjugate(kPS) + kSS * conjugate(kSP) - kSP * conjugate(kSS))
                        (-complexI/(cplx 2.0)) * (kPS * conjugate(kPP) - kPP * conjugate(kPS) - kSS * conjugate(kSP) + kSP * conjugate(kSS))
                        (complexI/(cplx 2.0)) * (kSS * conjugate(kPP) - kSP * conjugate(kPS) + kPS * conjugate(kSP) - kPP * conjugate(kSS))
                        (kSS * conjugate(kPP) - kSP * conjugate(kPS) - kPS * conjugate(kSP) + kPP * conjugate(kSS))/(cplx 2.0)
                    ]
                ]

                |> ComplexMatrix4x4.create
            ).re
            |> MuellerMatrix

        static member fromEmFields (s : EmField) (p : EmField) =
            let rSS = s.amplitudeS
            let rSP = s.amplitudeP

            let rPS = p.amplitudeS
            let rPP = p.amplitudeP

            MuellerMatrix.create rSS rSP rPS rPP

        static member (*) (MuellerMatrix (RealMatrix4x4 (RealMatrix a)), StokesVector (RealVector4 (RealVector b))) : StokesVector =
            a * b |> StokesVector.create

        static member (+) (MuellerMatrix (RealMatrix4x4 (RealMatrix a)), MuellerMatrix (RealMatrix4x4 (RealMatrix b))) : MuellerMatrix =
            a + b |> RealMatrix |> RealMatrix4x4 |> MuellerMatrix

        static member Zero
            with get () = MuellerMatrix RealMatrix4x4.Zero
