﻿namespace Berreman

module Fields =

    //open ExtremeNumericsMath

    open System.Numerics
    open MathNetNumericsMath

    open Geometry
    open MaterialProperties
    open Constants

    // CGS units are used.
    // TODO kk:20180922 - Get rid of boiler plate code below.


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

        static member create a = a |> ComplexVector3.create |> E
        static member fromRe a = a |> ComplexVector3.fromRe |> E

        member this.x =
            let (E a) = this
            a.x

        member this.y =
            let (E a) = this
            a.y

        member this.z =
            let (E a) = this
            a.z


    /// Electromagnetic field H.
    and H =
        | H of ComplexVector3

        static member (*) (Rho (ComplexMatrix3x3 a), H (ComplexVector3 b)) = a * b |> ComplexVector3 |> D
        static member (*) (Mu (ComplexMatrix3x3 a), H (ComplexVector3 b)) = a * b |> ComplexVector3 |> B

        static member (+) (H (ComplexVector3 a), H (ComplexVector3 b)) : H = a + b |> ComplexVector3 |> H

        static member (*) (a : Complex, H (ComplexVector3 b)) = a * b |> ComplexVector3 |> H
        static member (*) (H (ComplexVector3 a), b : Complex) = a * b |> ComplexVector3 |> H

        static member create a = a |> ComplexVector3.create |> H
        static member fromRe a = a |> ComplexVector3.fromRe |> H

        member this.x =
            let (H a) = this
            a.x

        member this.y =
            let (H a) = this
            a.y

        member this.z =
            let (H a) = this
            a.z


    /// Electromagnetic field D.
    and D =
        | D of ComplexVector3

        static member (+) (D (ComplexVector3 a), D (ComplexVector3 b)) : D = a + b |> ComplexVector3 |> D

        static member (*) (a : Complex, D (ComplexVector3 b)) = a * b |> ComplexVector3 |> D
        static member (*) (D (ComplexVector3 a), b : Complex) = a * b |> ComplexVector3 |> D


        member this.x =
            let (D a) = this
            a.x

        member this.y =
            let (D a) = this
            a.y

        member this.z =
            let (D a) = this
            a.z


    /// Electromagnetic field B.
    and B =
        | B of ComplexVector3

        static member (+) (B (ComplexVector3 a), B (ComplexVector3 b)) : B = a + b |> ComplexVector3 |> B

        static member (*) (a : Complex, B (ComplexVector3 b)) = a * b |> ComplexVector3 |> B
        static member (*) (B (ComplexVector3 a), b : Complex) = a * b |> ComplexVector3 |> B

        member this.x =
            let (B a) = this
            a.x

        member this.y =
            let (B a) = this
            a.y

        member this.z =
            let (B a) = this
            a.z


    /// Poynting vector S.
    type S =
        | S of RealVector3


    /// Two component of electromagnetic field E.
    type E2 =
        | E2 of ComplexVector2

        member this.x =
            let (E2 a) = this
            a.x

        member this.y =
            let (E2 a) = this
            a.y

        static member create a = a |> ComplexVector2.create |> E2


    /// Two component of electromagnetic field H.
    and H2 =
        | H2 of ComplexVector2

        member this.x =
            let (H2 a) = this
            a.x

        member this.y =
            let (H2 a) = this
            a.y

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
        static member create (e : double) =
            Ellipticity (max (min e 1.0) -1.0)

        static member (~-) (Ellipticity a) = -a |> Ellipticity
        static member defaultValue = Ellipticity 0.0
        static member minValue = Ellipticity -1.0
        static member maxValue = Ellipticity 1.0
        member this.description =
            let (Ellipticity e) = this
            sprintf "ellipticity: %A" e


    type Polarization =
        | Polarization of Angle

        static member create (Angle p) =
            p % (pi / 2.0) |> Angle |> Polarization

        member this.crossed =
            let (Polarization (Angle p)) = this
            Angle (p + (pi / 2.0)) |> Polarization

        static member defaultValue = Angle 0.0 |> Polarization

        // TODO Check angle
        static member s = Angle 0.0 |> Polarization
        static member p = Polarization.s.crossed
        member this.description =
            let (Polarization (Angle a)) = this
            sprintf "polarization: %A degree(s)" (a / degree)


    type IncidenceAngle =
        | IncidenceAngle of Angle
        static member create (Angle p) =
            (p % (pi / 2.0) + pi) % (pi / 2.0) |> Angle |> IncidenceAngle

        member angle.value = let (IncidenceAngle a) = angle in a.value
        static member normal = IncidenceAngle.create (Angle.degree 0.0)
        static member maxValue = IncidenceAngle.create (Angle.degree 89.0)
        member this.description =
            let (IncidenceAngle (Angle a)) = this
            sprintf "incidence angle: %A degree(s)" (a / degree)

        static member (+) (IncidenceAngle a, Angle b) = a.value + b |> Angle |> IncidenceAngle
        static member (-) (IncidenceAngle a, Angle b) = a.value - b |> Angle |> IncidenceAngle


    /// n1 * sin(fita), where fita is the incidence angle and n1 is the refraction index of upper media.
    /// This is an invariant and it deserves a type.
    type N1SinFita =
        {
            n1 : RefractionIndex
            fita : IncidenceAngle
        }

        static member create n f =
            {
                n1 = n
                fita = f
            }

        static member normal = N1SinFita.create RefractionIndex.vacuum IncidenceAngle.normal
        member this.value = this.n1.value * (sin this.fita.value)
        member this.complex = this.value |> cplx
        member this.description = this.ToString()
        member this.rotateY y = { this with fita = this.fita + y }


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
        member this.n1SinFita = N1SinFita.create this.refractionIndex this.incidenceAngle

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
            n1SinFita : N1SinFita
            opticalProperties : OpticalProperties
            e2 : E2
            h2 : H2
        }


    type EmField =
        {
            waveLength : WaveLength
            n1SinFita : N1SinFita
            opticalProperties : OpticalProperties
            e : E
            h : H
        }
        member emf.d = emf.opticalProperties.eps * emf.e + emf.opticalProperties.rho * emf.h
        member emf.b = emf.opticalProperties.rhoT * emf.e + emf.opticalProperties.mu * emf.h

        /// Poynting vector
        member emf.s =
            let (E e) = emf.e
            let (H h) = emf.h
            (ComplexVector3.cross e h.conjugate).re |> S

        member emf.normal : RealVector3 option =
            let (S s) = emf.s
            let norm = s.norm

            if norm > almostZero
            then Some (s / norm)
            else None

        member emf.complexNormal = thread emf.normal (fun n -> [ cplx n.x; cplx n.y; cplx n.z ] |> ComplexVector3.create)

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

        static member create (emXY : EmFieldXY, eZ, hZ) : EmField =
            {
                waveLength = emXY.waveLength
                n1SinFita = emXY.n1SinFita
                opticalProperties = emXY.opticalProperties
                e = [ emXY.e2.x; emXY.e2.y; eZ ] |> ComplexVector.create |> ComplexVector3 |> E
                h = [ emXY.h2.x; emXY.h2.y; hZ ] |> ComplexVector.create |> ComplexVector3 |> H
            }

        static member create (info : IncidentLightInfo, o : OpticalProperties) : EmField =
            let (Ellipticity e) = info.ellipticity
            let a0 = 1.0 / sqrt(1.0 + e * e) |> cplx
            let a90 = e / sqrt(1.0 + e * e) |> cplx
            let (e0, h0) = info.eh0
            let (e90, h90) = info.eh90

            {
                waveLength = info.waveLength
                n1SinFita = info.n1SinFita
                opticalProperties = o
                e = a0 * e0 + cplxI * a90 * e90
                h = a0 * h0 + cplxI * a90 * h90
            }

        member private emf.rotate (Rotation r) : EmField =
            let c = r.toComplex()
            let cInv = c.inverse

            let (E e) = emf.e
            let (H h) = emf.h
            { emf with e = cInv * e |> E; h = cInv * h |> H; opticalProperties = emf.opticalProperties.rotate (Rotation r) }

        member emf.rotatePiX = emf.rotate Rotation.rotatePiX
        member emf.rotateY y = Rotation.rotateY y |> emf.rotate

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


    type EmFieldSystem =
        {
            incident : EmField
            reflected : EmField
            transmitted : EmField
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
