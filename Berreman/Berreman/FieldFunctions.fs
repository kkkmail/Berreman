namespace Berreman

module FieldFunctions =

    open MathNetNumericsMath

    open Constants
    open Geometry
    open Fields
    open Solvers

    let toValue errMessage x =
        match x with
        | Some v -> v
        | None -> failwith errMessage


    type ComplexVector3
        with
        /// I.V. Lindell: Methods for Electromagnetic Field Analysis, Chapter 1, Vector p.
        member this.pVector =
            let n = this.norm

            if n < almostZero then RealVector3.zeroVector
            else ((ComplexVector3.cross this this.conjugate) / (createComplex 0.0 (n * n))).re

        /// I.V. Lindell: Methods for Electromagnetic Field Analysis, Chapter 1, Vector q.
        member a.qVector =
            let n = a.norm

            if n < almostZero then RealVector3.zeroVector
            else
                let aa = a * a
                let r = (a / (sqrt aa)).re
                (aa.abs / (n * n)) * (r / r.norm)

        member this.ellipticity : Ellipticity =
            let v = this.pVector.norm
            if v < almostZero then Ellipticity.defaultValue
            else (1.0 - sqrt(1.0 - v * v)) / v |> Ellipticity


    type EmField
        with
        member em.stokesVector =
            let stokes (b : ComplexBasis3) =
                let (E e) = em.e
                let ex = e * b.cX
                let ey = e * b.cY
                let s0 = (ex * ex.conjugate + ey * ey.conjugate).Real
                let s1 = (ex * ex.conjugate - ey * ey.conjugate).Real
                let s2 = (ex * ey.conjugate + ey * ex.conjugate).Real
                let s3 = (complexI * (ex * ey.conjugate - ey * ex.conjugate)).Real
                [ s0; s1; s2; s3 ] |> StokesVector.create

            thread em.complexBasis stokes

        member em.intensityX (i : EmField) =
            let (E e) = em.e
            let (H h) = em.h
            let (S is) = i.s
            (ComplexVector3.cross (ComplexBasis3.defaultValue.toX e) h.conjugate).re.norm / is.z

        member em.intensityY (i : EmField) =
            let (E e) = em.e
            let (H h) = em.h
            let (S is) = i.s
            (ComplexVector3.cross (ComplexBasis3.defaultValue.toY e) h.conjugate).re.norm / is.z

        member em.intensity (i : EmField) =
            //(em.intensityX i) + (em.intensityY i)
            let (S s) = em.s
            let (S is) = i.s
            (s.z |> abs) / is.z

        member em.ellipticity : Ellipticity =
            let (E e) = em.e
            let (S s) = em.s
            let p = e.pVector

            if p * s >= 0.0 then -e.ellipticity
            else e.ellipticity

        member em.azimuth : Polarization =
            let (E e) = em.e
            let q = e.qVector

            let n = q.norm
            if n < almostZero then Polarization.defaultValue
            else
                let v = (q / q.norm) * RealBasis3.defaultValue.vY
                let a = asin v
                a |> Angle |> Polarization

        member em.muellerMatrix : MuellerMatrix =
            failwith ""


    type FunctionDescription =
        {
            name : string
            subscript : string option
            description : string option
        }
        member this.fullName =
            match this.subscript with
            | Some s -> this.name + s
            | None -> this.name


    type OpticalFunction =
        | I
        | Ip
        | Is
        | R
        | Rp
        | Rs
        | T
        | Tp
        | Ts
        | EllipticityR
        | EllipticityT
        | AzimuthR
        | AzimuthT


        member this.info =
            match this with
            | I -> { name = "I"; subscript = None; description = None }
            | Ip -> { name = "I"; subscript = None; description = None }
            | Is -> { name = "I"; subscript = None; description = None }
            | R -> { name = "R"; subscript = None; description = None }
            | Rp -> { name = "R"; subscript = Some "p"; description = None }
            | Rs -> { name = "R"; subscript = Some "s"; description = None }
            | T -> { name = "T"; subscript = None; description = None }
            | Tp -> { name = "T"; subscript = Some "p"; description = None }
            | Ts -> { name = "T"; subscript = Some "s"; description = None }
            | EllipticityR -> { name = "elT"; subscript = None; description = None }
            | EllipticityT -> { name = "elR"; subscript = None; description = None }
            | AzimuthR -> { name = "pR"; subscript = None; description = None }
            | AzimuthT -> { name = "pT"; subscript = None; description = None }


    type EmFieldSystem
        with
        member this.i = this.incident.intensity this.incident
        member this.ip = this.incident.intensityX this.incident
        member this.is = this.incident.intensityY this.incident


        member this.r = this.reflected.intensity this.incident
        member this.rp = this.reflected.intensityX this.incident
        member this.rs = this.reflected.intensityY this.incident

        member this.t = this.transmitted.intensity this.incident
        member this.tp = this.transmitted.intensityX this.incident
        member this.ts = this.transmitted.intensityY this.incident

        member this.ellipticityR =
            let (Ellipticity e) = this.reflected.ellipticity
            e

        member this.ellipticityT =
            let (Ellipticity e) = this.transmitted.ellipticity
            e

        member this.azimuthR =
            let (Polarization (Angle a)) = this.reflected.azimuth
            a

        member this.azimuthT =
            let (Polarization (Angle a)) = this.transmitted.azimuth
            a

        member this.func f =
            match f with
            | I -> this.i
            | Ip -> this.ip
            | Is -> this.is
            | R -> this.r
            | Rp -> this.rp
            | Rs -> this.rs
            | T -> this.t
            | Tp -> this.tp
            | Ts -> this.ts
            | EllipticityR -> this.ellipticityR
            | EllipticityT -> this.ellipticityT
            | AzimuthR -> this.azimuthR
            | AzimuthT -> this.azimuthT


    type Solution
        with
        member this.func f =
            match this with
            | Single b -> b.emSys.func f |> Some
            | Multiple m ->
                let r () = m.rt |> List.choose (fun e -> e.reflected)
                let t () = m.rt |> List.choose (fun e -> e.transmitted)
                let fn g l = l |> List.fold (fun acc e -> acc + g e m.incident) 0.0 |> Some

                match f with
                | I -> m.incident.intensity m.incident |> Some
                | Ip -> m.incident.intensityX m.incident |> Some
                | Is -> m.incident.intensityY m.incident |> Some
                | R -> r() |> fn (fun e -> e.intensity)
                | Rp -> r() |> fn (fun e -> e.intensityX)
                | Rs -> r() |> fn (fun e -> e.intensityY)
                | T -> t() |> fn (fun e -> e.intensity)
                | Tp -> t() |> fn (fun e -> e.intensityX)
                | Ts -> t() |> fn (fun e -> e.intensityY)

                // These ones must be calculated via sum of Stokes vectors.
                | EllipticityR -> None
                | EllipticityT -> None
                | AzimuthR -> None
                | AzimuthT -> None


        member this.stokesI : StokesVector =
            match this with
            | Single b -> b.emSys.incident.stokesVector |> Option.defaultValue StokesVector.Zero
            | Multiple m -> m.incident.stokesVector |> Option.defaultValue StokesVector.Zero

        member this.stokesR : StokesVector =
            match this with
            | Single b -> b.emSys.reflected.stokesVector |> Option.defaultValue StokesVector.Zero
            | Multiple m ->
                m.rt
                |> List.map (fun e -> e.reflected)
                |> List.choose id
                |> List.map (fun e -> e.stokesVector)
                |> List.choose id
                |> List.sum

        member this.stokesT : StokesVector =
            match this with
            | Single b -> b.emSys.transmitted.stokesVector |> Option.defaultValue StokesVector.Zero
            | Multiple m ->
                m.rt
                |> List.map (fun e -> e.transmitted)
                |> List.choose id
                |> List.map (fun e -> e.stokesVector)
                |> List.choose id
                |> List.sum

