namespace Berreman

open System.Numerics
open MathNetNumericsMath
open Constants
open Geometry
open Fields
open Solvers

module FieldFunctions =

    /// The value of an OpticalFunction for one field: a relative intensity, an ellipticity or an azimuth (radians).
    type OpticalFunctionValue =
        | OpticalFunctionValue of float

        member this.value = let (OpticalFunctionValue v) = this in v
        static member zero = OpticalFunctionValue 0.0
        static member (+) (OpticalFunctionValue a, OpticalFunctionValue b) = OpticalFunctionValue (a + b)


    type ComplexVector3
        with
        /// I.V. Lindell: Methods for Electromagnetic Field Analysis, Chapter 1, Vector p.
        member this.pVector =
            let n = this.norm

            if n < almostZero.value then RealVector3.zeroVector
            else ((ComplexVector3.cross this this.conjugate) / (Complex.create 0.0 (n * n))).re

        /// I.V. Lindell: Methods for Electromagnetic Field Analysis, Chapter 1, Vector q.
        member a.qVector =
            let n = a.norm

            if n < almostZero.value then RealVector3.zeroVector
            else
                let aa = a * a
                let r = (a / (sqrt aa)).re
                let (RealVector3 (RealVector rv)) = r
                (aa.abs / (n * n)) * (rv / r.norm) |> RealVector |> RealVector3

        member this.ellipticity : Ellipticity =
            let v = this.pVector.norm
            if v < almostZero.value then Ellipticity.defaultValue
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

        member em.intensityX (i : EmField) : OpticalFunctionValue =
            let (E e) = em.e
            let (H h) = em.h
            let (S is) = i.s
            ((ComplexVector3.cross (ComplexBasis3.defaultValue.toX e) h.conjugate).re.z |> abs) / is.z |> OpticalFunctionValue

        member em.intensityY (i : EmField) : OpticalFunctionValue =
            let (E e) = em.e
            let (H h) = em.h
            let (S is) = i.s
            ((ComplexVector3.cross (ComplexBasis3.defaultValue.toY e) h.conjugate).re.z |> abs) / is.z |> OpticalFunctionValue

        member em.intensity (i : EmField) : OpticalFunctionValue =
            let (S s) = em.s
            let (S is) = i.s
            (s.z |> abs) / is.z |> OpticalFunctionValue

        member em.ellipticity : Ellipticity =
            let (E e) = em.e
            let (S (RealVector3 (RealVector s))) = em.s
            let (RealVector3 (RealVector p)) = e.pVector

            if p * s >= 0.0 then -e.ellipticity
            else e.ellipticity

        member em.azimuth : Polarization =
            let (E e) = em.e
            let q = e.qVector

            let n = q.norm
            if n < almostZero.value then Polarization.defaultValue
            else
                let (RealVector3 (RealVector qv)) = q
                let (RealVector3 (RealVector vY)) = RealBasis3.defaultValue.vY
                let v = (qv / q.norm) * vY
                let a = asin v
                a |> Angle |> Polarization

        /// Per-field Mueller matrix (F.3). Pure delegation to the existing
        /// `MuellerMatrix.fromEmFields` constructor (Fields.fs:636), supplying the
        /// field's own s- and p-resolved components (`amplitudeS`/`amplitudeP`).
        /// The 4×4 Mueller algebra already lives in `MuellerMatrix.create`
        /// (Fields.fs:601) and MUST NOT be re-derived here.
        member em.muellerMatrix : MuellerMatrix =
            MuellerMatrix.fromEmFields em em


    /// A part of an optical function's printed symbol: its letter(s) or its subscript.
    type FunctionSymbol =
        | FunctionSymbol of string

        member this.value = let (FunctionSymbol v) = this in v


    type FunctionDescription =
        {
            name : FunctionSymbol
            subscript : FunctionSymbol option
        }
        member this.fullName : string =
            let (FunctionSymbol name) = this.name

            match this.subscript with
            | Some (FunctionSymbol s) -> name + s
            | None -> name


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
            | I -> { name = FunctionSymbol "I"; subscript = None }
            | Ip -> { name = FunctionSymbol "I"; subscript = Some (FunctionSymbol "p") }
            | Is -> { name = FunctionSymbol "I"; subscript = Some (FunctionSymbol "s") }
            | R -> { name = FunctionSymbol "R"; subscript = None }
            | Rp -> { name = FunctionSymbol "R"; subscript = Some (FunctionSymbol "p") }
            | Rs -> { name = FunctionSymbol "R"; subscript = Some (FunctionSymbol "s") }
            | T -> { name = FunctionSymbol "T"; subscript = None }
            | Tp -> { name = FunctionSymbol "T"; subscript = Some (FunctionSymbol "p") }
            | Ts -> { name = FunctionSymbol "T"; subscript = Some (FunctionSymbol "s") }
            | EllipticityR -> { name = FunctionSymbol "elT"; subscript = None }
            | EllipticityT -> { name = FunctionSymbol "elR"; subscript = None }
            | AzimuthR -> { name = FunctionSymbol "pR"; subscript = None }
            | AzimuthT -> { name = FunctionSymbol "pT"; subscript = None }


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
            OpticalFunctionValue e

        member this.ellipticityT =
            let (Ellipticity e) = this.transmitted.ellipticity
            OpticalFunctionValue e

        member this.azimuthR =
            let (Polarization (Angle a)) = this.reflected.azimuth
            OpticalFunctionValue a

        member this.azimuthT =
            let (Polarization (Angle a)) = this.transmitted.azimuth
            OpticalFunctionValue a

        member this.func (f : OpticalFunction) : OpticalFunctionValue =
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
        member this.func (f : OpticalFunction) : OpticalFunctionValue option =
            match this with
            | Single b -> b.emSys.func f |> Some
            | Multiple m ->
                let r() = m.rt |> List.choose (fun e -> e.reflected)
                let t() = m.rt |> List.choose (fun e -> e.transmitted)
                let fn g l = l |> List.fold (fun acc e -> acc + g e m.incident) OpticalFunctionValue.zero |> Some

                let fnMax f g l =
                        l
                        |> List.sortBy (fun e -> let (OpticalFunctionValue v) = f e m.incident in -v)
                        |> List.tryHead
                        |> Option.bind (fun e -> g e |> Some)

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
                | EllipticityR -> r() |> fnMax (fun e -> e.intensity) (fun e -> let (Ellipticity v) = e.ellipticity in OpticalFunctionValue v)
                | EllipticityT -> t() |> fnMax (fun e -> e.intensity) (fun e -> let (Ellipticity v) = e.ellipticity in OpticalFunctionValue v)
                | AzimuthR -> r() |> fnMax (fun e -> e.intensity) (fun e -> let (Polarization (Angle v)) = e.azimuth in OpticalFunctionValue v)
                | AzimuthT -> t() |> fnMax (fun e -> e.intensity) (fun e -> let (Polarization (Angle v)) = e.azimuth in OpticalFunctionValue v)


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
