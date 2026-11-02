namespace Analytics

open System.Numerics
open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.Solvers
open Berreman.FieldFunctions

/// Pure analysis/derivation functions over the engine's solver outputs (Part F §F.1,
/// §F.3, §F.4, §F.5). Every function is a pure function of a `Solution` (or of the field
/// amplitudes inside it); none re-invokes the 4×4 solver and none re-derives algebra the
/// engine already exposes. Spectral / angular scans are produced by `Variables.calculate`
/// — this module supplies only the per-`Solution` derivations §4 asks for that the engine
/// does not already carry.
module AnalysisFunctions =

    /// The fraction of the incident power a system or a layer absorbs (A = 1 − R − T for a system).
    type AbsorptanceValue =
        | AbsorptanceValue of float

        member this.value = let (AbsorptanceValue a) = this in a


    /// The degree of polarization √(s1² + s2² + s3²) / s0 of a Stokes vector, 0 when it carries no intensity.
    type DegreeOfPolarization =
        | DegreeOfPolarization of float

        member this.value = let (DegreeOfPolarization p) = this in p


    // The constructor's ellipsometer reads this same type: one Ψ/Δ entity, kept in the lower assembly (spec 0048 E005).
    /// The ellipsometric angles Ψ and Δ, tan Ψ · e^{iΔ} = ρ = r_p / r_s.
    type PsiDelta =
        {
            psi : Angle
            delta : Angle
        }


    /// One of the ellipsometric parameters N, C, S, each in [-1, 1].
    type NcsValue =
        | NcsValue of float

        member this.value = let (NcsValue v) = this in v


    /// (N, C, S) = (cos 2Ψ, sin 2Ψ cos Δ, sin 2Ψ sin Δ); N² + C² + S² = 1 by construction.
    type NcsCoordinates =
        {
            n : NcsValue
            c : NcsValue
            s : NcsValue
        }


    type StokesVector with

        /// §F.4 — degree of polarization from the four Stokes components. Ellipticity and azimuth are
        /// NOT re-derived here: `EmFieldSystem` and the `OpticalFunction` cases already carry them.
        member this.degreeOfPolarization : DegreeOfPolarization =
            let (StokesVector v) = this
            let s0 = v.[0]
            let s1 = v.[1]
            let s2 = v.[2]
            let s3 = v.[3]
            if abs s0 < almostZero.value then DegreeOfPolarization 0.0
            else sqrt (s1 * s1 + s2 * s2 + s3 * s3) / s0 |> DegreeOfPolarization


    type Solution with

        /// §F.1 — A = 1 − R − T, read through `Solution.func` for both R and T; it NEVER re-invokes
        /// the solver, and the s/p split is already carried by the distinct `OpticalFunction` cases.
        member this.absorptance : AbsorptanceValue =
            let r = this.func R |> Option.defaultValue OpticalFunctionValue.zero
            let t = this.func T |> Option.defaultValue OpticalFunctionValue.zero
            AbsorptanceValue (1.0 - r.value - t.value)

        /// §F.3 — the incident / reflected / transmitted Stokes vectors the solution already exposes,
        /// assembled; no Stokes algebra is re-implemented.
        member this.stokesSystem : StokesSystem =
            {
                incidentStokes = this.stokesI
                reflectedStokes = this.stokesR
                transmittedStokes = this.stokesT
            }

        /// §F.5 — tan Ψ = |ρ|, Δ = arg ρ, with ρ = r_p / r_s read off the reflected field's own s- and
        /// p-resolved amplitudes: the single amplitude-extraction path, which Part G §G.6 shares.
        member this.psiDelta : PsiDelta =
            let reflected = this.emSys.reflected
            let rho = reflected.amplitudeP / reflected.amplitudeS
            { psi = Angle (atan (Complex.Abs rho)); delta = Angle rho.Phase }

        /// §F.5 — (N, C, S) from `psiDelta`.
        member this.ncs : NcsCoordinates =
            let pd = this.psiDelta
            let psi = pd.psi.value
            let delta = pd.delta.value
            {
                n = NcsValue (cos (2.0 * psi))
                c = NcsValue (sin (2.0 * psi) * cos delta)
                s = NcsValue (sin (2.0 * psi) * sin delta)
            }
