namespace Analytics

open System.Numerics
open Berreman.Constants
open Berreman.Fields
open Berreman.Solvers
open Berreman.FieldFunctions

/// Pure analysis/derivation functions over the engine's solver outputs (Part F §F.1,
/// §F.3, §F.4, §F.5). Every function is a pure function of a `Solution` (or of the field
/// amplitudes inside it); none re-invokes the 4×4 solver and none re-derives algebra the
/// engine already exposes. Spectral / angular scans are produced by `Variables.calculate`
/// (Variables.fs:237) — this module supplies only the per-`Solution` derivations §4 asks
/// for that the engine does not already carry.
module AnalysisFunctions =

    /// §F.1 — absorption channel A = 1 − R − T for a computed solution. Reuses
    /// `Solution.func` (FieldFunctions.fs:192) for both R and T and NEVER re-invokes the
    /// solver; the s/p split (Rp/Rs/Tp/Ts) is already carried by the distinct
    /// `OpticalFunction` cases, so no new split logic is introduced here.
    let absorptance (s : Solution) : float =
        (s.func R, s.func T)
        |> fun (r, t) -> 1.0 - (defaultArg r 0.0) - (defaultArg t 0.0)

    /// §F.3 — assemble the existing-but-unconstructed `StokesSystem` record
    /// (Fields.fs:590) from the per-`Solution` incident/reflected/transmitted Stokes
    /// vectors already exposed as `Solution.stokesI`/`stokesR`/`stokesT`
    /// (FieldFunctions.fs:222,227,238). No Stokes algebra is re-implemented.
    let stokesSystem (s : Solution) : StokesSystem =
        {
            incidentStokes = s.stokesI
            reflectedStokes = s.stokesR
            transmittedStokes = s.stokesT
        }

    /// §F.4 — degree of polarization √(s1² + s2² + s3²) / s0 from the four components of
    /// a `StokesVector` (Fields.fs:580). Its inputs are `Solution.stokesR`/`stokesT`
    /// (FieldFunctions.fs:227,238). Ellipticity/azimuth are NOT re-derived here — they
    /// are already surfaced by `EmFieldSystem.ellipticityR`/`ellipticityT`/`azimuthR`/
    /// `azimuthT` and the matching `OpticalFunction` cases.
    let degreeOfPolarization (StokesVector v) : float =
        let s0 = v.[0]
        let s1 = v.[1]
        let s2 = v.[2]
        let s3 = v.[3]
        if abs s0 < almostZero then 0.0
        else sqrt (s1 * s1 + s2 * s2 + s3 * s3) / s0

    /// Complex ellipsometric ratio ρ = r_p / r_s read from the reflected field's own
    /// s- and p-resolved amplitudes `EmField.amplitudeS`/`amplitudeP` (Fields.fs:520,530).
    /// This is the single amplitude-extraction path (no fork of the s/p decomposition);
    /// Part G §G.6 consumes this same ρ = r_p/r_s definition over `EmFieldSystem.reflected`.
    let private rho (s : Solution) : Complex =
        let reflected = s.emSys.reflected
        reflected.amplitudeP / reflected.amplitudeS

    /// §F.5 — ellipsometric (Ψ, Δ): tan Ψ = |ρ|, Δ = arg ρ. Pure; does not call the
    /// optimizer and introduces no new field-amplitude extraction path.
    let psiDelta (s : Solution) : float * float =
        let r = rho s
        let psi = atan (Complex.Abs r)
        let delta = r.Phase
        (psi, delta)

    /// §F.5 — (N, C, S) = (cos 2Ψ, sin 2Ψ cos Δ, sin 2Ψ sin Δ) from `psiDelta`. By
    /// construction N² + C² + S² = 1.
    let ncs (s : Solution) : float * float * float =
        let (psi, delta) = psiDelta s
        let n = cos (2.0 * psi)
        let c = sin (2.0 * psi) * cos delta
        let sComp = sin (2.0 * psi) * sin delta
        (n, c, sComp)
