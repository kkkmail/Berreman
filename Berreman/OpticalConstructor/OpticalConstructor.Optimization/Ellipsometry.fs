namespace OpticalConstructor.Optimization

open Berreman.Solvers
open Analytics

/// §G.6 — Ψ, Δ derivation for ellipsometric residuals (spec 0022 Part G §G.6, [Standard]).
///
/// The complex reflected `EmComponent` amplitudes are already solved by `getRT`
/// (`Solvers.fs:74`) and exposed on `EmFieldSystem.reflected` (`Fields.fs:550`).
/// The ellipsometric ratio ρ = r_p / r_s yields tan(Ψ)·e^{iΔ}, so Ψ = atan(|ρ|)
/// and Δ = arg(ρ). This module does NOT fork `Solvers.fs` or `FieldFunctions.fs`
/// (§0 constraint 2) and does NOT re-derive Ψ/Δ: it SHARES the Part F §F.5
/// definition by calling `Analytics.AnalysisFunctions.psiDelta` (slice 008),
/// which reads that same ρ = r_p/r_s off `EmFieldSystem.reflected`.
///
/// `FieldFunctions.fs:109` `OpticalFunction` is a closed DU and §0 constraint 2
/// forbids forking it, so Ψ/Δ are added here as a small net-new
/// `EllipsometricFunction` DU with its own evaluator — NOT by editing the
/// engine's `OpticalFunction`. The Mueller path (`FieldFunctions.fs:93`) is owned
/// by Part F §F.3 (slice 008) and is NOT revived here.
module Ellipsometry =

    /// The two net-new ellipsometric quantities reachable by a `FitTarget` (§G.4),
    /// so ellipsometric targets compose identically to the photometric
    /// `OpticalFunction` cases.
    type EllipsometricFunction =
        | Psi
        | Delta

        /// Evaluate the quantity on a solved `Solution`, sharing the §F.5
        /// (Ψ, Δ) = (atan|ρ|, arg ρ) derivation rather than re-deriving it.
        member this.evaluate (solution : Solution) : float =
            let (psi, delta) = AnalysisFunctions.psiDelta solution
            match this with
            | Psi -> psi
            | Delta -> delta
