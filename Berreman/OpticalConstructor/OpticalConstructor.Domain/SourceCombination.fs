namespace OpticalConstructor.Domain

open Berreman.Geometry
open Berreman.Fields
open OpticalConstructor.Domain.SourceSpec

/// Multi-source weighted incoherent averaging (§E.8). Each `SourceSpec` expands
/// (§E.4–E.7/E.9) into weighted `IncidentLightInfo` values; the combined
/// illumination is the WEIGHTED INCOHERENT SUM of every expanded sample's solved
/// `StokesVector` output, reusing `StokesVector.(+)`/`StokesVector.Zero`
/// (`Fields.fs:584,586`). Combination is on `StokesVector` OUTPUTS only —
/// field-level (`EmField`) values are NEVER summed across sources (§E.8-risk).
///
/// The reducer is parameterised by a `toStokes : IncidentLightInfo -> StokesVector`
/// function so it stays decoupled from the solver wiring (Part F owns the actual
/// solve); the caller supplies the solve-and-read-Stokes closure. No caching of
/// expanded source lists, no retry, no parallel scheduler (§E.8, constraint 6).
module SourceCombination =

    /// Scale a `StokesVector` by a non-negative scalar weight, component-wise. A
    /// `StokesVector` adds linearly under incoherent superposition, so a per-sample
    /// intensity weight is an honest component-wise scale (§E.8).
    let scaleStokes (weight : float) (stokes : StokesVector) : StokesVector =
        let (StokesVector v) = stokes
        StokesVector.create [ weight * v.[0]; weight * v.[1]; weight * v.[2]; weight * v.[3] ]

    /// Weighted incoherent sum of a list of expanded `(IncidentLightInfo, weight)`
    /// samples (§E.8): solve each to its `StokesVector` via `toStokes`, scale by
    /// its weight, and accumulate through `StokesVector.(+)` from `StokesVector.Zero`.
    let combine
        (toStokes : IncidentLightInfo -> StokesVector)
        (samples : (IncidentLightInfo * float) list) : StokesVector =
        samples
        |> List.fold (fun acc (info, weight) -> acc + scaleStokes weight (toStokes info)) StokesVector.Zero

    /// Combine several sources (§E.8): expand each `SourceSpec` (§E.4–E.7/E.9) and
    /// reduce ALL expanded samples into one weighted incoherent `StokesVector`. The
    /// expansion is recomputed from the immutable sources on demand (no cache).
    let combineSources
        (toStokes : IncidentLightInfo -> StokesVector)
        (sources : SourceSpec list) : StokesVector =
        sources
        |> List.collect SourceSpec.expand
        |> combine toStokes
