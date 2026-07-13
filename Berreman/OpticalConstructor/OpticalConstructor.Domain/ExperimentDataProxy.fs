namespace OpticalConstructor.Domain

open OpticalConstructor.Domain.Experiments            // DataFilePath (step 025)
open OpticalConstructor.Domain.MeasuredData           // IntensitySeries / EllipsometricSeries / ExperimentDataError (step 034)

/// Spec 0038 (035, ADD_CONTRACT STORE_XDUO_0006) — the measured-data LOAD seam. Declares, in the
/// Domain, the DECLARED-lifecycle `ExperimentDataProxy`: the IO boundary that turns an experiment's
/// elevated `DataFilePath` (step 025) into a parsed measured-data series (step 034), or a typed
/// `ExperimentDataError`. It is the disk-read EDGE the pure `MeasuredData` parsers were written to
/// sit behind — `MeasuredData.parseIntensitySeries` / `parseEllipsometricSeries` take the file TEXT
/// and never touch the filesystem or throw, so a future real proxy is exactly the missing
/// `path -> read text -> parse` adapter and its exceptions are caught AT this boundary and mapped to
/// the typed `ExperimentDataError` channel.
///
/// The seam is kept as pure DATA (the `Library.LibraryProxy` / `Scene.SceneProxy` convention): a
/// record of camelCase `Result`-returning functions, so logic that holds the proxy stays
/// referentially transparent and a test substitutes a canned in-memory stub of the SAME shape.
/// Function-valued fields have no structural equality, so the record is `[<ReferenceEquality>]` —
/// a host model (Elmish) that holds one keeps its required equality, comparing the proxy by identity.
///
/// DECLARED lifecycle: this is the seam ONLY — no filesystem read, no path resolution, no wiring. A
/// later `IMPLEMENT_CONTRACT STORE_XDUO_0006` cycle supplies the real file-backed `create` in
/// `OpticalConstructor.Storage` (read the file's text, hand it to the `MeasuredData` parser, map any
/// IO exception to a typed error), leaving every consumer that holds the proxy unchanged. The mock
/// that satisfies this surface (canned series keyed by path) lives with its test in
/// `OpticalConstructor.Tests`, mirroring the `SceneProxyTests` mock.
///
/// Compiles LAST in the Domain: it names both `Experiments` (`DataFilePath`) and `MeasuredData` (the
/// series + error types), so it follows `MeasuredData.fs`; nothing in the Domain depends on it.
module ExperimentData =

    /// The measured-data load seam (the functional-proxy convention): a record of camelCase
    /// `Result`-returning functions that resolve an experiment's `DataFilePath` to its parsed series.
    ///
    /// - `tryLoadIntensity path` — read + parse the intensity data file at `path` into an
    ///   `IntensitySeries`, or a typed `ExperimentDataError` (a malformed / empty file, or — for a
    ///   later real store — a missing file / IO failure mapped onto the channel);
    /// - `tryLoadEllipsometric path` — the same for an ellipsometric `wavelength_nm,psi_deg,delta_deg`
    ///   (optional `aoi_deg`) file into an `EllipsometricSeries`.
    ///
    /// DECLARED lifecycle: the seam only — the real disk-backed `create` lands in a later
    /// `IMPLEMENT_CONTRACT STORE_XDUO_0006` in `OpticalConstructor.Storage`.
    [<ReferenceEquality>]
    type ExperimentDataProxy =
        {
            tryLoadIntensity : DataFilePath -> Result<IntensitySeries, ExperimentDataError>
            tryLoadEllipsometric : DataFilePath -> Result<EllipsometricSeries, ExperimentDataError>
        }
